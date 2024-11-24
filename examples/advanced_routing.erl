-module(advanced_routing).
-export([start/0]).
-export([
    % Route handlers
    get_users/1,
    create_user/1,
    get_user_posts/1,
    create_user_post/1,
    update_user/1,
    delete_user/1,
    api_status/1,

    % Middleware
    json_middleware/1,
    rate_limit_middleware/1,
    cors_middleware/1,
    validate_user_middleware/1,
    logger_middleware/1
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

start() ->
    setup_yaws(),
    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(),

    % API status endpoint
    yaws_appmod_router:add_route("GET", "/api/status", fun advanced_routing:api_status/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:logger_middleware/1
    ]),

    % User routes with different HTTP methods
    yaws_appmod_router:add_route("GET", "/api/users", fun advanced_routing:get_users/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:rate_limit_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    yaws_appmod_router:add_route("POST", "/api/users", fun advanced_routing:create_user/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:rate_limit_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    % Single user operations with validation
    yaws_appmod_router:add_route("GET", "/api/users/:id", fun advanced_routing:get_user_posts/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:validate_user_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    yaws_appmod_router:add_route("PUT", "/api/users/:id", fun advanced_routing:update_user/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:validate_user_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    yaws_appmod_router:add_route("DELETE", "/api/users/:id", fun advanced_routing:delete_user/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:validate_user_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    % Nested routes for user posts
    yaws_appmod_router:add_route("GET", "/api/users/:user_id/posts", fun advanced_routing:get_user_posts/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:validate_user_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    yaws_appmod_router:add_route("POST", "/api/users/:user_id/posts", fun advanced_routing:create_user_post/1, [
        fun advanced_routing:cors_middleware/1,
        fun advanced_routing:validate_user_middleware/1,
        fun advanced_routing:json_middleware/1
    ]),

    setup_server().

%% Server setup
setup_yaws() ->
    application:load(yaws),
    application:set_env(yaws, embedded, true).

setup_server() ->
    Id = "embedded_yaws",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    SconfList = [
        {port, 8080},
        {servername, "advanced_server"},
        {listen, {127, 0, 0, 1}},
        {docroot, Docroot},
        {opaque, #{}},
        {appmods, [{"/", yaws_appmod_router}]}
    ],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    [supervisor:start_child(yaws_sup, Ch) || Ch <- ChildSpecs],
    ok = yaws_api:setconf(GC, SCList),
    io:format("Advanced routing server started on port 8080~n"),
    ok.

%% Middleware implementations
cors_middleware(Arg) ->
    Headers = [
        {header, {access_control_allow_origin, "*"}},
        {header, {access_control_allow_methods, "GET, POST, PUT, DELETE, OPTIONS"}},
        {header, {access_control_allow_headers, "Content-Type"}}
    ],
    {ok, Arg#arg{opaque = maps:put(cors_headers, Headers, Arg#arg.opaque)}}.

json_middleware(Arg) ->
    Headers = [{header, {content_type, "application/json"}}],
    {ok, Arg#arg{opaque = maps:put(json_headers, Headers, Arg#arg.opaque)}}.

rate_limit_middleware(Arg) ->
    % Simple rate limiting example - could be enhanced with proper storage
    case erlang:get(request_count) of
        undefined ->
            erlang:put(request_count, 1),
            {ok, Arg};
        Count when Count > 100 ->
            {error, [{status, 429}, {content, "application/json", 
                "{\"error\": \"Too many requests. Please try again later.\"}"
            }]};
        Count ->
            erlang:put(request_count, Count + 1),
            {ok, Arg}
    end.

validate_user_middleware(Arg) ->
    Params = Arg#arg.appmoddata,
    case maps:get(id, Params, undefined) of
        undefined ->
            {error, [{status, 400}, {content, "application/json",
                "{\"error\": \"User ID is required\"}"
            }]};
        _Id ->
            {ok, Arg}
    end.

logger_middleware(Arg) ->
    Method = (Arg#arg.req)#http_request.method,
    Path = Arg#arg.server_path,
    io:format("[~p] ~p ~p~n", [calendar:local_time(), Method, Path]),
    {ok, Arg}.

%% Route handlers
api_status(_Arg) ->
    Response = #{
        status => <<"ok">>,
        version => <<"1.0.0">>,
        timestamp => calendar:local_time()
    },
    {content, "application/json", jsx:encode(Response)}.

get_users(Arg) ->
    % Example query parameter handling
    QueryParams = yaws_api:parse_query(Arg),
    Limit = proplists:get_value("limit", QueryParams, "10"),

    Users = [
        #{id => 1, name => <<"John">>, email => <<"john@example.com">>},
        #{id => 2, name => <<"Jane">>, email => <<"jane@example.com">>}
    ],

    {content, "application/json", jsx:encode(#{
        users => Users,
        limit => list_to_integer(Limit),
        total => length(Users)
    })}.

create_user(Arg) ->
    % Example request body handling
    case yaws_api:parse_post(Arg) of
        {ok, Data} ->
            {content, "application/json", jsx:encode(#{
                message => <<"User created successfully">>,
                data => Data
            })};
        {error, Error} ->
            {content, "application/json", jsx:encode(#{
                error => Error
            })}
    end.

get_user_posts(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    Posts = [
        #{id => 1, title => <<"First Post">>, user_id => UserId},
        #{id => 2, title => <<"Second Post">>, user_id => UserId}
    ],

    {content, "application/json", jsx:encode(#{
        user_id => UserId,
        posts => Posts
    })}.

create_user_post(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(user_id, Params),

    {content, "application/json", jsx:encode(#{
        message => <<"Post created successfully">>,
        user_id => UserId
    })}.

update_user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    {content, "application/json", jsx:encode(#{
        message => <<"User updated successfully">>,
        user_id => UserId
    })}.

delete_user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    {content, "application/json", jsx:encode(#{
        message => <<"User deleted successfully">>,
        user_id => UserId
    })}.
