-module(advanced_routing).
-export([start/0]).
-export([
    % Route handlers
    get_user/1,
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

-define(PORT, 8080).
-define(HOST, "localhost").


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
    % curl -is "http://127.0.0.1:8080/api/users/bill"
    yaws_appmod_router:add_route("GET", "/api/users/:id", fun advanced_routing:get_user/1, [
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
    yaws_appmod_router:add_route("GET", "/api/users/:id/posts", fun advanced_routing:get_user_posts/1, [
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
        {port, ?PORT},
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
    io:format("Advanced routing server started on port 8080~n",[]),
    ok.

%% Middleware implementations
cors_middleware(Arg) ->
    Hdrs = maps:get(headers, Arg#arg.opaque, []),
    NewHdrs = [
        {header, {"Access-Control-Allow-Origin", "*"}},
        {header, {"Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS"}},
        {header, {"Access-Control-Allow-Headers", "Content-Type"}}
    ],
    {ok, Arg#arg{opaque = maps:put(headers, NewHdrs ++ Hdrs, Arg#arg.opaque)}}.

json_middleware(Arg) ->
    Headers = [{header, {content_type, "application/json"}}],
    {ok, Arg#arg{opaque = maps:put(json_headers, Headers, Arg#arg.opaque)}}.

rate_limit_middleware(Arg) ->
    % Simple rate limiting example - could be enhanced with proper storage
    QueryParams = yaws_api:parse_query(Arg),
    RC = proplists:get_value("request-counter", QueryParams, "1"),

    try list_to_integer(RC) of
        Count when Count > 100 ->
            {error, [{status, 429},
                     {content, "application/json",
                      "{\"error\": \"Too many requests. Please try again later.\"}" }]};
        _ ->
            {ok, Arg}
    catch
        _:_ ->
            {error, [{status, 400},
                     {content, "application/json",
                      "{\"error\": \"Malformed request-counter param\"}"}]}
    end.

validate_user_middleware(Arg) ->
    Params = Arg#arg.appmoddata,
    case maps:get(id, Params, undefined) of
        "bill" ->
            {ok, Arg};
        _ ->
            {error, 
                [{status, 400},
                 {content, "application/json", "{\"error\": \"Wrong User ID \"}"}
                ]}
    end.

logger_middleware(Arg) ->
    Method = (Arg#arg.req)#http_request.method,
    Path = Arg#arg.server_path,
    TS = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    io:format("<~s> ~p ~p~n", [TS, Method, Path]),
    {ok, Arg}.

%% Route handlers
api_status(#arg{opaque = OpaqueMap} = _Arg) ->
    Hdrs = maps:get(cors_headers, OpaqueMap, []),
    Response = #{
        status => <<"ok">>,
        version => <<"1.0.0">>
    },
    [{status,200} | Hdrs] ++
    [{content, "application/json", json:encode(Response)}].

get_users(#arg{opaque = OpaqueMap} = _Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Users = [
        #{id => 1, name => <<"John">>, email => <<"john@example.com">>},
        #{id => 2, name => <<"Jane">>, email => <<"jane@example.com">>}
    ],
    [{status,200} | Hdrs] ++
    [{content, "application/json",
      json:encode(#{
        users => Users,
        total => length(Users)
      })}].

%% Example request: curl -is -X POST -d 'user=bill'  http://localhost:8080/api/users
create_user(#arg{opaque = OpaqueMap} = Arg) ->
    % Example request body handling
    case yaws_api:parse_post(Arg) of
        [{"user",User}] ->
            Hdrs = maps:get(headers, OpaqueMap, []),
            Location = {"Location", "http://"++?HOST++":"++integer_to_list(?PORT)++"/api/users/"++User},
            [{status,200},
             {header, Location} | Hdrs] ++
            [{content, "application/json", json:encode(#{
                message => <<"User created successfully">>,
                user => list_to_binary(User)
            })}];
        {error, Error} ->
            [{status, 400},
             {content, "application/json", json:encode(#{
                error => Error
             })}]
    end.

get_user_posts(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    case UserId of
        <<"bill">> ->
            Posts = [
                #{id => 1, title => <<"First Post">>, user_id => UserId},
                #{id => 2, title => <<"Second Post">>, user_id => UserId}
            ],

            {content, "application/json", json:encode(#{
                user_id => UserId,
                posts => Posts
            })};
        _ ->
            [{status, 404},
             {content, "application/json", json:encode(#{
                error => <<"User not found">>
             })}]
    end.


get_user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    case UserId of
        "bill" ->
            {content, "application/json", json:encode(#{
                user_id => list_to_binary(UserId),
                name => <<"Bill Smith">>
            })};
        _ ->
            [{status, 404},
             {content, "application/json", json:encode(#{
                error => <<"User not found">>
             })}]
    end.


create_user_post(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(user_id, Params),

    {content, "application/json", json:encode(#{
        message => <<"Post created successfully">>,
        user_id => UserId
    })}.

update_user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    {content, "application/json", json:encode(#{
        message => <<"User updated successfully">>,
        user_id => UserId
    })}.

delete_user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    {content, "application/json", json:encode(#{
        message => <<"User deleted successfully">>,
        user_id => UserId
    })}.
