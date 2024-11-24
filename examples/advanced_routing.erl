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
    options_user/1,

    % Middleware
    rate_limit_middleware/1,
    cors_middleware/1,
    validate_user_middleware/1,
    json_post_data_middleware/1,
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
    yaws_appmod_router:add_route("GET", "/api/status",
                                 %% Handler
                                 fun advanced_routing:api_status/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1
                                 ]),

    yaws_appmod_router:add_route("GET", "/api/users",
                                 %% Handler
                                 fun advanced_routing:get_users/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:rate_limit_middleware/1
                                 ]),

    yaws_appmod_router:add_route("POST", "/api/users",
                                 %% Handler
                                 fun advanced_routing:create_user/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:json_post_data_middleware/1,
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:rate_limit_middleware/1
                                 ]),

    yaws_appmod_router:add_route("OPTIONS", "/api/users",
                                 %% Handler
                                 fun advanced_routing:options_user/1,
                                 %% Middlewares
                                 []),

    %% Single user operations with validation
    yaws_appmod_router:add_route("GET", "/api/users/:id",
                                 %% Handler
                                 fun advanced_routing:get_user/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:validate_user_middleware/1
                                 ]),

    yaws_appmod_router:add_route("PUT", "/api/users/:id",
                                 %% Handler
                                 fun advanced_routing:update_user/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:validate_user_middleware/1
                                 ]),

    yaws_appmod_router:add_route("DELETE", "/api/users/:id",
                                 %% Handler
                                 fun advanced_routing:delete_user/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:validate_user_middleware/1
                                 ]),

    % Nested routes for user posts
    yaws_appmod_router:add_route("GET", "/api/users/:id/posts",
                                 %% Handler
                                 fun advanced_routing:get_user_posts/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:validate_user_middleware/1
                                 ]),

    yaws_appmod_router:add_route("POST", "/api/users/:id/posts",
                                 %% Handler
                                 fun advanced_routing:create_user_post/1,
                                 %% Middlewares
                                 [
                                  fun advanced_routing:json_post_data_middleware/1,
                                  fun advanced_routing:cors_middleware/1,
                                  fun advanced_routing:validate_user_middleware/1
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
    SconfList =
        [
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
    NewHdrs =
        [
         {header, {"Access-Control-Allow-Origin", "*"}},
         {header, {"Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS"}},
         {header, {"Access-Control-Allow-Headers", "Content-Type"}}
        ],
    {ok, Arg#arg{opaque = maps:put(headers, NewHdrs ++ Hdrs, Arg#arg.opaque)}}.

rate_limit_middleware(Arg) ->
    %% Simple rate limiting example
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

json_post_data_middleware(#arg{clidata = Data,
                               opaque = OpaqueMap} = Arg) ->
    case json:decode(Data) of
        JsonMap when is_map(JsonMap) ->
            {ok, Arg#arg{opaque = maps:put(json_data, JsonMap, OpaqueMap)}};
        _ ->
            {error, [{status, 400},
                     {content, "application/json",
                      "{\"error\": \"Malformed JSON data\"}"}]}
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

%%% ---------------------------------------------------------------------------
%%% R O U T E   H A N D L E R S
%%% ---------------------------------------------------------------------------
api_status(#arg{opaque = OpaqueMap} = _Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Response = #{
                 status => <<"ok">>,
                 version => <<"1.0.0">>
                },
    [{status,200} | Hdrs] ++
        [{content, "application/json", json:encode(Response)}].

options_user(Arg) ->
    Other = yaws_api:headers_other(yaws_api:arg_headers(Arg)),
    Acrm = [{Hdr,HdrVal} || {http_header, _, Hdr, _, HdrVal} <- Other,
                            "access-control-request-method" == string:to_lower(Hdr)],
    case Acrm of
        [{_AccessControlRequestMethod, _Method}] ->
            [{status,200},
             {header, {"Access-Control-Allow-Origin", "*"}}, % FIXME
             {header, {"Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS"}}
            ];
        _ ->
            [{status,200},
             {header, {"Allow", "GET, POST, PUT, PATCH, DELETE, OPTIONS"}}
            ]
    end.

get_users(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    QueryParams = yaws_api:parse_query(Arg),
    Limit = list_to_integer(proplists:get_value("limit", QueryParams, "10")),
    Users = [
             #{id => 1, name => <<"John">>, email => <<"john@example.com">>},
             #{id => 2, name => <<"Jane">>, email => <<"jane@example.com">>}
            ],
    [{status,200} | Hdrs] ++
        [{content, "application/json",
          json:encode(#{
                        users => Users,
                        total => length(Users),
                        limit => Limit
                       })}].

%% Example request: curl -is -X POST -d 'user=bill'  http://localhost:8080/api/users
create_user(#arg{opaque = OpaqueMap} = _Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    JsonMap = maps:get(json_data, OpaqueMap, #{}),
    case {maps:get(<<"user">>, JsonMap, undefined), maps:get(<<"email">>, JsonMap, undefined)} of
        {User, Email} when is_binary(User), is_binary(Email) ->
            UserStr = binary_to_list(User),
            Location = {"Location", "http://"++?HOST++":"++integer_to_list(?PORT)++"/api/users/"++UserStr},
            [{status,200}, {header, Location} | Hdrs] ++
                [{content, "application/json",
                  json:encode(#{
                                message => <<"User created successfully">>,
                                user => User,
                                email => Email
                               })}];
        {error, Error} ->
            [{status, 400} | Hdrs] ++
             [{content, "application/json",
               json:encode(#{
                             error => Error
                            })}]
    end.

get_user_posts(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    case UserId of
        <<"bill">> ->
            Posts =
                [
                 #{id => 1, title => <<"First Post">>, user_id => UserId},
                 #{id => 2, title => <<"Second Post">>, user_id => UserId}
                ],

            [{status,200} | Hdrs] ++
            [{content, "application/json",
              json:encode(#{
                            user_id => UserId,
                            posts => Posts
                           })}];
        _ ->
            [{status, 404} | Hdrs] ++
             [{content, "application/json",
               json:encode(#{
                             error => <<"User not found">>
                            })}]
    end.


get_user(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    case UserId of
        "bill" ->
            {content, "application/json",
             json:encode(#{
                           user_id => list_to_binary(UserId),
                           name => <<"Bill Smith">>
                          })};
        _ ->
            [{status, 404} | Hdrs] ++
             [{content, "application/json",
               json:encode(#{
                             error => <<"User not found">>
                            })}]
    end.


create_user_post(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),

    JsonMap = maps:get(json_data, OpaqueMap, #{}),
    Title = maps:get(<<"title">>, JsonMap, <<"">>),
    Content = maps:get(<<"content">>, JsonMap, <<"">>),

    [{status,200} | Hdrs] ++
    [{content, "application/json",
      json:encode(#{
                    message => <<"Post created successfully">>,
                    user_id => UserId,
                    title => Title,
                    content => Content
                   })}].

update_user(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    [{status,200} | Hdrs] ++
    [{content, "application/json",
      json:encode(#{
                    message => <<"User updated successfully">>,
                    user_id => UserId
                   })}].

delete_user(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = list_to_binary(maps:get(id, Params)),

    [{status,200} | Hdrs] ++
    [{content, "application/json",
      json:encode(#{
                    message => <<"User deleted successfully">>,
                    user_id => UserId
                   })}].
