-module(crud_routing).
-export([start/0]).
-export([
    % Route handlers
    handle_workers/1,
    handle_semaphores/1,

    % Middleware
    cors_middleware/1,
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

    yaws_appmod_router:add_route("CRUD", "/api/workers",
                                 %% Handler
                                 fun crud_routing:handle_workers/1,
                                 %% Middlewares
                                 [
                                  fun crud_routing:logger_middleware/1,
                                  fun crud_routing:cors_middleware/1,
                                  fun crud_routing:json_post_data_middleware/1
                                 ]),

    yaws_appmod_router:add_route("CRD", "/api/semaphores",
                                 %% Handler
                                 fun crud_routing:handle_semaphores/1,
                                 %% Middlewares
                                 [
                                  fun crud_routing:logger_middleware/1,
                                  fun crud_routing:cors_middleware/1
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
         {servername, "crud_server"},
         {listen, {127, 0, 0, 1}},
         {docroot, Docroot},
         {opaque, #{}},
         {appmods, [{"/", yaws_appmod_router}]}
        ],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    [supervisor:start_child(yaws_sup, Ch) || Ch <- ChildSpecs],
    ok = yaws_api:setconf(GC, SCList),
    io:format("CRUD routing server started on port 8080~n",[]),
    ok.

%% Middleware implementations
cors_middleware(Arg) ->
    Hdrs = maps:get(headers, Arg#arg.opaque, []),
    NewHdrs =
        [
         {header, {"Access-Control-Allow-Origin", "*"}},
         {header, {"Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS"}}, % FIXME Methods
         {header, {"Access-Control-Allow-Headers", "Content-Type"}}
        ],
    {ok, Arg#arg{opaque = maps:put(headers, NewHdrs ++ Hdrs, Arg#arg.opaque)}}.


json_post_data_middleware(#arg{clidata    = Data,
                               appmoddata = AppmodMap,
                               opaque     = OpaqueMap} = Arg) ->
    Action = maps:get(action, AppmodMap, undefined),
    case lists:member(Action, [create, replace, modify]) of
        true ->
            case json:decode(Data) of
                JsonMap when is_map(JsonMap) ->
                    {ok, Arg#arg{opaque = maps:put(json_data, JsonMap, OpaqueMap)}};
                _ ->
                    {error, [{status, 400},
                             {content, "application/json",
                              "{\"error\": \"Malformed JSON data\"}"}]}
            end;
        false ->
            {ok, Arg}
    end.


logger_middleware(Arg) ->
    Method = (Arg#arg.req)#http_request.method,
    Path = Arg#arg.server_path,
    Host = yaws_api:headers_host(Arg#arg.headers),
    TS = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    io:format("<~s> (~s) ~p ~p~n", [TS, Host,Method, Path]),
    {ok, Arg}.

%%% ---------------------------------------------------------------------------
%%% R O U T E   H A N D L E R S
%%% ---------------------------------------------------------------------------

handle_semaphores(_Arg) ->
    [{status,200}]. % TODO mostly for testing OPTIONS


handle_workers(#arg{appmoddata = Map} = Arg) ->
    case maps:get(action, Map) of
        index   -> get_workers(Arg);
        show    -> get_worker(Arg);
        create  -> create_worker(Arg);
        replace -> replace_worker(Arg);
        modify  -> modify_worker(Arg);
        delete  -> delete_worker(Arg);
        _       -> method_not_allowed(Arg)
    end.

method_not_allowed(_Arg) ->
    [{status, 405},
     {content, "application/json", "{\"error\": \"Method not allowed\"}"}].

get_workers(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    QueryParams = yaws_api:parse_query(Arg),
    Limit = list_to_integer(proplists:get_value("limit", QueryParams, "10")),
    Workers = [
               #{id => 1, name => <<"John">>, email => <<"john@example.com">>},
               #{id => 2, name => <<"Jane">>, email => <<"jane@example.com">>}
              ],
    [{status,200} | Hdrs] ++
        [{content, "application/json",
          json:encode(#{
                        workers => Workers,
                        total => length(Workers),
                        limit => Limit
                       })}].


create_worker(#arg{opaque = OpaqueMap} = _Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    JsonMap = maps:get(json_data, OpaqueMap, #{}),
    case {maps:get(<<"worker">>, JsonMap, undefined), maps:get(<<"email">>, JsonMap, undefined)} of
        {Name, Email} when is_binary(Name), is_binary(Email) ->
            Location = {"Location", "http://"++?HOST++":"++integer_to_list(?PORT)++"/api/workers/"++binary_to_list(Name)},
            [{status,201}, {header, Location} | Hdrs] ++
                [{content, "application/json",
                  json:encode(#{worker => Name, email => Email})}];
        _ ->
            [{status, 400},
             {content, "application/json", "{\"error\": \"Missing or invalid parameters\"}"}]
    end.

get_worker(#arg{opaque = OpaqueMap} = Arg) ->
    Hdrs = maps:get(headers, OpaqueMap, []),
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),
    case UserId of
        "bill" ->
            {content, "application/json",
             json:encode(#{
                           worker => list_to_binary(UserId),
                           name => <<"Bill Smith">>
                          })};
        _ ->
            [{status, 404} | Hdrs] ++
             [{content, "application/json",
               json:encode(#{
                             error => <<"User not found">>
                            })}]
    end.

replace_worker(#arg{appmoddata = Map, opaque = OpaqueMap} = _Arg) ->
    case maps:get(id, Map) of
        "bill" ->
            JsonMap = maps:get(json_data, OpaqueMap, #{}),
            case {maps:get(<<"worker">>, JsonMap, undefined), maps:get(<<"email">>, JsonMap, undefined)} of
                {Name, Email} when is_binary(Name), is_binary(Email) ->
                    Worker = #{worker => Name, email => Email},
                    [{status,200}] ++
                        [{content, "application/json", json:encode(Worker)}];
                _ ->
                    [{status, 400},
                     {content, "application/json", "{\"error\": \"Missing or invalid parameters\"}"}]
            end;
        _ ->
            [{status, 404},
             {content, "application/json", "{\"error\": \"Worker not found\"}"}]
    end.

modify_worker(#arg{appmoddata = Map, opaque = OpaqueMap} = _Arg) ->
    case maps:get(id, Map) of
        Id when is_integer(Id) ->
            JsonMap = maps:get(json_data, OpaqueMap, #{}),
            case {maps:get(<<"worker">>, JsonMap, undefined), maps:get(<<"email">>, JsonMap, undefined)} of
                {Name, Email} when is_binary(Name) or is_binary(Email) ->
                    Worker = #{worker => Name, email => Email},
                    [{status,200}] ++
                        [{content, "application/json", json:encode(Worker)}];
                _ ->
                    [{status, 400},
                     {content, "application/json", "{\"error\": \"Missing or invalid parameters\"}"}]
            end;
        _ ->
            [{status, 404}, {content, "application/json", "{\"error\": \"Worker not found\"}"}]
    end.

delete_worker(#arg{appmoddata = Map} = _Arg) ->
    case maps:get(id, Map) of
        "bill" ->
            [{status,204}];
        _ ->
            [{status, 404}, {content, "application/json", "{\"error\": \"Worker not found\"}"}]
    end.
