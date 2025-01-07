-module(yaws_appmod_router).
-export([
    auth/2,
    enable_auth/1,
    disable_auth/1,
    is_auth_enabled/1,
    init/0,
    init/1,
    add_route/4,
    table_add_route/5,
    find_route/2,
    find_route/3,
    match_path/2,
    execute_middlewares/2,
    out/1,
    http_methods/0,
    is_http_method/1,
    is_crud/1,
    is_crud_operation/1,
    crud_operations/0,
    print_routes/0,
    print_routes/1,
    mk_table_name/2
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").

-export_type([crud_operations/0,
              crud/0,
              actions/0,
              table_name/0,
              error_msg/0
            ]).

-type crud_operations() :: $C | $R | $U | $D.
-type crud() :: list(crud_operations()).
-type actions() :: index | show | create | replace | modify | delete | options.
-type table_name() :: atom().
-type error_msg() :: binary().


-define(is_crud_operation(Operation), is_integer(Operation)).

-define(TABLE_NAME, yaws_appmod_routes).
-define(TABLE_NAME_STRING, "yaws_appmod_routes").


%%% @doc Initialize default router table
-spec init() -> {ok, table_name()} | {error, error_msg()} .
init() ->
    init(?TABLE_NAME).

%%% @doc Initialize router table with given name
-spec init(table_name()) -> {ok, table_name()} | {error, error_msg()} .
init(TableName) ->
    case whereis(yaws_appmod_router_server) of
        undefined ->
            {ok, _} = yaws_appmod_router_server:start_link(),
            yaws_appmod_router_server:create_table(TableName);
        _ ->
            yaws_appmod_router_server:create_table(TableName)
    end.

%%% @doc Create a table name that include ListenIp and Port
-spec mk_table_name(inet:ip_address(), inet:port_number()) -> atom().
mk_table_name(IP, Port) ->
    list_to_atom(?TABLE_NAME_STRING ++ "_" ++ inet_parse:ntoa(IP) ++ "_" ++ integer_to_list(Port)).

%%% @doc Enable Yaws auth/2 callback handling
-spec enable_auth(OpaqueMap :: map()) -> map().
enable_auth(OpaqueMap) when is_map(OpaqueMap) ->
    OpaqueMap#{yaws_appmod_app_auth => true}.

%%% @doc Disable Yaws auth/2 callback handling
-spec disable_auth(OpaqueMap :: map()) -> map().
disable_auth(OpaqueMap) when is_map(OpaqueMap) ->
    OpaqueMap#{yaws_appmod_app_auth => false}.

%%% @doc Check if Yaws auth/2 callback handling is enabled
-spec is_auth_enabled(OpaqueMap :: map()) -> boolean().
is_auth_enabled(OpaqueMap) when is_map(OpaqueMap) ->
    maps:get(yaws_appmod_app_auth, OpaqueMap, false).

%%% @doc Add a new route to the default router table
-spec add_route(Method :: string(),
                PathPattern :: string(),
                Handle :: function(),
                Middlewares :: list() ) ->
        ok.
add_route(Method, PathPattern, Handler, Middlewares) ->
    table_add_route(?TABLE_NAME, Method, PathPattern, Handler, Middlewares).


%%% @doc Add a new route to the specified router table
-spec table_add_route(TableName :: atom(),
                        Method :: string(),
                        PathPattern :: string(),
                        Handle :: function(),
                        Middlewares :: list() ) ->
        ok.
table_add_route(Table, Method, PathPattern, Handler, Middlewares)
  when is_list(Method) andalso is_list(PathPattern) andalso
     is_function(Handler, 1) andalso is_list(Middlewares) ->
    yaws_appmod_router_server:add_route(Table, Method, PathPattern, Handler, Middlewares).


%%% @doc Print all routes
-spec print_routes() ->
        ok.
print_routes() ->
    {ok, Tables} = yaws_appmod_router_server:get_tables(),
    print_routes(Tables).

%%% @doc Print all routes in the specified router tables
-spec print_routes(TableNames :: [atom()]) ->
        ok.
print_routes(Tables) when is_list(Tables) ->
    F = fun(Table) ->
            Routes = [R || {route,R} <- ets:tab2list(Table)],
            Sorted = lists:keysort(#route.path_pattern, Routes),
            pprint_header(Table),
            lists:foreach(fun pprint/1, Sorted)
        end,
    lists:foreach(F, Tables).

pprint_header(Table) ->
    io:format("~n>>> TABLE(~w)~n", [Table]),
    io:format("~-8.s ~-40.s ~-5.s ~-9.s ~s~n", ["METHOD", "PATH PATTERN", "CRUD", "ACTION", "HANDLER"]),
    io:format("~-8.s ~-40.s ~-5.s ~-9.s ~s~n", ["------", "------------", "----", "------", "-------"]).

pprint(#route{method       = Method,
              path_pattern = PathPattern,
              handler      = Handler,
              action       = Action,
              crud         = Crud}) ->
    io:format("~-8.s ~-40.s ~-5.s ~-9.w ~w~n", [Method, PathPattern, pp_crud(Crud), Action, Handler]);
pprint(X) ->
    io:format(">>> ~p~n", [X]).

pp_crud(undefined) -> "-";
pp_crud(Crud)      -> Crud.

%% Find matching routes for a request
find_route(Method, Path) ->
    find_route(Method, Path, ?TABLE_NAME).

find_route(Method, Path, TableName) ->
    %% Direct access to the ETS table for reading
    Routes = [Route || {route, #route{method = M} = Route} <- ets:tab2list(TableName),
                        M == Method],
    lists:filtermap(
        fun(#route{path_pattern = PP, action = Action} = Route) ->
            case match_path(PP, Path) of
                {true, Params} when Action == undefined->
                    {true, Route#route{params = Params}};
                {true, Params} when Action /= undefined->
                    {true, Route#route{params = Params#{action => Action}}};
                false -> false
            end
        end, Routes).


%% Match a path pattern against an actual path
match_path(Pattern, Path) ->
    PatternSegments = string:split(Pattern, "/", all),
    PathSegments = string:split(Path, "/", all),
    case length(PatternSegments) =:= length(PathSegments) of
        true -> match_segments(PatternSegments, PathSegments, #{});
        false -> false
    end.

%% Match individual path segments and build params map
match_segments([], [], Params) ->
    {true, Params};
match_segments([[$: | ParamName] | RestPattern], [Value | RestPath], Params) ->
    match_segments(RestPattern, RestPath, Params#{list_to_atom(ParamName) => Value});
match_segments([Same | RestPattern], [Same | RestPath], Params) ->
    match_segments(RestPattern, RestPath, Params);
match_segments(_, _, _) ->
    false.

%% Execute middleware chain
execute_middlewares(Middlewares, Req) ->
    try
        {ok, lists:foldl(
               fun(Middleware, AccReq) ->
                       case Middleware(AccReq) of
                           {ok, NewReq} -> NewReq;
                           {error, _Reason} = Error -> throw(Error)
                       end
               end, Req, Middlewares)}
    catch
        throw:Error ->
            Error;

        _:Error ->
                 io:format("~p(~p) INTERNAL ERROR: ~p~n", [?MODULE, ?LINE, Error]),
                 {error,
                  [{status, 500},
                   {content, "text/html", "<h1>500 Internal Server Error</h1>"}
                  ]}
    end.

auth(#arg{opaque = OpaqueMap} = Arg, Auth) when is_map(OpaqueMap) ->
    case is_auth_enabled(OpaqueMap) of
        true ->
            invoke_auth(Arg, Auth);
        false ->
            true
    end.

invoke_auth(#arg{server_path = Path,
                 opaque      = OpaqueMap} = Arg,
            Auth) ->
    try
        Method = atom_to_list(yaws_api:http_request_method(yaws_api:arg_req(Arg))),
        TableName = get_table_name(OpaqueMap, ?TABLE_NAME),

        case find_route(Method, Path, TableName) of
            [#route{action = options, crud = Crud}] when is_list(Crud) ->
                true;
            [#route{handler = Handler}] ->
                %% NOTE: The Handler may not implement the auth/2 callback,
                %% so we just ignore a function_clause crash here.
                try
                    Handler({Arg, Auth})
                catch
                    error:function_clause ->
                        true
                end;
            L when is_list(L) andalso length(L) > 1 ->
                io:format("~p(~p) AUTH ERROR: Multiple routes found for ~p ~p , ~p~n",
                            [?MODULE, ?LINE, Method, Path, L]),
                false;
            [] ->
                true
        end
    catch
        Type:Error:StackTrace ->
            io:format("~p(~p) AUTH ERROR: ~p~n~p~n", [?MODULE, ?LINE, {Type,Error}, StackTrace]),
            false
    end.

%% Main appmod entry point
out(Arg) when is_record(Arg, arg) ->
    try
        Method = atom_to_list(yaws_api:http_request_method(yaws_api:arg_req(Arg))),
        Path = Arg#arg.server_path,
        OpaqueMap = Arg#arg.opaque,
        TableName = get_table_name(OpaqueMap, ?TABLE_NAME),

        case find_route(Method, Path, TableName) of
            [#route{action = options, crud = Crud}] when is_list(Crud) ->
                return_options(Arg, Crud);
            [#route{handler = Handler, middlewares = Middlewares, params = Params}] ->
                case execute_middlewares(Middlewares, Arg#arg{appmoddata = Params}) of
                    {ok, UpdatedReq} ->
                        Handler(UpdatedReq);
                    {error, Reason} ->
                        Reason
                end;
            L when is_list(L) andalso length(L) > 1 ->
                io:format("~p(~p) ERROR: Multiple routes found for ~p ~p , ~p~n",
                          [?MODULE, ?LINE, Method, Path, L]),
                [{status, 500},
                 {content, "text/html", "<h1>500 Internal Server Error</h1>"}
                ];
            [] ->
                [{status, 404}]  % Not Found
        end
    catch
        _:Error:StackTrace ->
            io:format("~p(~p) ERROR: ~p~n~p~n", [?MODULE, ?LINE, Error, StackTrace]),
            [{status, 500},
             {content, "text/html", "<h1>500 Internal Server Error</h1>"}
            ]
    end.

get_table_name(OpaqueMap, Default) ->
    %% In case OpaqueMap is not a map, return Default
    try maps:get(table_name, OpaqueMap, Default)
    catch _:_ -> Default end.

return_options(_Req, Crud) when is_list(Crud) ->
    Methods =
        lists:foldr(
            fun( $C, Acc) -> "POST, " ++ Acc;
                ($R, Acc) -> "GET, " ++ Acc;
                ($U, Acc) -> "PUT, PATCH, " ++ Acc;
                ($D, Acc) -> "DELETE, " ++ Acc
            end, "OPTIONS", Crud),
    [{status, 204},
     {header, {"Allow", Methods}}
    ].

http_methods() ->
    ["GET", "HEAD", "POST", "PUT", "DELETE", "OPTIONS"].

is_http_method(Method) ->
    lists:member(Method, http_methods()).

crud_operations() ->
    [$C, $R, $U, $D].
is_crud_operation(Operation) when ?is_crud_operation(Operation) ->
    lists:member(Operation, crud_operations()).

is_crud(Crud) when is_list(Crud) ->
    lists:all(fun is_crud_operation/1, Crud).

