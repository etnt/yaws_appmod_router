-module(yaws_appmod_router).
-export([
    init/0,
    add_route/4,
    find_route/2,
    match_path/2,
    execute_middlewares/2,
    out/1,
    http_methods/0,
    is_http_method/1,
    is_crud/1,
    is_crud_operation/1,
    crud_operations/0,
    print_routes/0
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").

-export_type([crud_operations/0,
              crud/0,
              actions/0]).

-type crud_operations() :: $C | $R | $U | $D.
-type crud() :: list(crud_operations()).
-type actions() :: index | show | create | replace | modify | delete | options.

-define(is_crud_operation(Operation), is_integer(Operation)).

-define(TABLE_NAME, yaws_appmod_routes).


%% Initialize router state
init() ->
    case whereis(yaws_appmod_router_server) of
        undefined ->
            {ok, _} = yaws_appmod_router_server:start_link();
        _ ->
            ok
    end.

%% Add a new route to the router
add_route(Method, PathPattern, Handler, Middlewares)
  when is_list(Method) andalso is_list(PathPattern) andalso
     is_function(Handler, 1) andalso is_list(Middlewares) ->
    yaws_appmod_router_server:add_route(Method, PathPattern, Handler, Middlewares).

print_routes() ->
    Routes = [R || {route,R} <- ets:tab2list(?TABLE_NAME)],
    Sorted = lists:keysort(#route.path_pattern, Routes),
    pprint_header(),
    lists:foreach(fun pprint/1, Sorted).

pprint_header() ->
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
    %% Direct access to the ETS table for reading
    Routes = [Route || {route, #route{method = M} = Route} <- ets:tab2list(?TABLE_NAME),
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

%% Main appmod entry point
out(Req) ->
    try
        Method = atom_to_list((Req#arg.req)#http_request.method),
        Path = Req#arg.server_path,

        case find_route(Method, Path) of
            [#route{action = options, crud = Crud} | _] when is_list(Crud) ->
                return_options(Req, Crud);
            [#route{handler = Handler, middlewares = Middlewares, params = Params} | _] ->
                case execute_middlewares(Middlewares, Req#arg{appmoddata = Params}) of
                    {ok, UpdatedReq} ->
                        Handler(UpdatedReq);
                    {error, Reason} ->
                        Reason
                end;
            [] ->
                [{status, 405}]  % Method not allowed
        end
    catch
        _:Error:StackTrace ->
            io:format("~p(~p) ERROR: ~p~n~p~n", [?MODULE, ?LINE, Error, StackTrace]),
            [{status, 500},
             {content, "text/html", "<h1>500 Internal Server Error</h1>"}
            ]
    end.

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
    ["GET", "POST", "PUT", "DELETE", "OPTIONS"].

is_http_method(Method) ->
    lists:member(Method, http_methods()).

crud_operations() ->
    [$C, $R, $U, $D].
is_crud_operation(Operation) when ?is_crud_operation(Operation) ->
    lists:member(Operation, crud_operations()).

is_crud(Crud) when is_list(Crud) ->
    lists:all(fun is_crud_operation/1, Crud).

