-module(yaws_appmod_router).
-export([
    init/0,
    add_route/4,
    find_route/2,
    match_path/2,
    execute_middlewares/2,
    out/1
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").



-define(TABLE_NAME, yaws_appmod_routes).

%% Initialize router state
init() ->
    % Start the router server if not already started
    case whereis(yaws_appmod_router_server) of
        undefined ->
            {ok, _} = yaws_appmod_router_server:start_link();
        _ ->
            ok
    end.

%% Add a new route to the router
add_route(Method, PathPattern, Handler, Middlewares)
    when is_function(Handler, 1) andalso is_list(Middlewares) ->
    yaws_appmod_router_server:add_route(Method, PathPattern, Handler, Middlewares).

%% Find matching routes for a request
find_route(Method, Path) ->
    % Direct access to the ETS table for reading
    Routes = [Route || {route, Route} <- ets:tab2list(?TABLE_NAME)],
    io:format("Routes: ~p~n", [Routes]),
    lists:filtermap(
        fun(#route{method = M, path_pattern = PP} = Route) ->
            case M =:= Method of
                true ->
                    case match_path(PP, Path) of
                        {true, Params} -> {true, Route#route{params = Params}};
                        false -> false
                    end;
                false -> 
                    false
            end
        end,
        Routes
    ).

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
execute_middlewares([], Req) ->
    {ok, Req};
execute_middlewares([Middleware | Rest], Req) ->
    case Middleware(Req) of
        {ok, UpdatedReq} -> execute_middlewares(Rest, UpdatedReq);
        {error, Reason} -> {error, Reason}
    end.

%% Main appmod entry point
out(Req) ->
    Method = atom_to_list((Req#arg.req)#http_request.method),
    Path = Req#arg.server_path,

    case find_route(Method, Path) of
        [#route{handler = Handler, middlewares = Middlewares, params = Params} | _] ->
            case execute_middlewares(Middlewares, Req#arg{appmoddata = Params}) of
                {ok, UpdatedReq} ->
                    Handler(UpdatedReq);
                {error, Reason} ->
                    Reason
            end;
        [] ->
            {html, "<h1>404 Not Found</h1>"}
    end.
