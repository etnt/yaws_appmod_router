-module(yaws_appmod_router).
-export([
    init/0,
    add_route/4,
    find_route/2,
    match_path/2,
    execute_middlewares/2,
    appmod/2
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

%% Records
-record(route, {
    method,
    path_pattern,
    handler,
    middlewares = []
}).

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
add_route(Method, PathPattern, Handler, Middlewares) ->
    yaws_appmod_router_server:add_route(Method, PathPattern, Handler, Middlewares).

%% Find matching routes for a request
find_route(Method, Path) ->
    % Direct access to the ETS table for reading
    Routes = [Route || {route, Route} <- ets:tab2list(?TABLE_NAME)],
    lists:filter(
        fun(#route{method = M, path_pattern = PP}) ->
            M =:= Method andalso match_path(PP, Path)
        end,
        Routes
    ).

%% Match a path pattern against an actual path
match_path(Pattern, Path) ->
    PatternSegments = string:split(Pattern, "/", all),
    PathSegments = string:split(Path, "/", all),
    case length(PatternSegments) =:= length(PathSegments) of
        true -> match_segments(PatternSegments, PathSegments);
        false -> false
    end.

%% Match individual path segments
match_segments([], []) ->
    true;
match_segments([[$: | _] | RestPattern], [_ | RestPath]) ->
    match_segments(RestPattern, RestPath);
match_segments([Same | RestPattern], [Same | RestPath]) ->
    match_segments(RestPattern, RestPath);
match_segments(_, _) ->
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
appmod(Req, _State) ->
    Method = atom_to_list((Req#arg.req)#http_request.method),
    Path = Req#arg.pathinfo,
    
    case find_route(Method, Path) of
        [#route{handler = Handler, middlewares = Middlewares} | _] ->
            case execute_middlewares(Middlewares, Req) of
                {ok, UpdatedReq} ->
                    Handler(UpdatedReq);
                {error, Reason} ->
                    {html, io_lib:format("<h1>Error: ~p</h1>", [Reason])}
            end;
        [] ->
            {html, "<h1>404 Not Found</h1>"}
    end.
