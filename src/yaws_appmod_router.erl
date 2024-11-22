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

%% Initialize router state
init() ->
    erlang:put(routes, []).

%% Add a new route to the router
add_route(Method, PathPattern, Handler, Middlewares) ->
    Route = #route{
        method = Method,
        path_pattern = PathPattern,
        handler = Handler,
        middlewares = Middlewares
    },
    Routes = erlang:get(routes),
    erlang:put(routes, [Route | Routes]).

%% Find matching routes for a request
find_route(Method, Path) ->
    Routes = erlang:get(routes),
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
