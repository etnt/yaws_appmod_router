-module(test_yaws_appmod_router).
-include_lib("eunit/include/eunit.hrl").
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

%% Test Fixtures
setup() ->
    {ok, _} = application:ensure_all_started(yaws_appmod_router),
    yaws_appmod_router:init().

cleanup(_) ->
    application:stop(yaws_appmod_router).

%% Mock Functions
mock_handler(_Req) ->
    {html, "Hello Test"}.

mock_middleware(Req) ->
    {ok, Req#arg{headers = [{test_middleware, passed} | Req#arg.headers]}}.

failing_middleware(_Req) ->
    {error, "Middleware Failed"}.

%% Helper function to create test requests
make_request(Method, Path) ->
    #arg{
        req = #http_request{method = list_to_atom(Method)},
        headers = [],
        pathinfo = Path
    }.

%% Test Cases
basic_router_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Router initialization creates ETS table",
       fun() ->
           ?assert(ets:info(?TABLE_NAME) =/= undefined)
       end},
      
      {"Can add a simple route",
       fun() ->
           yaws_appmod_router:add_route("GET", "/test", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           [{route, Route}] = ets:tab2list(?TABLE_NAME),
           ?assertEqual("GET", Route#route.method),
           ?assertEqual("/test", Route#route.path_pattern)
       end}
     ]}.

route_matching_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Matches exact paths",
       fun() ->
           yaws_appmod_router:add_route("GET", "/exact", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           _Req = make_request("GET", "/exact"),
           [Match] = yaws_appmod_router:find_route("GET", "/exact"),
           ?assertMatch(#route{path_pattern="/exact"}, Match)
       end},
      
      {"Matches dynamic segments",
       fun() ->
           yaws_appmod_router:add_route("GET", "/user/:id", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           [Match] = yaws_appmod_router:find_route("GET", "/user/123"),
           ?assertMatch(#route{path_pattern="/user/:id"}, Match)
       end},
      
      {"No match for non-existent route",
       fun() ->
           yaws_appmod_router:add_route("GET", "/exists", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           ?assertEqual([], yaws_appmod_router:find_route("GET", "/not-exists"))
       end},
      
      {"Method mismatch returns no routes",
       fun() ->
           yaws_appmod_router:add_route("GET", "/method-test", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           ?assertEqual([], yaws_appmod_router:find_route("POST", "/method-test"))
       end}
     ]}.

middleware_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Successful middleware chain",
       fun() ->
           Middlewares = [fun mock_middleware/1],
           Req = make_request("GET", "/test"),
           ?assertMatch({ok, #arg{headers=[{test_middleware,passed}|_]}},
                       yaws_appmod_router:execute_middlewares(Middlewares, Req))
       end},
      
      {"Failing middleware stops chain",
       fun() ->
           Middlewares = [fun failing_middleware/1, fun mock_middleware/1],
           Req = make_request("GET", "/test"),
           ?assertEqual({error, "Middleware Failed"},
                       yaws_appmod_router:execute_middlewares(Middlewares, Req))
       end},
      
      {"Empty middleware chain succeeds",
       fun() ->
           Req = make_request("GET", "/test"),
           ?assertEqual({ok, Req},
                       yaws_appmod_router:execute_middlewares([], Req))
       end}
     ]}.

appmod_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Successful request handling",
       fun() ->
           yaws_appmod_router:add_route("GET", "/test", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           Req = make_request("GET", "/test"),
           ?assertEqual({html, "Hello Test"},
                       yaws_appmod_router:appmod(Req, undefined))
       end},
      
      {"404 for unknown route",
       fun() ->
           Req = make_request("GET", "/unknown"),
           ?assertEqual({html, "<h1>404 Not Found</h1>"},
                       yaws_appmod_router:appmod(Req, undefined))
       end},
      
      {"Middleware error handling",
       fun() ->
           yaws_appmod_router:add_route("GET", "/protected", fun mock_handler/1, 
                                      [fun failing_middleware/1]),
           timer:sleep(100), % Give gen_server time to process
           Req = make_request("GET", "/protected"),
           ?assertEqual({html, "<h1>Error: Middleware Failed</h1>"},
                       yaws_appmod_router:appmod(Req, undefined))
       end}
     ]}.

path_matching_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Matches simple static paths",
       fun() ->
           ?assert(yaws_appmod_router:match_path("/test", "/test")),
           ?assertNot(yaws_appmod_router:match_path("/test", "/other"))
       end},
      
      {"Matches paths with dynamic segments",
       fun() ->
           ?assert(yaws_appmod_router:match_path("/user/:id", "/user/123")),
           ?assert(yaws_appmod_router:match_path("/post/:id/comment/:cid", 
                                               "/post/456/comment/789"))
       end},
      
      {"Fails on segment count mismatch",
       fun() ->
           ?assertNot(yaws_appmod_router:match_path("/user/:id", "/user/123/extra")),
           ?assertNot(yaws_appmod_router:match_path("/user/:id/profile", "/user/123"))
       end}
     ]}.

server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Server process is registered",
       fun() ->
           ?assertNotEqual(undefined, whereis(yaws_appmod_router_server))
       end},
      
      {"Server maintains ETS table",
       fun() ->
           ?assertNotEqual(undefined, ets:info(?TABLE_NAME))
       end},
      
      {"Server handles route additions",
       fun() ->
           yaws_appmod_router:add_route("GET", "/server-test", fun mock_handler/1, []),
           timer:sleep(100), % Give gen_server time to process
           [{route, Route}] = ets:tab2list(?TABLE_NAME),
           ?assertEqual("GET", Route#route.method),
           ?assertEqual("/server-test", Route#route.path_pattern)
       end}
     ]}.
