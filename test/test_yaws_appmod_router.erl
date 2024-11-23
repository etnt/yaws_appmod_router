-module(test_yaws_appmod_router).
-include_lib("eunit/include/eunit.hrl").
-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").

-define(TABLE_NAME, yaws_appmod_routes).

%% Test Fixtures
setup() ->
    {ok, _} = application:ensure_all_started([yaws_appmod_router]),
    yaws_appmod_router:init().

cleanup(_) ->
    application:stop(yaws_appmod_router).

%% Mock Functions
mock_handler(_Req) ->
    {html, "Hello Test"}.

mock_middleware(Req) ->
    {ok, Req#arg{headers = [{test_middleware, passed} | Req#arg.headers]}}.

failing_middleware(_Req) ->
    {error, {html, "<h1>Error: Middleware Failed</h1>"}}.

%% Helper function to create test requests
make_request(Method, Path) ->
    #arg{
        req = #http_request{method = list_to_atom(Method)},
        headers = [],
        appmoddata = undefined,
        server_path = Path
    }.

%% Test Cases
basic_router_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Router initialization creates ETS table", fun() ->
            ?assert(ets:info(?TABLE_NAME) =/= undefined)
        end},

        {"Can add a simple route", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/test", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            [{route, Route}] = ets:tab2list(?TABLE_NAME),
            ?assertEqual("GET", Route#route.method),
            ?assertEqual("/test", Route#route.path_pattern)
        end}
    ]}.

route_matching_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Matches exact paths", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/exact", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            _Req = make_request("GET", "/exact"),
            [Match] = yaws_appmod_router:find_route("GET", "/exact"),
            ?assertMatch(#route{path_pattern = "/exact"}, Match)
        end},

        {"Matches dynamic segments with params", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/user/:id", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            [Match] = yaws_appmod_router:find_route("GET", "/user/123"),
            ?assertMatch(
                #route{path_pattern = "/user/:id", params = #{id := "123"}},
                Match
            )
        end},

        {"Matches multiple dynamic segments", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/user/:id/post/:post_id", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            [Match] = yaws_appmod_router:find_route(
                "GET", "/user/123/post/456"
            ),
            ?assertMatch(
                #route{
                    path_pattern = "/user/:id/post/:post_id",
                    params = #{id := "123", post_id := "456"}
                },
                Match
            )
        end},

        {"No match for non-existent route", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/exists", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            ?assertEqual(
                [], yaws_appmod_router:find_route("GET", "/not-exists")
            )
        end},

        {"Method mismatch returns no routes", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/method-test", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            ?assertEqual(
                [], yaws_appmod_router:find_route("POST", "/method-test")
            )
        end}
    ]}.

middleware_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Successful middleware chain", fun() ->
            Middlewares = [fun mock_middleware/1],
            Req = make_request("GET", "/test"),
            ?assertMatch(
                {ok, #arg{headers = [{test_middleware, passed} | _]}},
                yaws_appmod_router:execute_middlewares(Middlewares, Req)
            )
        end},

        {"Failing middleware stops chain", fun() ->
            Middlewares = [fun failing_middleware/1, fun mock_middleware/1],
            Req = make_request("GET", "/test"),
            ?assertEqual(
                {error, {html, "<h1>Error: Middleware Failed</h1>"}},
                yaws_appmod_router:execute_middlewares(Middlewares, Req)
            )
        end},

        {"Empty middleware chain succeeds", fun() ->
            Req = make_request("GET", "/test"),
            ?assertEqual(
                {ok, Req},
                yaws_appmod_router:execute_middlewares([], Req)
            )
        end}
    ]}.

appmod_handling_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Successful request handling", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/test", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            Req = make_request("GET", "/test"),
            ?assertEqual(
                {html, "Hello Test"},
                yaws_appmod_router:out(Req)
            )
        end},

        {"404 for unknown route", fun() ->
            Req = make_request("GET", "/unknown"),
            ?assertEqual(
                {html, "<h1>404 Not Found</h1>"},
                yaws_appmod_router:out(Req)
            )
        end},

        {"Middleware error handling", fun() ->
            yaws_appmod_router:add_route(
                "GET",
                "/protected",
                fun mock_handler/1,
                [fun failing_middleware/1]
            ),
            % Give gen_server time to process
            timer:sleep(100),
            Req = make_request("GET", "/protected"),
            ?assertEqual(
                {html, "<h1>Error: Middleware Failed</h1>"},
                yaws_appmod_router:out(Req)
            )
        end},

        {"Route parameters are passed to handler", fun() ->
            ParamHandler = fun(Arg) ->
                Params = Arg#arg.appmoddata,
                UserId = maps:get(id, Params),
                {content, "text/plain", UserId}
            end,
            yaws_appmod_router:add_route(
                "GET",
                "/user/:id",
                ParamHandler,
                []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            Req = make_request("GET", "/user/123"),
            ?assertEqual(
                {content, "text/plain", "123"},
                yaws_appmod_router:out(Req)
            )
        end}
    ]}.

path_matching_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Matches simple static paths", fun() ->
            ?assertMatch(
                {true, #{}}, yaws_appmod_router:match_path("/test", "/test")
            ),
            ?assertEqual(
                false, yaws_appmod_router:match_path("/test", "/other")
            )
        end},

        {"Matches paths with dynamic segments", fun() ->
            ?assertMatch(
                {true, #{id := "123"}},
                yaws_appmod_router:match_path("/user/:id", "/user/123")
            ),
            ?assertMatch(
                {true, #{id := "456", cid := "789"}},
                yaws_appmod_router:match_path(
                    "/post/:id/comment/:cid",
                    "/post/456/comment/789"
                )
            )
        end},

        {"Fails on segment count mismatch", fun() ->
            ?assertEqual(
                false,
                yaws_appmod_router:match_path("/user/:id", "/user/123/extra")
            ),
            ?assertEqual(
                false,
                yaws_appmod_router:match_path("/user/:id/profile", "/user/123")
            )
        end}
    ]}.

server_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Server process is registered", fun() ->
            ?assertNotEqual(undefined, whereis(yaws_appmod_router_server))
        end},

        {"Server maintains ETS table", fun() ->
            ?assertNotEqual(undefined, ets:info(?TABLE_NAME))
        end},

        {"Server handles route additions", fun() ->
            yaws_appmod_router:add_route(
                "GET", "/server-test", fun mock_handler/1, []
            ),
            % Give gen_server time to process
            timer:sleep(100),
            [{route, Route}] = ets:tab2list(?TABLE_NAME),
            ?assertEqual("GET", Route#route.method),
            ?assertEqual("/server-test", Route#route.path_pattern)
        end}
    ]}.
