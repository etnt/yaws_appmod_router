-module(simple).
-export([start/0]).
-export([root/1
        , hello/1]).

%% Start Yaws in embedded mode with appmod configuration
start() ->
    setup_yaws(),
    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(),

    yaws_appmod_router:add_route("GET", "/", fun simple:root/1, []),
    yaws_appmod_router:add_route("GET", "/hello", fun simple:hello/1, []),

    Id = "embedded_yaws",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    SconfList = [
        {port, 8080},
        {servername, "simple_server"},
        {listen, {127, 0, 0, 1}},
        {docroot, Docroot},
        {appmods, [{"/", yaws_appmod_router}]}
    ],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    [supervisor:start_child(yaws_sup, Ch) || Ch <- ChildSpecs],

    ok = yaws_api:setconf(GC, SCList),

    io:format("Yaws server started on port 8080~n"),
    ok.

setup_yaws() ->
    application:load(yaws),
    %% When Yaws is started in embedded mode, it expects its configuration
    %% through the function call yaws_api:setconf/N.
    application:set_env(yaws, embedded, true).

%% Appmod handler function
root(_Arg) ->
    {content, "text/plain", "This is the '/' route !"}.

hello(_Arg) ->
    {content, "text/plain", "Hello this is the '/hello' route !"}.

