-module(simple).
-export([start/0]).
-export([
    root/1,
    hello/1,
    user/1,
    login/1,
    auth_middleware/1
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

%% Start Yaws in embedded mode with appmod configurationo
start() ->
    setup_yaws(),
    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(),

    yaws_appmod_router:add_route("GET", "/", fun simple:root/1, []),
    yaws_appmod_router:add_route("GET", "/hello", fun simple:hello/1, []),
    yaws_appmod_router:add_route("GET", "/user/:id", fun simple:user/1, []),
    yaws_appmod_router:add_route("GET", "/login", fun simple:login/1, [
        fun simple:auth_middleware/1
    ]),

    Id = "embedded_yaws",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    SconfList = [
        {port, 8080},
        {servername, "simple_server"},
        {listen, {127, 0, 0, 1}},
        {docroot, Docroot},
        {opaque, #{}},
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

%% Handler demonstrating parameter extraction
user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),
    {content, "text/plain",
        io_lib:format("Hello user ~s! This is your profile page.", [UserId])}.

%% Basic auth middleware
auth_middleware(Arg) ->
    Auth = yaws_api:headers_authorization(yaws_api:arg_headers(Arg)),
    case Auth of
        {"bill", "qwe123", _Orig} ->
            %% Add authenticated flag to opaque
            NewArg = update_opaque(Arg, authenticated, true),
            {ok, NewArg};
        Else ->
            unauthorized_response(Else)
    end.

unauthorized_response(_Else) ->
    Status = {status, 401},
    Headers = [{header, ["WWW-Authenticate: Basic realm=\"My Server\""]}],
    Html = "<html><body><h1>Authentication Failed</h1><p>Please provide valid credentials.</p></body></html>",
    {error, [Status | Headers] ++ [{content, "text/html", Html}]}.


update_opaque(#arg{opaque = OpaqueMap} = Arg, Key, Value) when is_map(OpaqueMap) ->
    Arg#arg{opaque = maps:put(Key, Value, OpaqueMap)}.

%% Protected login route
login(Arg) ->
    case maps:get(authenticated, Arg#arg.opaque, false) of
        true ->
            {content, "text/html",
                "<html><body><h1>Welcome!</h1><p>You have successfully authenticated.</p></body></html>"};
        _ ->
            {content, "text/html",
                "<html><body><h1>Error</h1><p>Authentication required.</p></body></html>"}
    end.
