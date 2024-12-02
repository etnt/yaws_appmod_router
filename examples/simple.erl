-module(simple).
-export([start/0]).
-export([
    root/1,
    hello/1,
    user/1,
    login/1,
    authenticate/1
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

%% Start Yaws in embedded mode with appmod configurationo
start() ->
    setup_yaws(),
    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(),

    %% NOTE: Just a simple route handler with no middleware.
    yaws_appmod_router:add_route("GET", "/", fun simple:root/1, []),
    yaws_appmod_router:add_route("GET", "/hello", fun simple:hello/1, []),

    %% NOTE: Below we will do the authentication in the Middleware,
    %% just before the out/1 route handler is invoked.
    yaws_appmod_router:add_route("GET", "/user/:id", fun simple:user/1, [
        fun simple:authenticate/1
    ]),

    %% NOTE: Below we do the authentication via the auth/2 callback
    yaws_appmod_router:add_route("GET", "/login", fun simple:login/1, []),

    Id = "embedded_yaws",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    Realm = "Simple",
    Type = "Basic",
    Auth = #auth{dir="/",
                docroot=Docroot,
                realm=Realm,
                type=Type,
                headers = ["WWW-Authenticate: "++Type++" realm=\""++Realm++"\"\r\n"],
                mod=yaws_appmod_router},
    OpaqueMap = #{},
    SconfList = [
        {port, 8080},
        {servername, "simple_server"},
        {listen, {127, 0, 0, 1}},
        {docroot, Docroot},
        %% NOTE: We enable auth/2 callback handling here.
        {opaque, yaws_appmod_router:enable_auth(OpaqueMap)},
        {auth, Auth},
        {appmods, [{"/", yaws_appmod_router}]}
    ],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    [supervisor:start_child(yaws_sup, Ch) || Ch <- ChildSpecs],

    ok = yaws_api:setconf(GC, SCList).


setup_yaws() ->
    application:load(yaws),
    %% When Yaws is started in embedded mode, it expects its configuration
    %% through the function call yaws_api:setconf/N.
    application:set_env(yaws, embedded, true).

%%% ----------------------------------------------------------------------------
%%% Route handler functions
%%% ----------------------------------------------------------------------------
%%%
%%% NOTE: Since we have enabled auth/2 callback handling, we have to handle
%%% both the Yaws auth/2 callback and the out/1 callback.
%%%
%%% Here we are sloppy and just handle the out/1 callback by checking
%%% for an #arg{} record. The auth/2 callback will call this function
%%% with a tuple {Arg, Auth} resulting in a crash here, which is then
%%% ignored by the yaws_appmod_router, which in its turn will continue with
%%% the out/1 callback that will invoke this function, this time with just
%%% the #arg{} record.
%%%
%%% ----------------------------------------------------------------------------

root(Arg) when is_record(Arg, arg) ->
    {content, "text/plain", "This is the '/' route !"}.

hello(Arg) when is_record(Arg, arg) ->
    {content, "text/plain", "Hello this is the '/hello' route !"}.

%% Handler demonstrating parameter extraction
%% NOTE: We check that the user has been authenticated by the Middleware.
user(#arg{opaque     = OpaqueMap,
          appmoddata = Params} = _Arg) ->
    case maps:get(authenticated, OpaqueMap, false) of
        false ->
            %% NOTE: We won't really end up here since our Middleware will
            %% return an error response if the user is not authenticated.
            %% But in case it would mal-function we return a generic error.
            [{status, 401},
             {content, "text/html", "<h1>Error</h1><p>Authentication required.</p>"}];
        true ->
            UserId = maps:get(id, Params),
            {content, "text/plain",
                io_lib:format("Hello user ~s! This is your profile page.", [UserId])}
    end.

%% Login route, protected by auth/2 callback
login(#arg{} = _Arg) ->
    {content, "text/html",
        "<html><body><h1>Welcome!</h1><p>You have successfully authenticated.</p></body></html>"};
%%
login({Arg, _Auth}) ->
    %% NOTE: This is executed during the auth/2 callback, so we can only
    %% return values that are valid according to the Yaws auth/2 callback.
    case authenticate(Arg) of
        {ok, _} -> true;
        _       -> false
    end.


%%% ----------------------------------------------------------------------------
%%% Authentication middleware, runs before the out/1 route handler is invoked
%%% ----------------------------------------------------------------------------

authenticate(Arg) ->
    Auth = yaws_api:headers_authorization(yaws_api:arg_headers(Arg)),
    case Auth of
        {"bill", "qwe123", _Orig} ->
            %% Add authenticated flag to opaque
            NewArg = update_opaque(Arg, authenticated, true),
            {ok, NewArg};
        Else ->
            unauthorized_response(Else)
    end.

%%% ----------------------------------------------------------------------------
%%% Helpers
%%% ----------------------------------------------------------------------------

unauthorized_response(_Else) ->
    Status = {status, 401},
    Headers = [{header, ["WWW-Authenticate: Basic realm=\"My Server\""]}],
    Html = "<html><body><h1>Authentication Failed</h1><p>Please provide valid credentials.</p></body></html>",
    {error, [Status | Headers] ++ [{content, "text/html", Html}]}.

update_opaque(#arg{opaque = OpaqueMap} = Arg, Key, Value) when is_map(OpaqueMap) ->
    Arg#arg{opaque = maps:put(Key, Value, OpaqueMap)}.
