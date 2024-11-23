-module(yaws_appmod_router_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_route/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").

-define(TABLE_NAME, yaws_appmod_routes).



%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_route(Method, PathPattern, Handler, Middlewares) ->
    gen_server:cast(?MODULE, {add_route, Method, PathPattern, Handler, Middlewares}).

%% gen_server callbacks
init([]) ->
    % Create ETS table with protected access - only this process can write
    ets:new(?TABLE_NAME, [named_table, protected, set, {keypos, 2}]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_route, Method, PathPattern, Handler, Middlewares}, State) ->
    Route = #route{
        method = Method,
        path_pattern = PathPattern,
        handler = Handler,
        middlewares = Middlewares
    },
    ets:insert(?TABLE_NAME, {route, Route}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
