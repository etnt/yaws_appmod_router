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

-import(yaws_appmod_router, [is_http_method/1, is_crud/1]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("yaws_appmod_router.hrl").

-define(TABLE_NAME, yaws_appmod_routes).


%% ------------------------------------------------------------------
%% API FUNCTIONS
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_route(Method, PathPattern, Handler, Middlewares) 
    when is_list(Method) andalso is_list(PathPattern) andalso
            is_function(Handler, 1) andalso is_list(Middlewares) ->
    case {is_http_method(Method), is_crud(Method)} of
        {true, false} ->
            gen_server:cast(?MODULE, {add_route, Method, PathPattern, Handler, Middlewares});
        {false, true} ->
            gen_server:cast(?MODULE, {add_crud_route, Method, PathPattern, Handler, Middlewares});
        _ ->
            {error, invalid_method}
    end;
add_route(_, _, _, _) ->
    {error, invalid_arguments}.


%% ------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%% ------------------------------------------------------------------
init([]) ->
    % Create ETS table with protected access - only this process can write
    ets:new(?TABLE_NAME, [named_table, protected, ordered_set, {keypos, 2}]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_route, Method, PathPattern, Handler, Middlewares}, State) ->
    insert_route(Method, PathPattern, Handler, Middlewares),
    {noreply, State};
%%
handle_cast({add_crud_route, Method, PathPattern, Handler, Middlewares}, State) ->
    add_crud_routes(Method, PathPattern, Handler, Middlewares),
    {noreply, State};
%%
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ------------------------------------------------------------------

add_crud_routes(Crud, PathPattern, Handler, Middlewares) ->
    lists:foreach(
        fun($C) ->
            insert_route("POST", PathPattern, Handler, Middlewares, Crud, create);
            ($R) ->
                insert_route("GET", PathPattern, Handler, Middlewares, Crud, index),
                insert_route("GET", PathPattern++"/:id", Handler, Middlewares, Crud, show);
            ($U) ->
                insert_route("PUT", PathPattern++"/:id", Handler, Middlewares, Crud, replace),
                insert_route("PATCH", PathPattern++"/:id", Handler, Middlewares, Crud, modify);
            ($D) ->
                insert_route("DELETE", PathPattern++"/:id", Handler, Middlewares, Crud, delete)
        end, Crud),
    insert_route("OPTIONS", PathPattern, undefined, [], Crud, options).

insert_route(Method, PathPattern, Handler, Middlewares) ->
    insert_route(Method, PathPattern, Handler, Middlewares, _Crud = undefined, _Action = undefined).

insert_route(Method, PathPattern, Handler, Middlewares, Crud, Action) ->
    Route = #route{
        method = Method,
        crud = Crud,
        action = Action,
        path_pattern = PathPattern,
        handler = Handler,
        middlewares = Middlewares
    },
    ets:insert(?TABLE_NAME, {route, Route}).
