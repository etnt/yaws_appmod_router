-module(yaws_appmod_router_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_route/5,
    create_table/1,
    get_tables/0
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

-record(state, {
    tables = []  :: [atom()]
}).


%% ------------------------------------------------------------------
%% API FUNCTIONS
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_route(TableName, Method, PathPattern, Handler, Middlewares) 
    when is_list(Method) andalso is_list(PathPattern) andalso
            is_function(Handler, 1) andalso is_list(Middlewares) ->
    case {is_http_method(Method), is_crud(Method)} of
        {true, false} ->
            gen_server:call(?MODULE, {add_route, TableName, Method, PathPattern, Handler, Middlewares});
        {false, true} ->
            gen_server:call(?MODULE, {add_crud_route, TableName, Method, PathPattern, Handler, Middlewares});
        _ ->
            {error, invalid_method}
    end;
add_route(_, _, _, _, _) ->
    {error, invalid_arguments}.

create_table(TableName) ->
    gen_server:call(?MODULE, {create_table, TableName}).

get_tables() ->
    gen_server:call(?MODULE, get_tables).


%% ------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%% ------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


handle_call({add_route, TableName, Method, PathPattern, Handler, Middlewares}, _From, State) ->
    insert_route(TableName, Method, PathPattern, Handler, Middlewares),
    {reply, ok, State};
%%
handle_call({add_crud_route, TableName, Method, PathPattern, Handler, Middlewares}, _From, State) ->
    add_crud_routes(TableName, Method, PathPattern, Handler, Middlewares),
    {reply, ok, State};
    %%
handle_call({create_table, TableName}, _From, #state{tables = Tables} = State) ->
    case ets:info(?TABLE_NAME, named_table) of
        undefined ->
            %% Create ETS table with protected access - only this process can write
            Tref = ets:new(TableName, [named_table, protected, ordered_set, {keypos, 2}]),
            {reply, {ok, Tref}, State#state{tables = [Tref|Tables]}};
        _ ->
            {reply, {error, <<"table_already_exists">>}, State}
    end;
%%
handle_call(get_tables, _From, #state{tables = Tables} = State) ->
    {reply, {ok, Tables}, State};
    %%
handle_call(_Request, _From, State) ->
    {reply, {error, <<"unknown_call">>}, State}.


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

add_crud_routes(TableName, Crud, PathPattern, Handler, Middlewares) ->
    lists:foreach(
        fun($C) ->
            insert_route(TableName, "POST", PathPattern, Handler, Middlewares, Crud, create);
            ($R) ->
                insert_route(TableName, "GET", PathPattern, Handler, Middlewares, Crud, index),
                insert_route(TableName, "GET", PathPattern++"/:id", Handler, Middlewares, Crud, show);
            ($U) ->
                insert_route(TableName, "PUT", PathPattern++"/:id", Handler, Middlewares, Crud, replace),
                insert_route(TableName, "PATCH", PathPattern++"/:id", Handler, Middlewares, Crud, modify);
            ($D) ->
                insert_route(TableName, "DELETE", PathPattern++"/:id", Handler, Middlewares, Crud, delete)
        end, Crud),
    insert_route(TableName, "OPTIONS", PathPattern, undefined, [], Crud, options).

insert_route(TableName, Method, PathPattern, Handler, Middlewares) ->
    insert_route(TableName, Method, PathPattern, Handler, Middlewares, _Crud = undefined, _Action = undefined).

insert_route(TableName, Method, PathPattern, Handler, Middlewares, Crud, Action) ->
    Route = #route{
        method = Method,
        crud = Crud,
        action = Action,
        path_pattern = PathPattern,
        handler = Handler,
        middlewares = Middlewares
    },
    ets:insert(TableName, {route, Route}).
