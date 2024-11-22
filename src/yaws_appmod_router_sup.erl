-module(yaws_appmod_router_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpecs = [
        #{
            id => yaws_appmod_router_server,
            start => {yaws_appmod_router_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [yaws_appmod_router_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
