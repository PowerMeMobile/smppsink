-module(smppsink_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Restart, Shutdown, Type),
    {Mod, {Mod, start_link, []}, Restart, Shutdown, Type, [Mod]}).


%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(smppsink_id_map, permanent, 10000, worker),
        ?CHILD(smppsink_store, permanent, 5000, worker),
        ?CHILD(smppsink_smpp_node_sup, permanent, 5000, supervisor),
        ?CHILD(smppsink_smpp_server, permanent, 10000, worker)
    ]}}.
