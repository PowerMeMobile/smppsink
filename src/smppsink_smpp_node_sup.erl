-module(smppsink_smpp_node_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_node/1]).
-export([init/1]).

-include_lib("alley_common/include/supervisor_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_node(port()) -> {ok, pid()}.
start_node(LSock) ->
    supervisor:start_child(?MODULE, [LSock]).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [
        {node_sup, {smppsink_smpp_node, start_link, []},
            temporary, 10000, worker, [smppkink_smpp_node]}
    ]}}.
