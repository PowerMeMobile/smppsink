-module(smppsink_id_map).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([next_id/2]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-include_lib("alley_common/include/gen_server_spec.hrl").

-record(counter, {
    key :: {binary(), binary()},
    id :: pos_integer()
}).

-record(st, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec next_id(binary(), binary()) -> pos_integer().
next_id(SystemType, SystemId) ->
    gen_server:call(?MODULE, {next_id, SystemType, SystemId}, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ensure_mnesia_tables(),
    {ok, #st{}}.

terminate(_Reason, _St) ->
    ok.

handle_call({next_id, SystemType, SystemId}, _From, St) ->
    Key = {SystemType, SystemId},
    NextId =
        case mnesia:dirty_update_counter(counter, Key, 1) of
            N when N >= 1 andalso N =< 99999999->
                N;
            _ ->
                mnesia:dirty_write(#counter{key = Key, id = 1}),
                1
        end,
    {reply, NextId, St};

handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.

handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.

handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Intenal
%% ===================================================================

ensure_mnesia_tables() ->
    ok = smppsink_mnesia:ensure_table(counter,
        [{attributes, record_info(fields, counter)},
         {disc_copies, [node()]}]),
    ok = mnesia:wait_for_tables([counter], infinity).
