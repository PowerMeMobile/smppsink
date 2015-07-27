-module(smppsink_random).

%% API
-export([
    start_link/0,
    seed/2,
    uniform/1,
    delete/1,
    clear/0
]).

%% Service API
-export([
    get_all/0
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

-include_lib("alley_common/include/gen_server_spec.hrl").

-type key()  :: term().
-type seed() :: {integer(), integer(), integer()}.

-record(st, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec seed(key(), seed()) -> ok | {error, already_exists}.
seed(Key, Seed) ->
    gen_server:call(?MODULE, {seed, Key, Seed}).

-spec uniform(key()) -> {ok, float()} | {error, not_found}.
uniform(Key) ->
    gen_server:call(?MODULE, {uniform, Key}).

-spec delete(key()) -> ok.
delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

-spec clear() -> ok.
clear() ->
    gen_server:cast(?MODULE, clear).

%% ===================================================================
%% Service API
%% ===================================================================

-spec get_all() -> [{key(), seed()}].
get_all() ->
    ets:tab2list(?MODULE).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {ok, #st{}}.

handle_call({seed, Key, Seed}, _From, St = #st{}) ->
    Reply =
        case ets:lookup(?MODULE, Key) of
            [] ->
                true = ets:insert(?MODULE, {Key, Seed}),
                ok;
            [{Key, _Value}] ->
                {error, already_exists}
        end,
    {reply, Reply, St};
handle_call({uniform, Key}, _From, St = #st{}) ->
    Reply =
        case ets:lookup(?MODULE, Key) of
            [] ->
                {error, not_found};
            [{Key, Seed}] ->
                {Rand, Seed2} = random:uniform_s(Seed),
                true = ets:insert(?MODULE, {Key, Seed2}),
                {ok, Rand}
        end,
    {reply, Reply, St};
handle_call(Request, _From, St = #st{}) ->
    {stop, {bad_arg, Request}, St}.

handle_cast({delete, Key}, St = #st{}) ->
    true = ets:delete(?MODULE, Key),
    {noreply, St};
handle_cast(clear, St = #st{}) ->
    true = ets:delete_all_objects(?MODULE),
    {noreply, St};
handle_cast(Request, St = #st{}) ->
    {stop, {bad_arg, Request}, St}.

handle_info(Message, St = #st{}) ->
    {stop, {bad_arg, Message}, St}.

terminate(_Reason, #st{}) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
