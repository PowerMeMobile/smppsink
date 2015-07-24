-module(smppsink_store).

%% API
-export([
    start_link/0,
    set/2,
    new/2,
    get/1,
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

-type key()   :: term().
-type value() :: term().

-record(st, {}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set(key(), value()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

-spec new(key(), value()) -> ok | {error, already_exists}.
new(Key, Value) ->
    gen_server:call(?MODULE, {new, Key, Value}).

-spec get(key()) -> {ok, value()} | {error, no_entry}.
get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            {error, no_entry};
        [{Key, Value}] ->
            {ok, Value}
    end.

-spec delete(key()) -> ok.
delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

-spec clear() -> ok.
clear() ->
    gen_server:cast(?MODULE, clear).

%% ===================================================================
%% Service API
%% ===================================================================

-spec get_all() -> [{key(), value()}].
get_all() ->
    ets:tab2list(?MODULE).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ?MODULE = ets:new(?MODULE, [named_table]),
    {ok, #st{}}.

handle_call({set, Key, Value}, _From, St = #st{}) ->
    true = ets:insert(?MODULE, {Key, Value}),
    {reply, ok, St};
handle_call({new, Key, Value}, _From, St = #st{}) ->
    Reply =
        case ets:lookup(?MODULE, Key) of
            [] ->
                true = ets:insert(?MODULE, {Key, Value}),
                ok;
            [{Key, _Value}] ->
                {error, already_exists}
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
