-module(smppsink_smpp_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([accepted/0]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-include("otp_records.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("oserl/include/oserl.hrl").

-record(st, {
    lsock :: port(),
    node :: pid(),
    node_mref :: reference()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec accepted() -> ok.
accepted() ->
    gen_server:call(?MODULE, accepted, infinity).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    Addr = smppsink_app:get_env(addr),
    Port = smppsink_app:get_env(port),
    CACertFile = smppsink_app:get_env(cacertfile),
    CertFile = smppsink_app:get_env(certfile),
    KeyFile = smppsink_app:get_env(keyfile),
    ?log_info("Starting SMPP server (addr: ~s, port: ~w)",
        [inet_parse:ntoa(Addr), Port]),
    ListenOpts = [
        {addr, Addr},
        {port, Port},
        {cacertfile, CACertFile},
        {certfile, CertFile},
        {keyfile, KeyFile}
    ],
    case smpp_session:listen(ListenOpts) of
        {ok, LSock} ->
            {ok, start_new_node(#st{lsock = LSock})};
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(_Reason, St) ->
    smppsink_smpp_node:stop(St#st.node),
    gen_tcp:close(St#st.lsock).

handle_call(accepted, _From, St) ->
    demonitor(St#st.node_mref, [flush]),
    {reply, ok, start_new_node(St)};

handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.

handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.

handle_info(#'EXIT'{pid = Pid}, #st{lsock = Pid} = St) ->
    {stop, lsock_closed, St};

handle_info(#'DOWN'{pid = Pid}, #st{node = Pid} = St) ->
    {noreply, start_new_node(St)};

handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% Internal
%% ===================================================================

start_new_node(St) ->
    {ok, Node} = smppsink_smpp_node_sup:start_node(St#st.lsock),
    MRef = monitor(process, Node),
    St#st{node = Node, node_mref = MRef}.
