-module(smppsink_smpp_node).

-behaviour(gen_server).
-behaviour(gen_mc_session).

%% API
-export([
    start_link/1,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

%% gen_mc_session callbacks
-export([
    handle_accept/2,
    handle_bind/2,
    handle_closed/2,
    handle_enquire_link/2,
    handle_operation/2,
    handle_resp/3,
    handle_unbind/2
]).

-include("otp_records.hrl").
-include_lib("alley_common/include/gen_server_spec.hrl").
-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").
-include_lib("oserl/include/oserl.hrl").

-record(st, {
    mc_session :: pid(),
    smpp_log_mgr :: pid(),
    addr :: string(),
    system_type :: binary(),
    system_id :: binary(),
    version :: pos_integer(),
    uuid :: string(),
    is_bound = false :: boolean()
}).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link/1 :: (port()) -> {ok, pid()}.
start_link(LSock) ->
    gen_server:start_link(?MODULE, LSock, []).

-spec stop/1 :: (pid()) -> no_return().
stop(Node) ->
    gen_server:cast(Node, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(LSock) ->
    process_flag(trap_exit, true),
    ?log_debug("Node: initializing", []),
    {ok, SMPPLogMgr} = smpp_log_mgr:start_link(),
    pmm_smpp_logger_h:sup_add_to_manager(SMPPLogMgr),
    Timers = ?TIMERS(
        smppsink_app:get_env(session_init_time),
        smppsink_app:get_env(enquire_link_time),
        smppsink_app:get_env(inactivity_time),
        smppsink_app:get_env(response_time)
    ),
    {ok, Session} = gen_mc_session:start_link(?MODULE, [
        {log, SMPPLogMgr}, {lsock, LSock}, {timers, Timers}
    ]),
    {ok, #st{smpp_log_mgr = SMPPLogMgr, mc_session = Session}}.

terminate(_Reason, St) ->
    ok = smppsink_random:delete(St#st.mc_session),
    catch(gen_mc_session:stop(St#st.mc_session)),
    catch(pmm_smpp_logger_h:deactivate(St#st.smpp_log_mgr)),
    catch(smpp_log_mgr:stop(St#st.smpp_log_mgr)).

handle_call({handle_accept, Addr}, _From, St) ->
    ?log_info("Accepted connection (addr: ~s)", [Addr]),
    Uuid = binary_to_list(uuid:unparse(uuid:generate())),
    smppsink_smpp_server:accepted(),
    {reply, ok, St#st{addr = Addr, uuid = Uuid}};

handle_call({handle_bind, BindType, SystemType, SystemId, Password, Version}, From, St) ->
    PduLogName = pdu_log_name(BindType, SystemType, SystemId, St#st.uuid),
    case smppsink_app:get_env(log_smpp_pdus) of
        true ->
            LogParams = [{base_dir, smppsink_app:get_env(smpp_pdu_log_dir)},
                         {base_file_name, PduLogName},
                         {max_size, smppsink_app:get_env(file_log_size)}],
            pmm_smpp_logger_h:activate(St#st.smpp_log_mgr, LogParams);
        false ->
            ok
    end,
    case authenticate(BindType, SystemType, SystemId, Password) of
        ok ->
            ?log_info("Granted bind "
                "(bind_type: ~p, system_type: ~s, system_id: ~s, password: ~s)",
                [BindType, SystemType, SystemId, Password]),

            NameAndVsn = lists:sublist(
                lists:flatten([
                    smppsink_app:get_env(server_name), " ",
                    smppsink_app:get_env(vsn)]),
                15), %% bind resp system_id length
            Params = [{system_id,  NameAndVsn}],
            NewSt = St#st{
                is_bound = true,
                system_type = list_to_binary(SystemType),
                system_id = list_to_binary(SystemId),
                version = Version
            },
            Reply = {ok, Params},
            case application:get_env(smppsink, postpone_bind_resp, 0) of
                Seconds when is_integer(Seconds) andalso Seconds > 0 ->
                    timer:send_after(Seconds*1000, {send_postponed_bind_reply, From, Reply}),
                    {noreply, NewSt};
                _ ->
                    {reply, Reply, NewSt}
            end;
        {error, ErrorCode} ->
            ?log_info("Denied bind "
                "(bind_type: ~p, system_type: ~s, system_id: ~s, password: ~s) (~s)",
                [BindType, SystemType, SystemId, Password, smpp_error:format(ErrorCode)]),
            {reply, {error, ErrorCode}, St}
    end;

handle_call(Request, _From, St) ->
    {stop, {unexpected_call, Request}, St}.

handle_cast({handle_operation, submit_sm, SeqNum, Params}, St) ->
    handle_submit_sm(SeqNum, Params, St),
    {noreply, St};

handle_cast({handle_operation, _Cmd, SeqNum, _Params}, St) ->
    Reply = {error, ?ESME_RPROHIBITED},
    gen_mc_session:reply(St#st.mc_session, {SeqNum, Reply}),
    {noreply, St};

handle_cast({handle_resp, Resp, _Ref}, St) ->
    Reply =
         case Resp of
            {ok, {_CmdId, _Status, _SeqNum, Body}} ->
                {ok, Body};
            {error, {command_status, Status}} ->
                {error, smpp_error:format(Status)};
            {error, Status} ->
                {error, smpp_error:format(Status)}
         end,
    ?log_debug("resp: ~p", [Reply]),
    {noreply, St};

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast({handle_closed, closed}, St) ->
    {stop, closed, St};

handle_cast({handle_closed, Reason}, St) ->
    {stop, {closed, Reason}, St};

handle_cast(handle_unbind, St) ->
    {stop, unbound, St};

handle_cast(Request, St) ->
    {stop, {unexpected_cast, Request}, St}.

handle_info({send_postponed_bind_reply, From, Reply}, St = #st{}) ->
    gen_server:reply(From, Reply),
    {noreply, St};
handle_info(#'EXIT'{pid = Pid, reason = Reason}, #st{mc_session = Pid} = St) ->
    {stop, {session_exit, Reason}, St};

handle_info(Info, St) ->
    {stop, {unexpected_info, Info}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% ===================================================================
%% gen_mc_session callbacks
%% ===================================================================

%% Called when a new connection from Addr arrives to a listening session.
%% If 'ok' is returned, then the connection is accepted and the session
%% moves to open state.
handle_accept(Node, Addr) ->
    gen_server:call(Node, {handle_accept, inet_parse:ntoa(Addr)}).

%% Called upon receiving a bind_* request.
%% If {ok, Params} is returned, then session goes to the bound state.
%% If {error, Reason} is returned, the session remains in the open state
%% until session_init_timer or inactivity_timer fires or a successful bind
%% request happens.
handle_bind(Node, {Cmd, {_, _, _, Params}}) ->
    BindType =
        case Cmd of
            bind_transmitter -> tx;
            bind_receiver    -> rx;
            bind_transceiver -> trx
        end,
    SystemType = ?gv(system_type, Params),
    SystemId = ?gv(system_id, Params),
    Password = ?gv(password, Params),
    Version  = ?gv(interface_version, Params),
    gen_server:call(Node,
        {handle_bind, BindType, SystemType, SystemId, Password, Version}, infinity).

%% Deliver an async repsonse to the request with Ref.
handle_resp(Node, Resp, Ref) ->
    gen_server:cast(Node, {handle_resp, Resp, Ref}).

%% Handle ESME-issued operation.
handle_operation(Node, {Cmd, {_, _, SeqNum, Params}}) ->
    %% probably redundant message
    gen_server:cast(Node, {handle_operation, Cmd, SeqNum, Params}),
    noreply.

%% Forward enquire_link operations (from the peer MC) to the callback module.
handle_enquire_link(_Node, _Pdu) ->
    ok.

%% Handle ESME-issued unbind.
handle_unbind(Node, _Pdu) ->
    gen_server:cast(Node, handle_unbind).

%% Notify Node of the Reason before stopping the session.
handle_closed(Node, Reason) ->
    gen_server:cast(Node, {handle_closed, Reason}).

handle_submit_sm(SeqNum, Params, St) ->
    ?log_debug("Got submit_sm: ~p", [Params]),
    spawn(fun() -> submit_sm_step(submit, {SeqNum, Params}, St) end).

%% ===================================================================
%% Internal
%% ===================================================================

pdu_log_name(BindType, SystemType, SystemId, Uuid) ->
    lists:flatten(io_lib:format("~s-cid~s-~s-~s.log",
        [BindType, SystemType, SystemId, Uuid])).

authenticate(BindType, SystemType, SystemId, Password) ->
    authenticate_step(verify_account, BindType, SystemType, SystemId, Password).

authenticate_step(verify_account, BindType, SystemType, SystemId, Password) ->
    Check = fun(Expected, Key, Params) ->
        Expected =:= proplists:get_value(Key, Params)
    end,
    Filter = fun({account, Params}) ->
        Check(SystemType, system_type, Params) andalso
        Check(SystemId, system_id, Params) andalso
        Check(Password, password, Params)
    end,
    Accounts = smppsink_app:get_env(accounts),
    case lists:any(Filter, Accounts) of
        true ->
            authenticate_step(register, BindType, SystemType, SystemId, Password);
        false ->
            {error, ?ESME_RBINDFAIL}
    end;

authenticate_step(register, BindType, SystemType, SystemId, _Password) ->
    RegName = {connection, BindType, SystemType, SystemId},
    try gproc:reg({n, l, RegName}) of
        true ->
            ok
    catch
        error:badarg ->
            {error, ?ESME_RALYBND}
    end.

submit_sm_step(submit, {SeqNum, Params}, St) ->
    MsgId = smppsink_id_map:next_id(St#st.system_type, St#st.system_id),
    Session = St#st.mc_session,
    Context = [
        {seq_num, SeqNum},
        {msg_id, MsgId},
        {session, Session},
        {system_type, St#st.system_type},
        {system_id, St#st.system_id},
        {version, St#st.version}
        | Params
    ],
    Commands = smppsink_commands_builder:build_commands(Context),
    smppsink_commands_performer:perform_commands(Commands, Context).
