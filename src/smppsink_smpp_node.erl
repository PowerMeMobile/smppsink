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

-define(gv(K, L), element(2, lists:keyfind(K, 1, L))).

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
    catch(gen_mc_session:stop(St#st.mc_session)),
    catch(pmm_smpp_logger_h:deactivate(St#st.smpp_log_mgr)),
    catch(smpp_log_mgr:stop(St#st.smpp_log_mgr)).

handle_call({handle_accept, Addr}, _From, St) ->
    ?log_info("Accepted connection (addr: ~s)", [Addr]),
    Uuid = binary_to_list(uuid:unparse(uuid:generate())),
    smppsink_smpp_server:accepted(),
    {reply, ok, St#st{addr = Addr, uuid = Uuid}};

handle_call({handle_bind, BindType, SystemType, SystemId, Password, Version}, _From, St) ->
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

            Params = [{system_id, smppsink_app:get_env(server_name)}],
            {reply, {ok, Params}, St#st{
                is_bound = true,
                system_type = list_to_binary(SystemType),
                system_id = list_to_binary(SystemId),
                version = Version
            }};
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
    submit_sm_step(submit, {SeqNum, Params}, St).

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
    Context = [
        {seq_num, SeqNum},
        {msg_id, MsgId},
        {session, St#st.mc_session},
        {system_type, St#st.system_type},
        {system_id, St#st.system_id},
        {version, St#st.version}
        | Params
    ],
    Commands = build_commands(Context),
    perform_commands(Commands, Context).

build_commands(Context) ->
    Message = ?gv(short_message, Context),
    case yamerl_constr:string(Message) of
        [Message] ->
            ?log_error("Invalid commands: ~p. Proceed as normal message", [Message]),
            default_commands(Context);
        [Plist] ->
            add_default_commands(handle_commands(Plist, Context), Context)
    end.

handle_commands(Commands, Context) ->
    handle_commands(Commands, Context, []).

handle_commands([], _Context, Acc) ->
    Acc;
handle_commands([{Key, Value} | Plist], Context, Acc) ->
    case parse_command({Key, Value}, Context) of
        {ok, Commands} ->
            handle_commands(Plist, Context, Acc ++ Commands);
        error ->
            default_commands(Context)
    end.

default_commands(Context) ->
    add_default_commands([], Context).

add_default_commands(Commands, Context) ->
    case proplists:get_value(reply_submit_status, Commands) of
        undefined ->
            add_default_commands([{reply_submit_status, 0} | Commands], Context);
        0 ->
            case ?gv(registered_delivery, Context) of
                0 ->
                    Commands;
                _ ->
                    case proplists:get_value(send_deliver_sm, Commands) of
                        undefined ->
                            Message = build_receipt("delivered", Context),
                            Commands ++ [{send_deliver_sm, Message}];
                        _ ->
                            Commands
                    end
            end;
        _ ->
            Commands
    end.

parse_command({"submit", Status}, _Context) when is_integer(Status) ->
    {ok, [{reply_submit_status, Status}]};
parse_command({"submit", Plist}, _Context) when is_list(Plist) ->
    Status = proplists:get_value("status", Plist, 0),
    Timeout = proplists:get_value("timeout", Plist, 0),
    {ok, [{sleep, Timeout}, {reply_submit_status, Status}]};
parse_command({"receipt", Status}, Context) ->
    case ?gv(registered_delivery, Context) of
        0 ->
            {ok, [nop]};
        _ ->
            case proplists:get_keys(Status) of
                [] ->
                    %% only status is given.
                    Message = build_receipt(string:to_lower(Status), Context),
                    {ok, [{send_deliver_sm, Message}]};
                _ ->
                    Status2 = proplists:get_value("status", Status, "delivered"),
                    Timeout = proplists:get_value("timeout", Status, 0),
                    Message = build_receipt(string:to_lower(Status2), Context),
                    {ok, [{sleep, Timeout}, {send_deliver_sm, Message}]}
            end
    end;
parse_command({Command, null}, _Context) ->
    ?log_error("Invalid command: ~p. Proceed as normal message", [Command]),
    error;
parse_command(Command, _Context) ->
    ?log_error("Unknown command: ~p. Proceed as normal message", [Command]),
    error.

perform_commands([], _Context) ->
    ok;
perform_commands([Command | Commands], Context) ->
    perform_command(Command, Context),
    perform_commands(Commands, Context).

perform_command({reply_submit_status, Status}, Context) ->
    Reply =
        case Status of
            0 ->
                MsgId = ?gv(msg_id, Context),
                ?log_debug("Reply success (message_id: ~p)", [MsgId]),
                {ok, [{message_id, integer_to_list(MsgId)}]};
            _ ->
                ?log_debug("Reply failure (status: 0x~8.16.0B, message: \"~s\")", [Status, smpp_error:format(Status)]),
                {error, Status}
        end,
    Session = ?gv(session, Context),
    SeqNum = ?gv(seq_num, Context),
    gen_mc_session:reply(Session, {SeqNum, Reply});
perform_command({send_deliver_sm, Message}, Context) ->
    ?log_debug("Send deliver sm (message: ~p)", [Message]),
    Session = ?gv(session, Context),
    gen_mc_session:deliver_sm(Session, Message);
perform_command({sleep, Timeout}, _Context) when is_integer(Timeout) ->
    ?log_debug("Sleep (timeout: ~p)", [Timeout]),
    timer:sleep(1000 * Timeout);
perform_command(nop, _Context) ->
    ok.

build_receipt(Status, Context) ->
    STon = ?gv(source_addr_ton, Context),
    SNpi = ?gv(source_addr_npi, Context),
    SAddr = ?gv(source_addr, Context),
    DTon = ?gv(dest_addr_ton, Context),
    DNpi = ?gv(dest_addr_npi, Context),
    DAddr = ?gv(destination_addr, Context),

    UTCDate = binary_to_list(
        ac_datetime:timestamp_to_utc_string(os:timestamp())),
    SubmitDate = UTCDate,
    DoneDate = UTCDate,
    Message = ?gv(short_message, Context),
    MsgId = ?gv(msg_id, Context),
    Version = ?gv(version, Context),
    Message2 = lists:concat([
        "id:",           MsgId,
        " submit date:", lists:sublist(SubmitDate, 10),
        " done date:",   lists:sublist(DoneDate, 10),
        " stat:",        text_status(Status),
        " text:",        lists:sublist(Message, 20)
    ]),
    Params33 = [
        {source_addr_ton,  STon},
        {source_addr_npi,  SNpi},
        {source_addr,      SAddr},
        {dest_addr_ton,    DTon},
        {dest_addr_npi,    DNpi},
        {destination_addr, DAddr},
        {data_coding,      0},
        {short_message,    Message2},
        {esm_class,        4}
    ],

    case Version of
        16#33 ->
            Params33;
        _ ->
            [
                {receipted_message_id, integer_to_list(MsgId)},
                {message_state, int_status(Status)}
                | Params33
            ]
    end.

%% ===================================================================
%% Receipt statuses
%% ===================================================================

text_status("unroute")       -> "UNROUTE";
text_status("delivered")     -> "DELIVRD";
text_status("expired")       -> "EXPIRED";
text_status("deleted")       -> "DELETED";
text_status("undeliverable") -> "UNDELIV";
text_status("accepted")      -> "ACCEPTD";
text_status("unknown")       -> "UNKNOWN";
text_status("rejected")      -> "REJECTD";
text_status(Unknown)         -> Unknown.

int_status("unroute")       -> 1;
int_status("delivered")     -> 2;
int_status("expired")       -> 3;
int_status("deleted")       -> 4;
int_status("undeliverable") -> 5;
int_status("accepted")      -> 6;
int_status("unknown")       -> 7;
int_status("rejected")      -> 8;
int_status(_Unknown)        -> 0.
