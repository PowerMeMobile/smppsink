-module(smppsink_commands_performer).

-export([
    perform_commands/2
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").

-type plist() :: [{atom(), term()}].

%% ===================================================================
%% API
%% ===================================================================

-spec perform_commands(plist(), plist()) -> ok | {error, integer()}.
perform_commands([], _Context) ->
    ok;
perform_commands([Command | Commands], Context) ->
    perform_command(Command, Context),
    perform_commands(Commands, Context).

perform_command({reply_submit_status, Status}, Context) ->
    Session = ?gv(session, Context),
    Reply =
        case Status of
            {value, 0} ->
                MsgId = ?gv(msg_id, Context),
                ?log_debug("Reply success (message_id: ~p)", [MsgId]),
                {ok, [{message_id, integer_to_list(MsgId)}]};
            {value, Code} ->
                ?log_debug("Reply failure (status: 0x~8.16.0B, message: \"~s\")",
                    [Code, smpp_error:format(Code)]),
                {error, Code};
            {freq, Freqs} ->
                {ok, Rand} = smppsink_random:uniform(Session),
                case choose_rand_value(Freqs, Rand, 0) of
                    0 ->
                        MsgId = ?gv(msg_id, Context),
                        ?log_debug("Reply success (message_id: ~p)", [MsgId]),
                        {ok, [{message_id, integer_to_list(MsgId)}]};
                    Code ->
                        ?log_debug("Reply failure (status: 0x~8.16.0B, message: \"~s\")",
                            [Code, smpp_error:format(Code)]),
                        {error, Code}
                end
        end,
    SeqNum = ?gv(seq_num, Context),
    gen_mc_session:reply(Session, {SeqNum, Reply});
perform_command({send_deliver_sm, Status}, Context) ->
    Session = ?gv(session, Context),
    RcptValue =
        case Status of
            {value, Value} ->
                Value;
            {freq, Freqs} ->
                {ok, Rand} = smppsink_random:uniform(Session),
                choose_rand_value(Freqs, Rand, "delivered")
        end,
    Message = build_receipt(string:to_lower(RcptValue), Context),
    ?log_debug("Send deliver sm (message: ~p)", [Message]),
    gen_mc_session:deliver_sm(Session, Message);
perform_command({sleep, Time}, _Context) ->
    ?log_debug("Sleep (time: ~p)", [Time]),
    case Time of
        0 ->
            ok;
        infinity ->
            %% TODO: Fix me
            timer:sleep(infinity);
        Time when is_integer(Time) ->
            timer:sleep(1000 * Time)
    end;
perform_command({seed, Seed}, Context) ->
    Session = ?gv(session, Context),
    smppsink_random:seed(Session, Seed),
    ok;
perform_command(nop, _Context) ->
    ok.

choose_rand_value(Freqs, Rand, Default) ->
    Distr = calc_freqs_distribution(Freqs),
    Outcome = ac_lists:findwith(
        fun({_Value, FF, FT}) -> Rand >= FF andalso Rand < FT end, Distr),
    case Outcome of
        {value, {Value, _FF, _FT}} ->
            Value;
        false ->
            Default
    end.

calc_freqs_distribution(Freqs) ->
    {_, Distr} = lists:foldl(
        fun({Code, Freq}, {FreqFrom, Acc}) ->
            FreqTo = FreqFrom + Freq,
            {FreqTo, Acc ++ [{Code, FreqFrom, FreqTo}]}
        end,
        {0, []},
        Freqs
    ),
    Distr.

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
        {source_addr_ton,  DTon},
        {source_addr_npi,  DNpi},
        {source_addr,      DAddr},
        {dest_addr_ton,    STon},
        {dest_addr_npi,    SNpi},
        {destination_addr, SAddr},
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

text_status("enroute")       -> "ENROUTE";
text_status("delivered")     -> "DELIVRD";
text_status("expired")       -> "EXPIRED";
text_status("deleted")       -> "DELETED";
text_status("undeliverable") -> "UNDELIV";
text_status("accepted")      -> "ACCEPTD";
text_status("unknown")       -> "UNKNOWN";
text_status("rejected")      -> "REJECTD";
text_status(Unrecognized)    -> Unrecognized.

int_status("enroute")       -> 1;
int_status("delivered")     -> 2;
int_status("expired")       -> 3;
int_status("deleted")       -> 4;
int_status("undeliverable") -> 5;
int_status("accepted")      -> 6;
int_status("unknown")       -> 7;
int_status("rejected")      -> 8;
int_status(_Unrecognized)   -> 9.
