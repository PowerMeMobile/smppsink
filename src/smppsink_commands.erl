-module(smppsink_commands).

-export([
    build_commands/1
]).

-include_lib("alley_common/include/logging.hrl").
-include_lib("alley_common/include/utils.hrl").

-type plist() :: [{atom(), term()}].

%% ===================================================================
%% API
%% ===================================================================

-spec build_commands(plist()) -> plist().
build_commands(Context) ->
    Message = ?gv(short_message, Context),
    case try_parse_commands(Message) of
        {ok, Commands} ->
            add_default_commands(handle_commands(Commands, Context), Context);
        error ->
            ?log_debug("Invalid commands: ~p. Proceed as normal message", [Message]),
            default_commands(Context)
    end.

%% ===================================================================
%% Internal
%% ===================================================================

try_parse_commands(Message) ->
    case try_fix_user_input(Message) of
        {ok, Message2} ->
            try yamerl_constr:string(Message2) of
                [] ->
                    error;
                [Message2] ->
                    error;
                [Commands] ->
                    {ok, Commands}
            catch
                _:_ ->
                    error
            end;
        error ->
            error
    end.

try_fix_user_input(Message) ->
    Message2 = re:replace(Message, "\e\\(", "{", [global, {return, list}]),
    Message3 = re:replace(Message2, "\e\\)", "}", [global, {return, list}]),
    Check = fun(Pattern) ->
        case re:run(Message3, Pattern) of
            {match, _} ->
                true;
            nomatch ->
                false
        end
    end,
    Patterns = ["submit\s*:", "receipt\s*:"],
    case lists:any(Check, Patterns) of
        true ->
            {ok, re:replace(Message3, ":", ": ", [global, {return, list}])};
        false ->
            error
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
    %% !!! keep add_default_seed the first !!!
    add_default_seed(
        add_default_reply_submit_status(Commands, Context),
        Context
    ).

add_default_reply_submit_status(Commands, Context) ->
    case ?gv(reply_submit_status, Commands) of
        undefined ->
            add_default_commands([{reply_submit_status, {code, 0}} | Commands], Context);
        {code, 0} ->
            add_default_send_deliver_sm(Commands, Context);
        {freq, _} ->
             add_default_send_deliver_sm(Commands, Context);
        _ ->
            Commands
    end.

add_default_seed(Commands, _Context) ->
    case ?gv(seed, Commands) of
        undefined ->
            [{seed, now()} | Commands];
        Seed ->
            [{seed, Seed} | Commands]
    end.

add_default_send_deliver_sm(Commands, Context) ->
    case ?gv(registered_delivery, Context) of
        0 ->
            Commands;
        _ ->
            case ?gv(send_deliver_sm, Commands) of
                undefined ->
                    Message = build_receipt("delivered", Context),
                    Commands ++ [{send_deliver_sm, Message}];
                _ ->
                    Commands
            end
    end.

parse_command({"submit", Status}, Context) ->
    parse_submit_status_command(Status, Context);
parse_command({"receipt", Status}, Context) ->
    case ?gv(registered_delivery, Context) of
        0 ->
            {ok, [nop]};
        _ ->
            parse_receipt_status_command(Status, Context)
    end;
parse_command({"seed", Seed}, Context) ->
    parse_seed_command(Seed, Context);
parse_command({Command, null}, _Context) ->
    ?log_debug("Invalid command: ~p. Proceed as normal message", [Command]),
    error;
parse_command(Command, _Context) ->
    ?log_debug("Unknown command: ~p. Proceed as normal message", [Command]),
    error.

parse_submit_status_command(Status, _Context) when is_integer(Status) ->
    {ok, [{reply_submit_status, {code, Status}}]};
parse_submit_status_command(Plist, _Context) when is_list(Plist) ->
    Status = parse_submit_status(proplists:get_value("status", Plist, 0)),
    Time = parse_delay(proplists:get_value("delay", Plist, 0)),
    {ok, [{sleep, Time}, {reply_submit_status, Status}]}.

parse_receipt_status_command(Status, Context) when is_list(Status) ->
    case proplists:get_keys(Status) of
        [] ->
            %% only status is given.
            Message = build_receipt(string:to_lower(Status), Context),
            {ok, [{send_deliver_sm, Message}]};
        _ ->
            Status2 = proplists:get_value("status", Status, "delivered"),
            Time = parse_delay(proplists:get_value("delay", Status, 0)),
            Message = build_receipt(string:to_lower(Status2), Context),
            {ok, [{sleep, Time}, {send_deliver_sm, Message}]}
    end;
parse_receipt_status_command(Status, Context) ->
    %% build status from what we got.
    Status2 = lists:flatten(io_lib:format("~p", [Status])),
    Message = build_receipt(Status2, Context),
    {ok, [{send_deliver_sm, Message}]}.

parse_seed_command([A1,A2,A3], _Context) when is_integer(A1),
                                              is_integer(A2),
                                              is_integer(A3) ->
    {ok, [{seed, {A1,A2,A3}}]};
parse_seed_command(_Seed, _Context) ->
    error.

parse_submit_status(Status) when is_integer(Status) ->
    {code, Status};
parse_submit_status(Status) when is_list(Status) ->
    case Status of
        %% submit:{status:[{value:1,freq:1.0}]} -> [[{"value",1},{"freq",1.0}]]
        [L|_] when is_list(L) ->
            %io:format("1~n"),
            Freqs = [parse_freq_status(F) || F <- Status],
            {freq, Freqs};
        %% submit:{status:{value:1,freq:1.0}} -> [{"value",1},{"freq",1.0}]
        Status ->
            %io:format("2~n"),
            Freqs = [parse_freq_status(Status)],
            {freq, Freqs}
    end.

parse_freq_status(Freqs) ->
    Value = proplists:get_value("value", Freqs, 0),
    Freq = proplists:get_value("freq", Freqs, 0.0),
    {Value, Freq}.

parse_delay(Time) when is_integer(Time), Time >= 0 ->
    Time;
parse_delay(Time) when is_integer(Time), Time < 0 ->
    0;
parse_delay(Time) when is_list(Time) ->
    case string:to_lower(Time) of
        "inf" ->
            infinity;
        _ ->
            0
    end.

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
