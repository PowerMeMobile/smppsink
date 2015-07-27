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
            add_default_commands([{reply_submit_status, {value, 0}} | Commands], Context);
        {value, 0} ->
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
                    Commands ++ [{send_deliver_sm, {value, "delivered"}}];
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
    {ok, [{reply_submit_status, {value, Status}}]};
parse_submit_status_command(Plist, _Context) when is_list(Plist) ->
    Status = parse_submit_status(proplists:get_value("status", Plist, 0)),
    Time = parse_delay(proplists:get_value("delay", Plist, 0)),
    {ok, [{sleep, Time}, {reply_submit_status, Status}]}.

parse_receipt_status_command(Status, _Context) when is_list(Status) ->
    case is_flat_list(Status) of
        true ->
            %% only status is given.
            {ok, [{send_deliver_sm, {value, Status}}]};
        false ->
            Status2 = parse_receipt_status(proplists:get_value("status", Status, "delivered")),
            Time = parse_delay(proplists:get_value("delay", Status, 0)),
            {ok, [{sleep, Time}, {send_deliver_sm, Status2}]}
    end;
parse_receipt_status_command(Status, _Context) ->
    %% build status from what we got.
    Status2 = lists:flatten(io_lib:format("~p", [Status])),
    {ok, [{send_deliver_sm, {value, Status2}}]}.

parse_seed_command(Seed, _Context) when is_integer(Seed) ->
    Seed2 = {Seed div 1000000000000,
             Seed div 1000000 rem 1000000,
             Seed rem 1000000},
    {ok, [{seed, Seed2}]};
parse_seed_command(_Seed, _Context) ->
    error.

parse_submit_status(Status) when is_integer(Status) ->
    {value, Status};
parse_submit_status(Status) when is_list(Status) ->
    case Status of
        %% submit:{status:[{value:1,freq:1.0}]} -> [[{"value",1},{"freq",1.0}]]
        [L|_] when is_list(L) ->
            %io:format("1~n"),
            Freqs = [parse_submit_freq_status(F) || F <- Status],
            {freq, Freqs};
        %% submit:{status:{value:1,freq:1.0}} -> [{"value",1},{"freq",1.0}]
        Status ->
            %io:format("2~n"),
            Freqs = [parse_submit_freq_status(Status)],
            {freq, Freqs}
    end.

parse_submit_freq_status(Freqs) ->
    Value = proplists:get_value("value", Freqs, 0),
    Freq = proplists:get_value("freq", Freqs, 0.0),
    {Value, Freq}.

parse_receipt_status(Status) when is_list(Status) ->
    case is_flat_list(Status) of
        true ->
            {value, Status};
        false ->
            case Status of
                %% receipt:{status:[{value:"delivered",freq:1.0}]} -> [[{"value","delivered"},{"freq",1.0}]]
                [L|_] when is_list(L) ->
                    %io:format("1~n"),
                    Freqs = [parse_receipt_freq_status(F) || F <- Status],
                    {freq, Freqs};
                %% submit:{status:{value:"delivered",freq:1.0}} -> [{"value","delivered"},{"freq",1.0}]
                _Other ->
                    %io:format("2~n"),
                    Freqs = [parse_receipt_freq_status(Status)],
                    {freq, Freqs}
            end
    end;
parse_receipt_status(Status) ->
    %% build status from what we got.
    Status2 = lists:flatten(io_lib:format("~p", [Status])),
    {value, Status2}.

is_flat_list(List) ->
    proplists:get_keys(List) =:= [] andalso
    List =:= lists:flatten(List).

parse_receipt_freq_status(Freqs) ->
    Value = proplists:get_value("value", Freqs, "delivered"),
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
