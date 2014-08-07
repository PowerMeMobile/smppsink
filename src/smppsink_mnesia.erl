-module(smppsink_mnesia).

%% API
-export([
    ensure_schema/0,
    ensure_table/2
]).

%% ===================================================================
%% API
%% ===================================================================

-spec ensure_schema() -> ok | {error, term()}.
ensure_schema() ->
    case mnesia:create_schema([node()]) of
        ok ->
            ok;
        {error, {_Node, {already_exists, _Node}}} ->
            ok;
        {error, _Other} = Error ->
            Error
    end.

-spec ensure_table(atom(), list()) -> ok | {error, term()}.
ensure_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Name}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.
