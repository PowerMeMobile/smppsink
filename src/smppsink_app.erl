-module(smppsink_app).

-behaviour(application).

%% application callbacks
-export([
    start/2,
    stop/1
]).

%% API
-export([
    get_env/1
]).

-include_lib("alley_common/include/application_spec.hrl").

-define(APP, smppsink).

%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = smppsink_mnesia:ensure_schema(),
    ok = application:start(mnesia),
    smppsink_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

-spec get_env(atom()) -> any().
get_env(Key) ->
    case application:get_env(?APP, Key) of
        undefined ->
            default_env(Key);
        {ok, Val} ->
            Val
    end.

%% ===================================================================
%% Internal
%% ===================================================================

default_env(addr) ->
    {0,0,0,0};
default_env(port) ->
    2775;
default_env(server_name) ->
    "smppsink";
default_env(accounts) ->
    [{account, [
        {bind_type, trx},
        {system_type, ""},
        {system_id, "user"},
        {password, "password"}]}];
default_env(session_init_time) ->
    10000;
default_env(enquire_link_time) ->
    60000;
default_env(inactivity_time) ->
    infinity;
default_env(response_time) ->
    60000;
default_env(log_smpp_pdus) ->
    true;
default_env(smpp_pdu_log_dir) ->
    "log/smpp";
default_env(file_log_size) ->
    5000000.
