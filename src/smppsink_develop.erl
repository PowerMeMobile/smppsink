-module(smppsink_develop).

-export([init/0]).

-spec init() -> ok.
init() ->
    lager:set_loglevel(lager_console_backend, debug),
    application:start(sync),
    ok.
