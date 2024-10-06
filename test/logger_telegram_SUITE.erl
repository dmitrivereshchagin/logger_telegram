-module(logger_telegram_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

%%%===================================================================
%%% Macro definitions
%%%===================================================================

-define(HANDLER_CONFIG,
        #{config => #{auth_token => "T", chat_id => "C"}, level => none}).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [handlers_added_on_start,
     handlers_removed_on_stop
    ].

init_per_testcase(_TestCase, Config) ->
    ok = application:load(logger_telegram),
    ok = application:set_env(logger_telegram, logger, []),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _  = application:stop(logger_telegram),
    ok = application:unload(logger_telegram).

%%%===================================================================
%%% Test cases
%%%===================================================================

handlers_added_on_start(_Config) ->
    Handler = {handler, 'H', logger_telegram_h, ?HANDLER_CONFIG},
    ok = application:set_env(logger_telegram, logger, [Handler]),
    {ok, _} = application:ensure_all_started(logger_telegram),
    ?assert(lists:member('H', logger:get_handler_ids())).

handlers_removed_on_stop(_Config) ->
    {ok, _} = application:ensure_all_started(logger_telegram),
    ok = logger:add_handler('H', logger_telegram_h, ?HANDLER_CONFIG),
    ok = application:stop(logger_telegram),
    ?assertNot(lists:member('H', logger:get_handler_ids())).
