-module(logger_telegram_h_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Macro definitions
%%%===================================================================

-define(BASE_URI, "http://localhost:32002").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, handler}, {group, bot_api}].

groups() ->
    [{handler, [],
      [added_using_explicit_data,
       added_using_data_from_env,
       not_added_without_auth_token,
       not_added_without_chat_id,
       config_set_using_explicit_data,
       config_set_using_data_from_env,
       config_not_set_without_auth_token,
       config_not_set_without_chat_id,
       config_updated
      ]},
     {bot_api, [],
      [valid_request_sent,
       notification_disabled_for_all_levels,
       notification_disabled_for_specified_level,
       notification_disabled_for_less_severe_level,
       notification_enabled_for_more_severe_level,
       notification_enabled_for_all_levels,
       web_page_pareview_disabled,
       web_page_pareview_enabled
      ]}
    ].

init_per_group(bot_api, Config) ->
    {ok, _} = application:ensure_all_started(booze),
    {ok, _} = bookish_spork:start_server(),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(bot_api, _Config) ->
    ok = bookish_spork:stop_server();
end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    ok = application:load(logger_telegram),
    ok = application:set_env(logger_telegram, base_uri, ?BASE_URI),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = logger_telegram_h:remove_handlers(),
    ok = application:unload(logger_telegram).

%%%===================================================================
%%% Test cases
%%%===================================================================

added_using_explicit_data(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T", chat_id => "C"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ?assertMatch({ok, #{config := #{auth_token := "T", chat_id := "C"}}},
                 logger:get_handler_config('H')).

added_using_data_from_env(_Config) ->
    ok = application:set_env(logger_telegram, auth_token, "T"),
    ok = application:set_env(logger_telegram, chat_id, "C"),
    ok = logger:add_handler('H', logger_telegram_h, #{}),
    ?assertMatch({ok, #{config := #{auth_token := "T", chat_id := "C"}}},
                 logger:get_handler_config('H')).

not_added_without_auth_token(_Config) ->
    HandlerConfig = #{config => #{chat_id => "C"}},
    ?assertEqual({error, {handler_not_added, invalid_config(auth_token, undefined)}},
                 logger:add_handler('H', logger_telegram_h, HandlerConfig)).

not_added_without_chat_id(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T"}},
    ?assertEqual({error, {handler_not_added, invalid_config(chat_id, undefined)}},
                 logger:add_handler('H', logger_telegram_h, HandlerConfig)).

config_set_using_explicit_data(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T1", chat_id => "C1"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ok = logger:set_handler_config('H', config, #{auth_token => "T2", chat_id => "C2"}),
    ?assertMatch({ok, #{config := #{auth_token := "T2", chat_id := "C2"}}},
                 logger:get_handler_config('H')).

config_set_using_data_from_env(_Config) ->
    ok = application:set_env(logger_telegram, auth_token, "T"),
    ok = application:set_env(logger_telegram, chat_id, "C"),
    HandlerConfig = #{config => #{auth_token => "T1", chat_id => "C1"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ok = logger:set_handler_config('H', config, #{}),
    ?assertMatch({ok, #{config := #{auth_token := "T", chat_id := "C"}}},
                 logger:get_handler_config('H')).

config_not_set_without_auth_token(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T1", chat_id => "C1"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ?assertEqual({error, invalid_config(auth_token, undefined)},
                 logger:set_handler_config('H', config, #{chat_id => "C2"})).

config_not_set_without_chat_id(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T1", chat_id => "C1"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ?assertEqual({error, invalid_config(chat_id, undefined)},
                 logger:set_handler_config('H', config, #{auth_token => "T2"})).

config_updated(_Config) ->
    HandlerConfig = #{config => #{auth_token => "T", chat_id => "C1"}},
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    ok = logger:update_handler_config('H', config, #{chat_id => "C2"}),
    ?assertMatch({ok, #{config := #{auth_token := "T", chat_id := "C2"}}},
                 logger:get_handler_config('H')).

valid_request_sent(_Config) ->
    HConfig = #{auth_token => "TOKEN", chat_id => "CHAT-ID"},
    FConfig = #{template => [msg]},
    ok = logger:add_handler('H', logger_telegram_h, handler_config(HConfig, FConfig)),
    {ok, {Method, URI, Body}} = log_and_capture_request(notice, "TEXT"),
    ?assertEqual(post, Method),
    ?assertEqual("/botTOKEN/sendMessage", URI),
    ?assertEqual("CHAT-ID", proplists:get_value("chat_id", Body)),
    ?assertEqual("TEXT", proplists:get_value("text", Body)).

notification_disabled_for_all_levels(_Config) ->
    HandlerConfig = handler_config(#{disable_notification_level => all}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(emergency),
    ?assertEqual("true", proplists:get_value("disable_notification", Body)).

notification_disabled_for_less_severe_level(_Config) ->
    HandlerConfig = handler_config(#{disable_notification_level => warning}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(notice),
    ?assertEqual("true", proplists:get_value("disable_notification", Body)).

notification_disabled_for_specified_level(_Config) ->
    HandlerConfig = handler_config(#{disable_notification_level => warning}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(warning),
    ?assertEqual("true", proplists:get_value("disable_notification", Body)).

notification_enabled_for_more_severe_level(_Config) ->
    HandlerConfig = handler_config(#{disable_notification_level => warning}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(error),
    ?assertEqual("false", proplists:get_value("disable_notification", Body)).

notification_enabled_for_all_levels(_Config) ->
    HandlerConfig = handler_config(#{disable_notification_level => none}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(notice),
    ?assertEqual("false", proplists:get_value("disable_notification", Body)).

web_page_pareview_disabled(_Config) ->
    HandlerConfig = handler_config(#{disable_web_page_preview => true}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(notice),
    ?assertEqual("true", proplists:get_value("disable_web_page_preview", Body)).

web_page_pareview_enabled(_Config) ->
    HandlerConfig = handler_config(#{disable_web_page_preview => false}),
    ok = logger:add_handler('H', logger_telegram_h, HandlerConfig),
    {ok, {_, _, Body}} = log_and_capture_request(notice),
    ?assertEqual("false", proplists:get_value("disable_web_page_preview", Body)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

invalid_config(Key, Value) ->
    {invalid_config, logger_telegram_h, #{Key => Value}}.

log_and_capture_request(Level) ->
    log_and_capture_request(Level, "").

log_and_capture_request(Level, String) ->
    bookish_spork:stub_request(),
    logger:log(Level, String),
    case bookish_spork:capture_request(timer:seconds(1)) of
        {ok, Request} ->
            {ok, request(Request)};
        {error, _} = Error ->
            Error
    end.

request(Request) ->
    Method = bookish_spork_request:method(Request),
    URI = binary_to_list(bookish_spork_request:uri(Request)),
    Body = binary_to_list(bookish_spork_request:body(Request)),
    {Method, URI, uri_string:dissect_query(Body)}.

handler_config(HConfig) ->
    handler_config(HConfig, #{}).

handler_config(HConfig, FConfig) ->
    HConfig1 = maps:merge(#{auth_token => "T", chat_id => "C"}, HConfig),
    #{config => HConfig1, formatter => {logger_formatter, FConfig}}.
