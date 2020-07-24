-module(logger_telegram_h).

%% API
-export([remove_handlers/0]).

%% Logger handler callbacks
-export([adding_handler/1, changing_config/3, log/2]).

%%%===================================================================
%%% Macro definitions
%%%===================================================================

-define(HTTP_REQUEST_TIMEOUT, timer:seconds(5)).

-define(IS_LEVEL(L),
        (L =:= emergency orelse
         L =:= alert     orelse
         L =:= critical  orelse
         L =:= error     orelse
         L =:= warning   orelse
         L =:= notice    orelse
         L =:= info      orelse
         L =:= debug     orelse
         L =:= all       orelse
         L =:= none)).

%%%===================================================================
%%% API
%%%===================================================================

-spec remove_handlers() -> ok.
remove_handlers() ->
    lists:foreach(fun logger:remove_handler/1, get_handler_ids()).

%%%===================================================================
%%% Logger handler callbacks
%%%===================================================================

%% @private
-spec adding_handler(Config) -> {ok, Config} | {error, term()} when
      Config :: logger:handler_config().
adding_handler(Config) ->
    update_h_config(get_default_h_config(), Config).

%% @private
-spec changing_config(SetOrUpdate, Old :: Config, New :: Config) ->
    {ok, Config} | {error, term()} when
      SetOrUpdate :: set | update, Config :: logger:handler_config().
changing_config(set, _OldConfig, NewConfig) ->
    update_h_config(get_default_h_config(), NewConfig);
changing_config(update, OldConfig, NewConfig) ->
    update_h_config(maps:get(config, OldConfig), NewConfig).

%% @private
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := Level} = LogEvent, #{config := HConfig} = Config) ->
    Text = format_log_event(LogEvent, Config),
    _ = httpc:request(
          post, http_request(Level, Text, HConfig),
          [{timeout, ?HTTP_REQUEST_TIMEOUT}],
          [{sync, false}, {receiver, fun(_) -> ok end}]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_handler_ids() ->
    F = fun(#{module := ?MODULE, id := HandlerId}) ->
                {true, HandlerId};
           (#{module := _Other}) ->
                false
        end,
    lists:filtermap(F, logger:get_handler_config()).

get_default_h_config() ->
    #{auth_token => get_env(auth_token),
      base_uri   => get_env(base_uri),
      chat_id    => get_env(chat_id),
      disable_notification_level => get_env(disable_notification_level),
      disable_web_page_preview   => get_env(disable_web_page_preview)
     }.

get_env(Par) ->
    application:get_env(logger_telegram, Par, undefined).

update_h_config(HConfig0, Config) ->
    HConfig = maps:merge(HConfig0, maps:get(config, Config, #{})),
    case check_h_config(maps:to_list(HConfig)) of
        ok ->
            {ok, maps:put(config, HConfig, Config)};
        {error, {Key, Value}} ->
            {error, {invalid_config, ?MODULE, #{Key => Value}}}
    end.

check_h_config([{auth_token, AuthToken} | Config])
  when is_list(AuthToken) ->
    check_h_config(Config);
check_h_config([{base_uri, BaseURI} | Config])
  when is_list(BaseURI) ->
    check_h_config(Config);
check_h_config([{chat_id, ChatId} | Config])
  when is_list(ChatId) ->
    check_h_config(Config);
check_h_config([{disable_notification_level, Level} | Config])
  when ?IS_LEVEL(Level) ->
    check_h_config(Config);
check_h_config([{disable_web_page_preview, Flag} | Config])
  when is_boolean(Flag) ->
    check_h_config(Config);
check_h_config([Other | _Config]) ->
    {error, Other};
check_h_config([]) ->
    ok.

format_log_event(LogEvent, #{formatter := {FModule, FConfig}}) ->
    FModule:format(LogEvent, FConfig).

http_request(Level, Text, HConfig) ->
    URI = http_request_uri("sendMessage", HConfig),
    ContentType = "application/x-www-form-urlencoded",
    Body = http_request_body(Level, Text, HConfig),
    {URI, [], ContentType, Body}.

http_request_uri(APIMethod, #{auth_token := AuthToken, base_uri := BaseURI}) ->
    BaseURI ++ "/bot" ++ AuthToken ++ "/" ++ APIMethod.

http_request_body(Level, Text, HConfig) ->
    ChatId = maps:get(chat_id, HConfig),
    DisableNotificationLevel = maps:get(disable_notification_level, HConfig),
    DisableWebPagePreview = maps:get(disable_web_page_preview, HConfig),
    uri_string:compose_query(
      [{"chat_id", ChatId},
       {"text", Text},
       {"disable_notification",
        encode_disable_notification_level(Level, DisableNotificationLevel)},
       {"disable_web_page_preview",
        encode_disable_web_page_preview(DisableWebPagePreview)}
      ]).

encode_disable_notification_level(_Level, all) ->
    "true";
encode_disable_notification_level(_Level, none) ->
    "false";
encode_disable_notification_level(Level, DisableLevel) ->
    case logger:compare_levels(Level, DisableLevel) of
        gt ->
            "false";
        _ ->
            "true"
    end.

encode_disable_web_page_preview(true) ->
    "true";
encode_disable_web_page_preview(false) ->
    "false".
