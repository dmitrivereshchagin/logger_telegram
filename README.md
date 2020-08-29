# logger\_telegram

Telegram handler for Erlang/OTP Logger.

## Handler configuration

Handler configuration is a map which can contain general configuration
and handler specific parameters.  The specific data is stored in a sub
map with the key `config`, and can contain the following parameters:

*   `auth_token = string()`:  
    Token to access Bot API.  Talk to [BotFather][1] in order to create
    a bot and receive its token.  Required.

*   `chat_id = string()`:  
    Target chat identifier (or channel username in the format
    `@channel`).  Required.

    If you plan to use a private channel, follow [these instructions][2]
    to obtain its identifier.

*   `disable_notification_level = logger:level() | all | none`:  
    Specifies log level to disable sound notifications.  Log events with
    the same, or a less severe level, are sent silently.  Defaults to
    `all`.

*   `disable_web_page_preview = boolean()`:  
    Disables link previews for links in messages.  Defaults to `true`.

All these parameters can also be set in the application configuration.
Corresponding application parameters are used as default values.

[1]: https://core.telegram.org/bots#6-botfather
[2]: https://stackoverflow.com/a/56546442

## Application configuration

A configuration similar to the following can be used to send messages to
a single chat or channel:

    {logger_telegram,
     [{auth_token, "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"},
      {chat_id, "@channel"}
     ]}

Multiple handler instances can be added, for example, to send messages
to several chats or channels:

    {logger_telegram,
     [{auth_token, "123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11"},
      {logger,
       [{handler, tg1, logger_telegram_h, #{config => #{chat_id => "@channel1"}}},
        {handler, tg2, logger_telegram_h, #{config => #{chat_id => "@channel2"}}}
       ]}
     ]}

If you don't want handlers to be added on application start, pass empty
list as the `logger` configuration parameter:

    {logger_telegram, [{logger, []}]}
