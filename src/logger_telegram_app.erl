-module(logger_telegram_app).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
start(_StartType, _StartArgs) ->
    case logger:add_handlers(logger_telegram) of
        ok ->
            supervisor:start_link(?MODULE, []);
        {error, Reason} ->
            {error, {handlers_not_added, Reason}}
    end.

%% @private
stop(_State) ->
    logger_telegram_h:remove_handlers().

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init(_Args) ->
    {ok, {#{}, []}}.
