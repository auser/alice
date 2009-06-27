-module (rest_server_app).
-behaviour(application).

-export([start/2, stop/1]).
-include("rest_server.hrl").

start(_Type, _Args) ->
  io:format("Starting... : ~p~n", application:get_env(?MODULE, port)),
    Args = lists:map(
        fun (Var) -> {ok, Value} = application:get_env(?MODULE, Var), Value end,
        [port]
    ),
  io:format("Args: ~p~n", [Args]),
  rest_server_sup:start_link(Args).

stop(_State) -> ok.