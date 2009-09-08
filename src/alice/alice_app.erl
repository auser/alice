-module (alice_app).
-behaviour(application).

-export([start/2, stop/1]).

start(Type, Args) ->
    % Args = lists:map(
    %     fun (Var) -> {ok, Value} = application:get_env(alice, Var), Value end,
    %     [port]
    % ),
    alice_sup:start(Type, Args).

stop(State) -> 
  alice_sup:stop(State).
