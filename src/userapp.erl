-module(userapp).
-behaviour(application).

-export([start/2, stop/1, start_phase/3]).
-include("userapp.hrl").

start_application() ->
  application:start(userapp).

start(_Type, _Args) ->
    application:start(inets),
    Args = lists:map(
        fun (Var) -> {ok, Value} = application:get_env(?MODULE, Var), Value end,
        [port, working_dir]
    ),
    userapp_sup:start_link(Args).

stop(_State) -> ok.

start_phase(mnesia, _, _) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
    mnesia:create_table(counter, [{attributes, record_info(fields, counter)}]),
    mnesia:activity(transaction, fun() -> mnesia:write(#counter{type = user, count = 1}) end),
    ok.
