-module (alice_sup).
-include ("alice.hrl").
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

start(Type, Args) ->  
  supervisor:start_link(?MODULE, [Type, Args]).

init([_Type, Args]) ->
  io:format("starting ~p with ~p~n", [?MODULE, Args]),
  
  LogServerSup  = { alice_log,        {alice_log, start_link, Args}, permanent,2000,worker,[]},
  RestServerSup = { rest_server_sup,  {rest_server_sup, start_link, [Args]}, permanent,2000,worker,[]},
  
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},[
    LogServerSup, 
    RestServerSup
  ]}}.
  
stop(Args) ->
  lists:map(fun(Term) -> Term:stop(Args) end, [
                                            alice_log,
                                            rest_server_sup
                                          ]),
  ok.