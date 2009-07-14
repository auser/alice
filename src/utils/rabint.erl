-module (rabint).
-compile (export_all).

-define(RPC_TIMEOUT, 30000).
-define (RABBIT_HEARTBEAT_DELAY, 1000).
-define (MAX_ATTEMPTS, 1000).

% rabint:call({rabbit_access_control, list_users, []})
call({Mod, Fun, Args})    -> rpc_call(Mod, Fun, lists:map(fun list_to_binary/1, Args)).
rpc_call(Mod, Fun, Args)  -> rpc:call(rabbit_node(), Mod, Fun, Args, ?RPC_TIMEOUT).

% Get the local rabbit node
rabbit_node()             -> rabbit_misc:localnode(rabbit).
ping_rabbit()							-> net_adm:ping(rabbit_node()).

% Maintain connection to rabbit
stay_connected_to_rabbit_node(Attempts) ->
  alice_log:info("stay_connected_to_rabbit_node: ~p~n", [Attempts]),
  case Attempts > ?MAX_ATTEMPTS of
    true -> alice_log:info("Lost connect with rabbitmq_server. Check that it's up and try again~n");
    false ->
      timer:sleep(?RABBIT_HEARTBEAT_DELAY),
      case ping_rabbit() of
        pong -> 
          timer:sleep(?RABBIT_HEARTBEAT_DELAY * 3000),
          stay_connected_to_rabbit_node(0);
        pang -> 
          alice_log:info("Lost connection with rabbitmq_server. Trying to regain connection~n", []),
          timer:sleep(?RABBIT_HEARTBEAT_DELAY),
          stay_connected_to_rabbit_node(Attempts+1)
      end
  end.