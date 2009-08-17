-module (rabint).
-include ("alice.hrl").
-compile (export_all).

-define(RPC_TIMEOUT, 30000).
-define (RABBIT_HEARTBEAT_DELAY, 1000).
-define (MAX_ATTEMPTS, 10).

% rabint:call({rabbit_access_control, list_users, []})
call({Mod, Fun, Args})    -> rpc_call(Mod, Fun, lists:map(fun list_to_binary/1, Args)).
rpc_call(Mod, Fun, Args)  -> rpc:call(rabbit_node(), Mod, Fun, Args, ?RPC_TIMEOUT).

% Get the rabbit node from the env
rabbit_node() ->
  case application:get_env(alice, rabbithost) of
    undefined         -> localnode(rabbit);
    {ok, Hostname}    -> rabbit_node(Hostname)
  end.
  
rabbit_node(Hostname)   ->  case Hostname of
  H when is_atom(H) -> H;
  Else -> list_to_atom(Else)
end.

ping_rabbit()						-> net_adm:ping(rabbit_node()).


% TAKEN RIGHT FROM rabbitmq-server/rabbit_misc
localnode(Name) ->
    %% This is horrible, but there doesn't seem to be a way to split a
    %% nodename into its constituent parts.
    list_to_atom(lists:append(atom_to_list(Name), lists:dropwhile(fun (E) -> E =/= $@ end, atom_to_list(node())))).


% Maintain connection to rabbit
stay_connected_to_rabbit_node(Attempts) ->
  case Attempts > ?MAX_ATTEMPTS of
    true -> 
      ?ERROR("Lost connect with rabbitmq_server. Check that it's up and try again~n", []);
    false ->
      timer:sleep(?RABBIT_HEARTBEAT_DELAY),
      case ping_rabbit() of
        pong ->
          stay_connected_to_rabbit_node(0);
        pang -> 
          ?ERROR("Lost connection with rabbitmq_server. Trying to regain connection to ~p~n", [rabbit_node()]),
          stay_connected_to_rabbit_node(Attempts+1)
      end
  end.