-module (conn).
-export ([get/1, post/2, put/2, delete/2]).
  

% {"Must include an arg of the following:", [node, address, port, peer_address, peer_port, state, channels, user, vhost, timeout, frame_max,recv_oct, recv_cnt, send_oct, send_cnt, send_pend]};
get([]) -> ?MODULE:get(["address", "port"]);

get(Args) -> 
  [Back] = get_connections_for( Args ),
    
  FunSearch = fun(Meth) ->
    case lists:keysearch(Meth, 1, Back) of
      false                         -> "unknown";
      {value, {address, Ip}}        -> {struct, [{"ip", utils:turn_binary(utils:format_ip(Ip))}]};
      {value, {peer_address, Ip}}   -> {struct, [{"peer_address", utils:turn_binary(utils:format_ip(Ip))}]};
      {value, {port, Port}}         -> {struct, [{"port", utils:turn_binary(Port)}]};
      {value, {peer_port, Port}}    -> {struct, [{"peer_port", utils:turn_binary(Port)}]}
    end
  end,
  
  O = lists:map(FunSearch, [ erlang:list_to_atom(A) || A <- Args]),
  {?MODULE, O};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_connections_for(ArgAtoms) ->
  %VHostArg, ArgAtoms
  Args = [ erlang:list_to_atom(Item) || Item <- ArgAtoms],
  io:format("using args ~p~n", [Args]),
  case rabint:rpc_call(rabbit_networking, connection_info_all, [Args]) of
    {error, E} -> {"error", erlang:list_to_binary(E)};
    Bin -> Bin
  end.
