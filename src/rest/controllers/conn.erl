-module (conn).
-export ([get/1, post/2, put/2, delete/2]).
  

% FIX node
% {"Must include an arg of the following:", [address, port, peer_address, peer_port, state, channels, user, vhost, timeout, frame_max,recv_oct, recv_cnt, send_oct, send_cnt, send_pend]};
get([]) -> ?MODULE:get(["address", "port"]);

get(Args) -> 
  Back = get_connections_for( Args ),
  
  O = case Back of
    [] -> 
      {struct, [{?MODULE, utils:turn_binary([])}]};
    Else ->
      FunCollect = fun(Info) ->
        Key = case lists:keysearch(address, 1, Info) of
          {value, {address, Ip}}          -> utils:format_ip(Ip);
          false                           -> "127.0.01"
        end,
        Val = extract_connection_info(Info, Args),        
        {struct, [{Key, Val}]}
      end,
      lists:map(FunCollect, Else)
  end,
  {?MODULE, O}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_connections_for(_ArgAtoms) ->
  %VHostArg, ArgAtoms
  rabint:rpc_call(rabbit_networking, connection_info_all, []).

extract_connection_info(Info, Args) ->
  FunSearch = fun(Meth) ->
    case lists:keysearch(Meth, 1, Info) of
      false                         -> {struct, [{erlang:atom_to_list(Meth), utils:turn_binary("not found")}]};
      {value, {address, Ip}}        -> {struct, [{"ip", utils:turn_binary(utils:format_ip(Ip))}]};
      {value, {peer_address, Ip}}   -> {struct, [{"peer_address", utils:turn_binary(utils:format_ip(Ip))}]};
      {value, {channels, Channels}} -> {struct, [{"channels", utils:turn_binary(Channels)}]};
      {value, {vhost, Vhost}}       -> {struct, [{"vhost", utils:turn_binary(Vhost)}]};
      {value, {timeout, Timeout}}   -> {struct, [{"timeout", utils:turn_binary(Timeout)}]};
      {value, {frame_max, Frame}}   -> {struct, [{"frame_max", utils:turn_binary(Frame)}]};
      {value, {recv_oct, RecvOc}}   -> {struct, [{"recv_oct", utils:turn_binary(RecvOc)}]};
      {value, {recv_cnt, RecvCn}}   -> {struct, [{"recv_cnt", utils:turn_binary(RecvCn)}]};
      {value, {send_oct, SendOct}}  -> {struct, [{"send_oct", utils:turn_binary(SendOct)}]};
      {value, {send_cnt, SendCnt}}  -> {struct, [{"send_cnt", utils:turn_binary(SendCnt)}]};
      {value, {send_pend, SendPen}} -> {struct, [{"send_pend", utils:turn_binary(SendPen)}]};
      {value, {user, User}}         -> {struct, [{"user", utils:turn_binary(User)}]};
      {value, {state, State}}       -> {struct, [{"state", utils:turn_binary(State)}]};
      {value, {port, Port}}         -> {struct, [{"port", utils:turn_binary(Port)}]};
      {value, {peer_port, Port}}    -> {struct, [{"peer_port", utils:turn_binary(Port)}]}
    end
  end,
  lists:map(FunSearch, [ erlang:list_to_atom(A) || A <- Args]).