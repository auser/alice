-module (conn).
-export ([get/1, post/2, put/2, delete/2]).
  

% FIX node
% {"Must include an arg of the following:", [address, port, peer_address, peer_port, state, channels, user, vhost, timeout, frame_max,recv_oct, recv_cnt, send_oct, send_cnt, send_pend]};
get([]) -> ?MODULE:get(["address", "port"]);

get(Args) -> 
  Back = get_connections_for( Args ),
    
  O = lists:map(
    fun(Line) ->
      {struct, parse_proplist_from(Line)}
    end, Back),    
  {?MODULE, O}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_connections_for(_ArgAtoms) ->
  rabint:rpc_call(rabbit_networking, connection_info_all, []).

parse_proplist_from(From) ->
  lists:map(fun(Prop) -> convert_prop_for_json(Prop) end, From).
    
convert_prop_for_json(Prop) ->
  case Prop of
    {address, Ip}         -> {"ip", utils:turn_binary(utils:format_ip(Ip))};
    {peer_address, Ip}    -> {"peer_address", utils:turn_binary(utils:format_ip(Ip))};
    {channels, Channels}  -> {"channels", utils:turn_binary(Channels)};
    {vhost, Vhost}        -> {"vhost", utils:turn_binary(Vhost)};
    {timeout, Timeout}    -> {"timeout", utils:turn_binary(Timeout)};
    {frame_max, Frame}    -> {"frame_max", utils:turn_binary(Frame)};
    {recv_oct, RecvOc}    -> {"recv_oct", utils:turn_binary(RecvOc)};
    {recv_cnt, RecvCn}    -> {"recv_cnt", utils:turn_binary(RecvCn)};
    {send_oct, SendOct}   -> {"send_oct", utils:turn_binary(SendOct)};
    {send_cnt, SendCnt}   -> {"send_cnt", utils:turn_binary(SendCnt)};
    {send_pend, SendPen}  -> {"send_pend", utils:turn_binary(SendPen)};
    {user, User}          -> {"user", utils:turn_binary(User)};
    {state, State}        -> {"state", utils:turn_binary(State)};
    {port, Port}          -> {"port", utils:turn_binary(Port)};
    {peer_port, Port}     -> {"peer_port", utils:turn_binary(Port)};
    {pid, Pid}            -> {"pid", utils:turn_binary(erlang:pid_to_list(Pid))}
  end.