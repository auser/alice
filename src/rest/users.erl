-module (users).
-export ([get/1, post/1, put/1, delete/1]).
-define(RPC_TIMEOUT, 30000).

get(Req) ->
  ["users"|Path] = string:tokens(Req:get(path), "/"),
  [Node|_Other] = rabbit_node(),
  AOut = case Path of
    [] -> 
      Users = call(Node, {rabbit_access_control, list_users, []}),
      "all users: " ++ Users;
    [Id] -> "id for " ++ Id ++ " user"
  end,  
  Out = AOut ++ " on rabbit node " ++ erlang:atom_to_list(Node),
  [{"Content-Type", "text/html"}, Out].

post(_Req) -> "unhandled".
put(_Req) -> "unhandled".
delete(_Req) -> "unhandled".

call(Node, {Mod, Fun, Args}) ->
    rpc_call(Node, Mod, Fun, lists:map(fun list_to_binary/1, Args)).

rpc_call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args, ?RPC_TIMEOUT).
    
% Get rabbit node
rabbit_node() ->
  {value, {nodes, Nodes}} = lists:keysearch(nodes, 1,
          rabbit:status()),
  Nodes.