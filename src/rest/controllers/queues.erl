-module (queues).
-export ([get/1, post/2, put/2, delete/2]).

  % node doesn't work when turned into an atom... TODO: Add this back
  % http://localhost:9999/info/queues/root/memory/messages_ready/messages_unacknowledged/messages_uncommitted
  % {"Must include vhost (use root for /) and an arg of the following:", [name, durable, auto_delete, arguments, messages_ready, messages_unacknowledged, messages_uncommitted, messages, acks_uncommitted, consumers, transactions, memory]};
get([]) -> ?MODULE:get(["root", "name", "memory"]);

get(["root" | OtherArgs]) ->
  ?MODULE:get(["/", OtherArgs]);
  
get([VhostArg|[Args]]) ->
  Back = get_info_for( VhostArg, Args ),
  
  FunSearch = fun(Meth, Line) ->
    case lists:keysearch(Meth, 1, Line) of
      false                                           -> <<"unknown">>;
      {value, {memory, Mem}}                          -> {struct, [{"memory", utils:turn_binary(Mem)}]};
      {value, {name, {resource, VHost, queue, Name}}} -> {struct, [{"name", utils:turn_binary(Name)}, {"vhost", VHost}]};
      {value, {durable, Dr}}                          -> {struct, [{"durable", utils:turn_binary(Dr)}]};
      {value, {auto_delete, Ad}}                      -> {struct, [{"auto_delete", utils:turn_binary(Ad)}]};
      {value, {arguments, Agms}}                      -> {struct, [{"arguments", utils:turn_binary(Agms)}]};
      {value, {messages_ready, Mr}}                   -> {struct, [{"messages_ready", utils:turn_binary(Mr)}]};
      {value, {messages_unacknowledged, Mu}}          -> {struct, [{"messages_unacknowledged", utils:turn_binary(Mu)}]};
      {value, {messages_uncommitted, Muc}}            -> {struct, [{"messages_uncommitted", utils:turn_binary(Muc)}]};
      {value, {acks_uncommitted, Acks}}               -> {struct, [{"acks_uncommitted", utils:turn_binary(Acks)}]};
      {value, {consumers, Cons}}                      -> {struct, [{"consumers", utils:turn_binary(Cons)}]};
      {value, {transactions, Trans}}                  -> {struct, [{"transactions", utils:turn_binary(Trans)}]};
      {value, {messages, M}}                          -> {struct, [{"messages", utils:turn_binary(M)}]}
    end
  end,
  
  O = lists:map(fun(Line)->
    [ FunSearch(erlang:list_to_atom(A), Line) || A <- Args ]
  end,
  Back),
  {?MODULE, O};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE
get_info_for(VhostArg, OtherArgs) ->
  %VHostArg, ArgAtoms
  Args = [ utils:turn_to_atom(Item) || Item <- OtherArgs],
  case rabint:rpc_call(rabbit_amqqueue, info_all, [ erlang:list_to_binary(VhostArg), 
                                                    Args
                                                  ]) of
    {error, E} -> {"error", erlang:list_to_binary(E)};
    [] -> [];
    Bin -> Bin
  end.

