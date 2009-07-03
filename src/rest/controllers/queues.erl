-module (queues).
-export ([get/1, post/2, put/2, delete/2]).

get([]) ->
  % node doesn't work when turned into an atom... TODO: Add this back
  % http://localhost:9999/info/queues/root/memory/messages_ready/messages_unacknowledged/messages_uncommitted
  {"Must include vhost (use root for /) and an arg of the following:", [name, durable, auto_delete, arguments, messages_ready, messages_unacknowledged, messages_uncommitted, messages, acks_uncommitted, consumers, transactions, memory]};

get(["root" | OtherArgs]) ->
  ?MODULE:get(["/", OtherArgs]);
  
get([VhostArg|[OtherArgs]]) -> 
  Back = get_info_for(VhostArg, OtherArgs ),

  FunSearch = fun(Meth) ->
    case lists:keysearch(Meth, 1, Back) of
      false -> "error";
      {value, {name, Ot}} ->
        % {resource,<<"/">>,queue,<<"queuename">>}
        {resource, Vhost, queue, Name} = Ot,
        {struct, [{"name", [Vhost, Name]}]};
      % Jsonable = [{struct, [{"applications", Apps}, {"nodes", Nodes}, {"running_nodes", RunningNodes}]}],
      {value, {Meth, Ot}} -> 
        {struct, [{erlang:atom_to_list(Meth), utils:turn_binary(Ot)}]}
    end
  end,
  
  O = lists:map(FunSearch, [ erlang:list_to_atom(A) || A <- OtherArgs]),
  {VhostArg, O};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE
get_info_for(VhostArg, OtherArgs) ->
  %VHostArg, ArgAtoms
  Args = [ erlang:list_to_atom(Item) || Item <- OtherArgs],
  case rabint:rpc_call(rabbit_amqqueue, info_all, [ erlang:list_to_binary(VhostArg), 
                                                    Args
                                                  ]) of
    {error, E} -> {"error", erlang:list_to_binary(E)};
    [Bin] -> Bin
  end.

