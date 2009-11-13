-module (queues).
-export ([get/1, post/2, put/2, delete/2]).
-export ([get_info_for/2]).

  % node doesn't work when turned into an atom... TODO: Add this back
  % http://localhost:9999/info/queues/root/memory/messages_ready/messages_unacknowledged/messages_uncommitted
  % {"Must include vhost (use root for /) and an arg of the following:", [name, durable, auto_delete, arguments, messages_ready, messages_unacknowledged, messages_uncommitted, messages, acks_uncommitted, consumers, transactions, memory]};
get([]) -> ?MODULE:get(["root", "name", "memory"]);

get(["root" | OtherArgs]) ->
  get_impl(["/", OtherArgs]);
  
get([VhostArg | OtherArgs]) ->
  get_impl([VhostArg, OtherArgs]);
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_impl([VhostArg|[Args]]) ->
  Back = get_info_for( VhostArg, Args ),
  O = lists:map(
    fun(Line) ->
      {struct, convert_prop_for_json(Line)}
    end, Back),
  {?MODULE, O}.

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

convert_prop_for_json(Prop) ->
  convert_prop_for_json(Prop, []).
convert_prop_for_json([], Acc) ->
  lists:flatten(Acc);
convert_prop_for_json([Prop|Rest], Acc) ->
  convert_prop_for_json(Rest, [
  case Prop of
    {name, {resource, VHost, queue, Name}}  ->
      [{"name", utils:turn_binary(Name)},{vhost, utils:turn_binary(VHost)}];
    {memory, Mem}                           -> {"memory", utils:turn_binary(Mem)};    
    {durable, Dr}                           -> {"durable", utils:turn_binary(Dr)};
    {auto_delete, Ad}                       -> {"auto_delete", utils:turn_binary(Ad)};
    {arguments, Agms}                       -> {"arguments", utils:turn_binary(Agms)};
    {messages_ready, Mr}                    -> {"messages_ready", utils:turn_binary(Mr)};
    {messages_unacknowledged, Mu}           -> {"messages_unacknowledged", utils:turn_binary(Mu)};
    {messages_uncommitted, Muc}             -> {"messages_uncommitted", utils:turn_binary(Muc)};
    {acks_uncommitted, Acks}                -> {"acks_uncommitted", utils:turn_binary(Acks)};
    {consumers, Cons}                       -> {"consumers", utils:turn_binary(Cons)};
    {transactions, Trans}                   -> {"transactions", utils:turn_binary(Trans)};
    {messages, M}                           -> {"messages", utils:turn_binary(M)}
  end | Acc]).