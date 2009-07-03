-module (exchanges).
-export ([get/1, post/2, put/2, delete/2]).


get(["root" | OtherArgs]) ->
  ?MODULE:get(["/", OtherArgs]);
  
get([VhostArg|[OtherArgs]]) -> 
  Back = get_exchange_for(VhostArg, OtherArgs ),

  O = parse_lines_from([ erlang:list_to_atom(Arg) || Arg <- OtherArgs ], Back),
  {VhostArg, O};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_exchange_for(VhostArg, OtherArgs) ->
  %VHostArg, ArgAtoms
  Args = [ erlang:list_to_atom(Item) || Item <- OtherArgs],
  case rabint:rpc_call(rabbit_exchange, info_all, [ erlang:list_to_binary(VhostArg), Args ]) of
    {error, E} -> {"error", erlang:list_to_binary(E)};
    Bin -> Bin
  end.

get_type_from_line(Type, Line) ->
  {value, Ot} = lists:keysearch(Type, 1, Line),
  case Type of
    name ->
      {name, {resource,Vhost,exchange,Name}} = Ot,
      {struct, [{Vhost, utils:turn_binary(Name)}]};
    type ->
      {type, TypeOf} = Ot,
      {struct, [{"type", utils:turn_binary(TypeOf)}]}
  end.

parse_lines_from(As, From) ->
  lists:map(
    fun(Line) -> 
      [
        get_type_from_line(Arg, Line)
        || Arg <- As
      ]
    end, From).