-module (exchanges).
-export ([get/1, post/2, put/2, delete/2]).


get([]) -> ?MODULE:get(["root"]);
get(["root" | OtherArgs]) ->
  RealArgs = case OtherArgs of
    [] -> ["name", "type", "durable", "auto_delete", "arguments"];
    Else -> Else
  end,
  ?MODULE:get(["/", RealArgs]);
  
get([VhostArg|[OtherArgs]]) -> 
  Back = get_exchange_for(VhostArg, OtherArgs),

  O = lists:map(
    fun(Line) ->
      {struct, parse_lines_from(Line)}
    end, Back),

  {?MODULE, O};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_exchange_for(VhostArg, OtherArgs) ->
  %VHostArg, ArgAtoms
  Args = [ erlang:list_to_atom(Item) || Item <- OtherArgs],

  case rabint:rpc_call(rabbit_exchange, info_all, [erlang:list_to_binary(VhostArg), Args]) of
    {error, E} -> {"error", erlang:list_to_binary(E)};
    Bin -> Bin
  end.

parse_lines_from(From) ->
  lists:map(fun(Line) -> get_type_from_line(Line) end, From).

get_type_from_line(Line) ->
  case Line of
    {name, {resource,_Vhost,exchange,Name}} ->
      {"name", utils:turn_binary(Name)};
    {type, TypeOf} ->
      {"type", utils:turn_binary(TypeOf)};
    {durable, Bool} ->
      {"durable", utils:turn_binary(Bool)};
    {auto_delete, Bool} ->
      {"auto_delete", utils:turn_binary(Bool)};
    {arguments, Arr} ->
      {"arguments", utils:turn_binary(Arr)}
  end.