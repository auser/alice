-module (bindings).
-export ([get/1, post/2, put/2, delete/2]).

get([]) -> ?MODULE:get(["/"]);
get(["root"]) -> ?MODULE:get(["/"]);
get([VhostArg]) ->
  Back = get_bindings_for(VhostArg),
  
  case Back of
    [] -> {"bindings", []};
    Else ->
      O = lists:map(fun(Quere) ->
        {Exchange, Queue, AsQueue, _Other} = Quere,
        {resource, _, exchange, Exch} = Exchange,
        {resource, _, queue, RealQueue} = Queue,

        % {"exchange", Exch}, {"other", Other}
        {struct,  [
                        {"queue", utils:turn_binary(RealQueue)},
                        {"exchange", utils:turn_binary(Exch)},
                        {"from_queue", utils:turn_binary(AsQueue)}                  
                      ]}
        end, Else),

      {?MODULE, O}
  end;
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE

get_bindings_for(VhostArg) ->
  case rabint:call({rabbit_exchange, list_bindings, [ VhostArg ]}) of
    {error, _E} -> [];
    Bin -> Bin
  end.