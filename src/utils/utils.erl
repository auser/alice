-module (utils).
-compile (export_all).
-define(JSON_ENCODE(V), mochijson2:encode(V)).

% utils:delete(a, [{port, "90"}, {a, "danger"}]). => [{port,"90"}]
% utils:delete(a, [{port, "90"}, {ab, "danger"}]). => [{port,"90"},{ab,"danger"}]
delete(Key, Config) ->
	[ T || T <- Config, element(1, T) =/= Key].
	
% utils:append([{port, 90}], [{port, 12345}, {name, "converse"}]).
% [{port,12345},{name,"converse"},{friends,"whisper"}]
append([H|T], L) -> 
  {Key, _Value} = H,
  NewL = delete(Key, L),
  [H|append(T, NewL)];
append([], L) -> L.


% utils:jsonify({"status", Apps})

jsonify(Body) when is_atom(Body) ->
  [ ?JSON_ENCODE({
        struct, [
          Body
        ]
    })
  ];

jsonify(Body) ->
  [ ?JSON_ENCODE({ 
      Body
    })
  ].