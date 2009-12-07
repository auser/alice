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

turn_binary(Arg) when is_atom(Arg) -> erlang:list_to_binary(erlang:atom_to_list(Arg));
turn_binary(Arg) when is_binary(Arg) -> Arg;
turn_binary(Arg) when is_boolean(Arg) -> erlang:list_to_binary(erlang:atom_to_list(Arg));
turn_binary(Arg) when is_tuple(Arg) -> erlang:term_to_binary(Arg);
turn_binary(Arg) when is_integer(Arg) -> erlang:list_to_binary(erlang:integer_to_list(Arg));
turn_binary(Arg) -> erlang:list_to_binary(Arg).

turn_to_atom(Arg) when is_atom(Arg) -> Arg;
turn_to_atom(Arg) when is_integer(Arg) -> erlang:list_to_atom(erlang:integer_to_list(Arg));
turn_to_atom(List) when is_list(List) -> 
	case io_lib:char_list(List) of
	  true -> erlang:list_to_atom(List);
	  false -> lists:map(fun(Ele) -> turn_to_atom(Ele) end, List)
	end;
turn_to_atom(Arg) -> Arg.

% Gross
format_ip({A,B,C,D}) ->
  integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ integer_to_list(C) ++ "." ++ integer_to_list(D).

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

