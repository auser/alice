-module (users).
-export ([get/1, post/1, put/1, delete/1]).

get(Req) ->
  io:format("Handling get: ~p~n", [Req]),
  "users!".

post(_Req) -> "unhandled".
put(_Req) -> "unhandled".
delete(_Req) -> "unhandled".