-module (testing).
-compile (export_all).

% f(). Str = "name=baskinrobbins&boxes=full". testing:turn_equal_to_json(Str).
turn_equal_to_json(Str) ->
  Set = string:tokens(Str, "&"),
  ArrSet = lists:map(
    fun(Line) -> 
      string:tokens(Line, "=")
    end, Set),
  lists:map(fun([K|V]) -> "{'"++K++"':'"++erlang:list_to_binary(V)++"'}" end, ArrSet).