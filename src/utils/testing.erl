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
  
deprecated_test() ->
  Vhost = "/",  
  Catcher = try rabint:call({rabbit_access_control, list_vhost_users, [Vhost]}) of
    {badrpc, Mess} ->
      io:format("----------Got back: badrc: ~p~n", [Mess]),
      rabint:call({rabbit_access_control, list_users, []});
    V -> V
  catch
    X -> 
      io:format("Uh oh. have to deprecate: ~p~n", [X])
  end,
  io:format("Got: ~p~n", [Catcher]),
  ok.