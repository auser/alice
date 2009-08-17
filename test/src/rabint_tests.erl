-module (rabint_tests).
-include ("alice.hrl").
-include_lib("eunit/include/eunit.hrl").

setup() ->
  ok.

teardown(_S) ->
  ok.

test_rabbit_node_test_() ->
  {setup, 
    fun setup/0, fun teardown/1, 
    fun() ->
      ?assertEqual(rabbit@nohost, rabint:rabbit_node()),
      ?assertEqual('random.host', rabint:rabbit_node("random.host"))
    end
  }.