-module (rabint_tests).
-include ("alice.hrl").
-include_lib("eunit/include/eunit.hrl").

rabbit_node_test_() ->
  ?assertEqual(rabbit@nohost, rabint:rabbit_node()),
  ?assertEqual('user@somedomain.com', rabint:rabbit_node("user@somedomain.com")),
  ?assertEqual('random.host', rabint:rabbit_node("random.host")).