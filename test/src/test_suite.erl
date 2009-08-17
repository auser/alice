-module (test_suite).
-include ("alice.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
    {module, rabint_tests}
  ].