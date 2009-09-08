-module (test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [
    % {module, hermes_logger_test},
    {module, rabint_tests}
  ].