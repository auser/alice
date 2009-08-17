-define(SOFTWARE_NAME, "------------- Alice ------------").
-define(COPYRIGHT_MESSAGE, "Copyright (C) 2009 Ari Lerner, Nate Murray, Michael Fairchild, CloudTeam").
-define (VERSION, "0.0.2").

-define (FMT_MSG (Msg, Args), lists:flatten([?MODULE, ?LINE, io_lib:format(Msg, Args)])).
-define (INFO (Msg, Args),    alice_log:info(Msg, Args)).
-define (ERROR (Msg, Args),   alice_log:error(Msg, Args)).

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

-define (TESTING, true).

-define (CONFIG_FILE, case ?TESTING of
  true -> "include/config.cfg";
  false -> "/etc/alice/alice.cfg"
end).
