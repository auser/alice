-module (dev).
-include ("alice.hrl").
-export ([get/1, post/2, put/2, delete/2]).

get([]) -> {"hello", <<"world">>};
get(["update"]) -> 
  Pwd = filename:absname(""),
  Cmd = lists:flatten([lists:append([
                                      ["cd ", Pwd], [" && "], 
                                      ["git pull origin master"], [" && "],
                                      ["make && make boot"], [" && "],
                                      ["echo 'done'"]
                                    ])]),
  os:cmd(Cmd),
  ?INFO("Hot reloaded Alice code at the behest of Wonderland~n", []),
  {"hello", <<"world">>};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.