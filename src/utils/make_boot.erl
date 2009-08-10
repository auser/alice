-module(make_boot).
-export([write_scripts/1]).

write_scripts(Args) -> 
  [Name] = Args,
  io:format("write_scripts for ~p~n", [Name]),
  Erts = erlang:system_info(version),
  application:load(sasl),
  application:load(os_mon),
  Version = "0.1",
  {value, {kernel, _, Kernel}} = lists:keysearch(kernel, 1, application:loaded_applications()),
  {value, {stdlib, _, Stdlib}} = lists:keysearch(stdlib, 1, application:loaded_applications()),
  {value, {sasl, _, Sasl}} = lists:keysearch(sasl, 1, application:loaded_applications()),
  {value, {os_mon, _, OsMon}} = lists:keysearch(os_mon, 1, application:loaded_applications()),

  Rel = "{release, {\"~s\", \"~s\"}, {erts, \"~s\"}, ["
         "{kernel, \"~s\"}, {stdlib, \"~s\"}, {sasl, \"~s\"}, {os_mon, \"~s\"}, {~s, \"~s\"}]}.",
  Lowername = string:to_lower(Name),
  Filename = lists:flatten(Lowername ++ ".rel"),
  io:format("Writing to ~p (as ~s)~n", [Filename, Lowername]),
  {ok, Fs} = file:open(Filename, [write]),

  io:format(Fs, Rel, [Name, Version, Erts, 
                          Kernel, 
                          Stdlib, 
                          Sasl, 
                          OsMon,
                          Lowername, Version]),
  file:close(Fs),

  systools:make_script(Lowername, [local]),
  halt().