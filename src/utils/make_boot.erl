-module(make_boot).
-export([write_scripts/0]).

write_scripts() -> 
        Erts = erlang:system_info(version),
        Version = "0.1",
        {value, {kernel, _, Kernel}} = lists:keysearch(kernel, 1,
                application:loaded_applications()),
        {value, {stdlib, _, Stdlib}} = lists:keysearch(stdlib, 1,
                application:loaded_applications()),

        Rel = "{release, {\"Alice\", \"~s\"}, {erts, \"~s\"}, ["
               "{kernel, \"~s\"}, {stdlib, \"~s\"}, {alice, \"~s\"}]}.",

        {ok, Fs} = file:open("alice.rel", [write]),
        io:format(Fs, Rel, [Version, Erts, Kernel, Stdlib, Version]),
        file:close(Fs),
        
        systools:make_script("alice", [local]),
        halt().


