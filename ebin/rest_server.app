{application, rest_server, [
        {description, "Rest Server"},
        {vsn, "1"},
        {modules, [rest_server]},
        {registered, [rest_server]},
        {applications, [kernel, stdlib]},
        {mod, {rest_server, []}}
]}.

