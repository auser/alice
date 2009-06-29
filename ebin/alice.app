{application, alice, [
        {description, "Alice"},
        {vsn, "0.1"},
        {modules, [rest_server]},
        {env, [
          {port, 9999}
        ]},
        {registered, [rest_server]},
        {applications, [kernel, stdlib]},
        {mod, {rest_server, []}}
]}.

