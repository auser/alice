{application, rest_app, [
        {description, "Rest Server Application"},
        {vsn, "0.1"},
        {modules, [rest_app, rest_server]},
        {env, [
          {port, 9999}
        ]},
        {registered, [rest_app, rest_server]},
        {applications, [kernel, stdlib]},
        {mod, {rest_app, []}}
]}.

