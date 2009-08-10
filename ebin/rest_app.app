{application, rest_app, [
        {description, "Rest Server Application"},
        {vsn, "0.1"},
        {modules, [alice_log, rest_app, rest_server]},
        {env, [
          {port, 9999}
        ]},
        {registered, [alice_log, rest_app, rest_server]},
        {applications, [kernel, stdlib, os_mon]},
        {mod, {rest_app, []}}
]}.

