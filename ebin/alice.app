{application, alice, [
        {description, "Alice"},
        {vsn, "0.1"},
        {modules, [alice_app, alice_log, rest_app]},
        {env, [
          {port, 9999},
          {log_path, "./logs/alice.log"},
          {rabbithost, undefined}
        ]},
        {registered, [alice_app, alice_log, rest_app, rest_server]},
        {applications, [kernel, stdlib]},
        {mod, {alice_app, []}}
]}.

