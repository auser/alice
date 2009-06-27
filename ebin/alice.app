{application, alice, [
        {description, "Alice - A RESTful interface to rabbitmq"},
        {vsn, "0.1"},
        {modules, [rest_server_app, rest_server_sup, rest_server]},
        {registered, [rest_server]},
        {env, [
          {port, 9999}
        ]},
        {applications, [kernel, stdlib]},
        {mod, {rest_server_app, []}}
]}.