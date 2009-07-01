-module (rabint).
-compile (export_all).
-define(RPC_TIMEOUT, 30000).

% rabint:call({rabbit_access_control, list_users, []})
call({Mod, Fun, Args})    -> rpc_call(Mod, Fun, lists:map(fun list_to_binary/1, Args)).
rpc_call(Mod, Fun, Args)  -> rpc:call(rabbit_node(), Mod, Fun, Args, ?RPC_TIMEOUT).

% Get the local rabbit node
rabbit_node()             -> rabbit_misc:localnode(rabbit).