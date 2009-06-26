-module(userapp_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    {ok, {{one_for_one, 2, 10}, [
        {userapp_yaws1, {userapp_server, start_link, [Args]}, permanent, 2000, worker, [userapp_server]}
    ]}}.
