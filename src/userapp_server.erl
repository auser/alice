-module(userapp_server).
-behaviour(gen_server).

-include("/usr/local/lib/yaws/include/yaws.hrl").
-include("../include/defines.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/1]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    case application:start(yaws) of
        ok -> userapp_server:set_conf(Args);
        Error -> {stop, Error}
    end.

set_conf([Port, WorkingDir]) ->
    ?TRACE("working dir is", WorkingDir),
    GC = #gconf{
        trace = false, logdir = WorkingDir ++ "/logs",
        % yaws = "UserApp 1.0", tmpsir = WorkingDir ++ "/.yaws"
        yaws = "UserApp 1.0"
    },
    SC = #sconf{
        port = Port, servername = "localhost", listen = {0, 0, 0, 0},
        docroot = "/tmp", appmods = [{"/", userapp_handler}]
    },
    ?TRACE("SC is", SC),
    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error -> {stop, Error}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
