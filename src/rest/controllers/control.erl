-module (control).
-export ([get/1, post/2, put/2, delete/2]).

% stop      - stops the RabbitMQ application and halts the node
% stop_app  - stops the RabbitMQ application, leaving the node running
% start_app - starts the RabbitMQ application on an already-running node
% reset     - resets node to default configuration, deleting all data
% force_reset
% cluster <ClusterNode> ...
% status
% rotate_logs [Suffix]

get(["status"]) ->
  case rabint:call({rabbit, status, []}) of
    Bin -> 
      {value, {running_applications, JApps}} = lists:keysearch(running_applications, 1, Bin),
      Apps = [ erlang:list_to_binary(erlang:atom_to_list(Atom)) || {Atom, _Name, _Version} <- JApps],
      
      {value, {nodes, ANodes}} = lists:keysearch(nodes, 1, Bin),
      Nodes = [ erlang:list_to_binary(erlang:atom_to_list(Node)) || Node <- ANodes],
      
      {value, {running_nodes, ARunningNodes}} = lists:keysearch(running_nodes, 1, Bin),
      RunningNodes = [ erlang:list_to_binary(erlang:atom_to_list(Node)) || Node <- ARunningNodes],
      
      Jsonable = [{struct, [{"applications", Apps}, {"nodes", Nodes}, {"running_nodes", RunningNodes}]}],
      {"status", Jsonable}
  end;  

get([]) -> {"control",<<"status">>};
get(_Path) -> {"error", <<"unhandled">>}.


post(["stop"], _Data) ->
  rabint:call({rabbit, stop_and_halt, []}),
  {"status", <<"stopped">>};

post(["start_app"], _Data) ->
  rabint:call({rabbit, start, []}),
  {"status", <<"started">>};    
  
post(["reset"], _Data) ->
  rabint:call({rabbit_mnesia, reset, []}),
  {"status", <<"reset">>};
  
post(["force_reset"], _Data) ->
  rabint:call({rabbit_mnesia, force_reset, []}),
  {"status", <<"reset">>};
  
post(["cluster"], Data) ->
  ClusterNodeSs = erlang:binary_to_list(proplists:get_value(<<"nodes">>, Data)),
  ClusterNodes = lists:map(fun list_to_atom/1, ClusterNodeSs),
  rabint:call({rabbit_mnesia, cluster, [ClusterNodes]}),
  {"status", <<"cluster">>};

post(["rotate_logs"], Data) ->
  Prefix = erlang:binary_to_list(proplists:get_value(<<"prefix">>, Data)),
  rabint:call({rabbit, rotate_logs, [Prefix]}),
  {"status", <<"rotated_logs">>};
  
post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(["stop_app"], _Data) ->
  rabint:call({rabbit, stop, []}),
  {"status", <<"stopped">>};

delete(_Path, _Data) -> {"error", <<"unhandled">>}.