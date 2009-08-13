-module (permissions).
-include ("alice.hrl").
-export ([get/1, post/2, put/2, delete/2]).

% set_permissions   [-p <VHostPath>] <UserName> <Regexp> <Regexp> <Regexp>
% clear_permissions [-p <VHostPath>] <UserName>
% list_permissions  [-p <VHostPath>]
% list_user_permissions <UserName>

% TODO: Complete

get([]) -> ?MODULE:get(["vhost", "/"]);
get(["vhost", "root"]) -> ?MODULE:get(["vhost", "/"]);
get(["vhost", Vhost]) ->
  case rabint:call({rabbit_access_control, list_vhost_permissions, [Vhost]}) of
    {error, {Atom, _Error}} -> {?MODULE, Atom};
    {child_error, function_clause} -> 
      ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
      list_vhost_users(Vhost);
    % Jsonable = [{struct, [{"applications", Apps}, {"nodes", Nodes}, {"running_nodes", RunningNodes}]}],
    Bin -> {"permissions", [erlang:tuple_to_list(P) || P <- Bin ]}
  end;
  
get([Username]) -> 
  get_user_perms(Username);
get(Path) -> {"error", erlang:list_to_binary("unhandled: "++Path)}.

post([Username], Data) ->
  VHost = extract_vhost(Data),
  CPerm = extract_param("configure", Data),
  WPerm = extract_param("write", Data),
  RPerm = extract_param("read", Data),
  case rabint:call({rabbit_access_control, set_permissions, [Username, VHost, CPerm, WPerm, RPerm]}) of
    {error, {Atom, _Error}} -> {?MODULE, Atom};
    {child_error, function_clause} -> 
      ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
      map_user_to_vhost(Username, VHost);
    ok -> get_user_perms(Username)
  end;
  
post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(["/", Username], Data) ->
  VHost = extract_vhost(Data),
  case rabint:call({rabbit_access_control, clear_permissions, [Username, VHost]}) of    
    {child_error, function_clause} -> 
      ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
      unmap_user_from_vhost(Username, VHost);
    {Error, _} -> {?MODULE, Error};
    ok -> get_user_perms(Username)
  end;
  
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE
get_user_perms(Username) ->
  case rabint:call({rabbit_access_control, list_user_permissions, [Username]}) of    
    {child_error, function_clause} -> 
      ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
      list_user_vhosts(Username);
      
    {_Error, Reason} -> 
      ?ERROR("Got error in rabint call for get_user_perms: ~p~n", [Reason]),
      {?MODULE, erlang:list_to_binary("unknown user: "++Username)};
    % Jsonable = [{struct, [{"applications", Apps}, {"nodes", Nodes}, {"running_nodes", RunningNodes}]}],
    Bin -> {"permissions", [erlang:tuple_to_list(P) || P <- Bin ]}
  end.
  
% ConfigurePerm, WritePerm, ReadPerm
extract_param(Name, Data) ->
  case proplists:get_value(erlang:list_to_binary(Name), Data) of
    undefined -> ".*";
    Bin -> erlang:binary_to_list(Bin)
  end.

extract_vhost(Data) ->
  case proplists:get_value(<<"vhost">>, Data) of
    undefined -> "/";
    Perm -> erlang:binary_to_list(Perm)
  end.
  
  
%%====================================================================
%% DEPRECATED SUPPORT
%%====================================================================
map_user_to_vhost(Username, Vhost) ->
  rabint:call({rabbit_access_control, map_user_vhost, [Vhost, Username]}),
  {?MODULE, lists:append(["Mapped ", Vhost, Username])}.

unmap_user_from_vhost(Username, Vhost) ->
  rabint:call({rabbit_access_control, unmap_user_vhost, [Vhost, Username]}),
  {?MODULE, lists:append(["Unmapped ", Username, Vhost])}.
  
list_vhost_users(Vhost) ->
  O = rabint:call({rabbit_access_control, list_vhost_users, [Vhost]}),
  {?MODULE, O}.

list_user_vhosts(Username) ->
  O = rabint:call({rabbit_access_control, list_user_vhosts, [Username]}),
  {?MODULE, O}.