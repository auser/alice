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
  case catch rabint:call({rabbit_access_control, list_vhost_permissions, [Vhost]}) of
    {badrpc, {'EXIT', Error}} ->
      case Error of
        {undef, _Arr} ->
          ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
          list_vhost_users(Vhost);
        E -> 
          ?ERROR("Got An error: ~p~n", [E])
      end;
    Bin -> 
      {"permissions", create_json_struct_for(Vhost, [erlang:tuple_to_list(P) || P <- Bin ])}
  end;
    
get([Username]) -> 
  get_user_perms(Username);
get(Path) -> {"error", erlang:list_to_binary("unhandled: "++Path)}.

post([Username], Data) ->
  VHost = extract_vhost(Data),
  CPerm = extract_param("configure", Data),
  WPerm = extract_param("write", Data),
  RPerm = extract_param("read", Data),
  
  case catch rabint:call({rabbit_access_control, set_permissions, [Username, VHost, CPerm, WPerm, RPerm]}) of
    {badrpc, {'EXIT', Error}} ->
      case Error of
        {undef, _Arr} ->
          ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
          map_user_to_vhost(Username, VHost);
        _E -> throw(Error)
      end;
    ok -> get_user_perms(Username)
  end;
  
    
post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete([Username], Data) ->
  VHost = extract_vhost(Data),  
  case catch rabint:call({rabbit_access_control, clear_permissions, [Username, VHost]}) of
    {badrpc, {'EXIT', Error}} ->
      case Error of
        {undef, _Arr} ->
          ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
          unmap_user_from_vhost(Username, VHost);
        _E -> throw(Error)
      end;
    ok -> get_user_perms(Username)
  end;
  
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE
get_user_perms(Username) ->
  case catch rabint:call({rabbit_access_control, list_user_permissions, [Username]}) of
    {badrpc, {'EXIT', Error}} ->
      case Error of
        {undef, _Arr} ->
          ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
          list_user_vhosts(Username);
        _E -> throw(Error)
      end;
    Bin ->
      ResponseList = [erlang:tuple_to_list(P) || P <- Bin ],
      VhostList = [ hd(List) || List <- ResponseList ],
      Out = {struct, [
          {"users", [{struct, [{"name", utils:turn_binary(Username)}, {"vhosts", VhostList}]}]
        }]},
      {?MODULE, Out}
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
%% Utils
%%====================================================================
create_json_struct_for(Vhost, Users) ->
  {struct, [{"vhosts", [{struct, [{"name", utils:turn_binary(Vhost)}, {"users", Users}]}]}]}.
  
%%====================================================================
%% DEPRECATED SUPPORT
%%====================================================================
map_user_to_vhost(Username, Vhost) ->
  O = rabint:call({rabbit_access_control, map_user_vhost, [Username, Vhost]}),
  Out = case O of
    ok -> utils:turn_binary(lists:append(["Mapped ", Username, " to ", Vhost]));
    {error, {no_such_user, _BinUsername}} -> utils:turn_binary(lists:append(["No such user ", Username]));
    {error, UnknownError} -> utils:turn_binary(lists:append(["Unknown error: ", UnknownError]))
  end,
  {?MODULE, Out}.

unmap_user_from_vhost(Username, Vhost) ->
  O = rabint:call({rabbit_access_control, unmap_user_vhost, [Username, Vhost]}),
  Out = case O of
    ok -> utils:turn_binary(lists:append(["Unmapped ", Username, " from ", Vhost]));
    Else -> utils:turn_binary(Else)
  end,
  {?MODULE, Out}.

% Fake it
list_vhost_users(Vhost) ->
  O = rabint:call({rabbit_access_control, list_vhost_users, [Vhost]}),
  Users = lists:map(fun(User) -> [User, <<".*">>, <<".*">>, <<".*">>] end, O),
  {?MODULE, create_json_struct_for(Vhost, Users)}.

list_user_vhosts(Username) ->
  O = rabint:call({rabbit_access_control, list_user_vhosts, [Username]}),
  Out = {struct, [{"users", [{struct, [{"name", utils:turn_binary(Username)}, {"vhosts", O}]}]}]},
  {?MODULE, Out}.