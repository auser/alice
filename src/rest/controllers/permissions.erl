-module (permissions).
-include ("alice.hrl").
-export ([get/1, post/2, put/2, delete/2]).

% set_permissions   [-p <VHostPath>] <UserName> <Regexp> <Regexp> <Regexp>
% clear_permissions [-p <VHostPath>] <UserName>
% list_permissions  [-p <VHostPath>]
% list_user_permissions <UserName>

% TODO: Complete

get([]) -> 
  % ?MODULE:get(["vhost", "/"]);
  VhostListing = [ erlang:binary_to_list(V) || V <- vhosts:get_all_vhosts()],
  Vhosts = lists:map(fun(V) ->
     get_vhost_perms(V)
    end, VhostListing),
  
  {?MODULE,
    {struct, [
      {vhosts, [ Q || Q <- Vhosts ] }
    ]}
  };
  
get(["vhost", "root"]) -> ?MODULE:get(["vhost", "/"]);
get(["vhost", Vhost]) ->
  {?MODULE, get_vhost_perms(Vhost)};

get([Username]) -> {?MODULE, get_user_perms(Username)};
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
    ok -> {?MODULE, get_user_perms(Username)}
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
    ok -> {?MODULE, get_user_perms(Username)}
  end;
  
delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE
get_user_perms(Username) ->
  VhostListing = case catch rabint:call({rabbit_access_control, list_user_permissions, [Username]}) of
    {badrpc, {'EXIT', Error}} ->
      case Error of
        {undef, _Arr} ->
          ?ERROR("DEPRECATED SUPPORT: To get rid of this message, upgrade to RabbitMQ 1.6", []),
          list_user_vhosts(Username);
        _E -> throw(Error)
      end;
    Bin ->
      [{struct, create_writable_perm_structure(erlang:tuple_to_list(P))} || P <- Bin ]
  end,
  {struct, [
    {name, utils:turn_binary(Username) },
    {vhosts, VhostListing }
  ]}.

get_vhost_perms(Vhost) ->
  U = list_vhost_users(Vhost),
      
  % Now aggregate their data        
  Users = lists:map(fun(User) ->
      UserTuple = create_writable_perm_structure(User),
      {struct, UserTuple }
    end, U),
  {struct, [{"name", utils:turn_binary(Vhost)},{"users", Users}]}.

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
create_writable_perm_structure(Perm) ->
  [Name|Rest] = Perm,
  [Configure|Rest2] = Rest,
  [Write|ReadArr] = Rest2,
  [Read] = ReadArr,
  
  [{"name", Name}, {"configure", Configure}, {"write", Write}, {"read", Read}].
  

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
  O = rabint:call({rabbit_access_control, list_vhost_permissions, [Vhost]}),
  Users = lists:map(fun(User) -> tuple_to_list(User) end, O),
  Users.

list_user_vhosts(Username) ->
  rabint:call({rabbit_access_control, list_user_vhosts, [Username]}).
  
