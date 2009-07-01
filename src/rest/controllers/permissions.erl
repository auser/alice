-module (permissions).
-export ([get/1, post/2, put/2, delete/1]).

% set_permissions   [-p <VHostPath>] <UserName> <Regexp> <Regexp> <Regexp>
% clear_permissions [-p <VHostPath>] <UserName>
% list_permissions  [-p <VHostPath>]
% list_user_permissions <UserName>

% TODO: Complete

get("/permissions/"++Id) ->
  case rabint:call({rabbit_access_control, list_user_permissions, [Id]}) of
    {ok, Bin} -> {?MODULE, Bin};
    {Error, _} -> {?MODULE, Error}
  end;
  
get(_Path) -> {"error", <<"unhandled">>}.
post(_Path, _Data) -> {"error", <<"unhandled">>}.
put(_Path, _Data) -> {"error", <<"unhandled">>}.
delete(_Path) -> {"error", <<"unhandled">>}.