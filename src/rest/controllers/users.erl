-module (users).
-export ([get/1, post/2, put/2, delete/1]).

% add_user        <UserName> <Password>
% delete_user     <UserName>
% change_password <UserName> <NewPassword>
% list_users

get("/users")         -> {"users", get_all_users()};
get("/users/"++Id)    -> {"users", Id};
get(_Path)            -> {"users", ""}.

post("/users", Json) ->   
  [Username, Password] = get_username_and_password_from_json(Json),
  case rabint:call({rabbit_access_control, add_user, [Username,Password]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"users", Error}
  end;
post(_Path, _Json) -> {"error",<<"Undefined route">>}.

put("/users/"++Id, _Json) ->
  case rabint:call({rabbit_access_control, change_password, [Id]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"users", Error}
  end;
  
put(_Path, _Json) -> {"error",<<"unhandled">>}.

delete("/users/"++Id) ->
  case rabint:call({rabbit_access_control, delete_user, [Id]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"error", Error}
  end;

delete(_Route) -> {"error",<<"unhandled">>}.

% PRIVATE / INTERNAL
get_username_and_password_from_json(Json) ->
  Username = erlang:binary_to_list(proplists:get_value(<<"username">>, Json)),
  Password = erlang:binary_to_list(proplists:get_value(<<"password">>, Json)),
  [Username, Password].
  
get_all_users() ->
  case rabint:call({rabbit_access_control, list_users, []}) of
    {ok, Bin} -> Bin;
    {_Error, _} -> erlang:list_to_binary([<<"no users">>])
  end.