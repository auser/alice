-module (users).
-export ([get/1, post/2, put/2, delete/2]).

% add_user        <UserName> <Password>
% delete_user     <UserName>
% change_password <UserName> <NewPassword>
% list_users

get([])         -> {"users", get_all_users()};
get([Id])    -> 
  Users = lists:map(fun(U) -> erlang:binary_to_list(U) end, get_all_users()),
  case lists:member(Id, Users) of
    false ->  {ok, 400, [], {Id, <<"not a user">>}};
    true ->   {"users", erlang:list_to_binary(Id)}
  end;
get(_Path)            -> {"users", ""}.

post([], Json) ->   
  [Username, Password] = get_username_and_password_from_json(Json),
  case rabint:call({rabbit_access_control, add_user, [Username,Password]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"users", Error}
  end;
post(_Path, _Json) -> {"error",<<"Undefined route">>}.

put([Id], _Json) ->
  case rabint:call({rabbit_access_control, change_password, [Id]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"users", Error}
  end;
  
put(_Path, _Json) -> {"error",<<"unhandled">>}.

delete([Id], _Data) ->
  case rabint:call({rabbit_access_control, delete_user, [Id]}) of
    ok -> {"users", get_all_users()};
    {Error, _} -> {"error", Error}
  end;

delete(_Path, _Data) -> {"error", <<"unhandled">>}.

% PRIVATE / INTERNAL
get_username_and_password_from_json(Json) ->
  Username = erlang:binary_to_list(proplists:get_value(<<"username">>, Json)),
  Password = erlang:binary_to_list(proplists:get_value(<<"password">>, Json)),
  [Username, Password].
  
get_all_users() ->
  case rabint:call({rabbit_access_control, list_users, []}) of
    {_Error, _} -> erlang:list_to_binary([<<"no users">>]);
    Bin -> Bin    
  end.