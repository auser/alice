-module (users).
-export ([get/1, post/1, put/1, delete/1]).

get(Req) ->
  ["users"|Path] = string:tokens(Req:get(path), "/"),
  Out = case Path of
    % /users => All users
    [] -> get_all_users();
    % /users/2 => User for id
    [Id] -> Id
  end,
  {"users", Out}.

post(Req) -> 
  Data = Req:recv_body(),

  {struct, Struct} = mochijson2:decode(Data),
  
  Username = erlang:binary_to_list(proplists:get_value(<<"username">>, Struct)),
  Password = erlang:binary_to_list(proplists:get_value(<<"password">>, Struct)),

  case rabint:call({rabbit_access_control, add_user, [Username,Password]}) of
    ok -> erlang:list_to_binary([<<"ok">>]);
    {_Error, _} -> erlang:list_to_binary([<<"error">>])
  end,
  {"users", get_all_users()}.
  
put(_Req) -> "unhandled".

delete(Req) -> 
  ["users"|Id] = string:tokens(Req:get(path), "/"),
  case rabint:call({rabbit_access_control, delete_user, [Id]}) of
    ok -> erlang:list_to_binary([<<"ok">>]);
    {_Error, _} -> erlang:list_to_binary([<<"error">>])
  end,
  {"users", get_all_users()}.
  
% PRIVATE

get_all_users() ->
  case rabint:call({rabbit_access_control, list_users, []}) of
    {ok, Bin} -> Bin;
    {_Error, _} -> erlang:list_to_binary([<<"no users">>])
  end.