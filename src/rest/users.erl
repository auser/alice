-module (users).
-export ([get/1, post/1, put/1, delete/1]).
-define(JSON_ENCODE(V), mochijson2:encode(V)).

get(Req) ->
  ["users"|Path] = string:tokens(Req:get(path), "/"),
  Out = case Path of
    % /users => All users
    [] -> 
      case rabint:call({rabbit_access_control, list_users, []}) of
        {ok, Bin} -> Bin;
        {_Error, _} -> erlang:list_to_binary([<<"no users">>])
      end;
    % /users/2 => User for id
    [Id] -> Id
  end,
  {"users", Out}.

post(_Req) -> "unhandled".
put(_Req) -> "unhandled".
delete(_Req) -> "unhandled".
