-module (vhosts).
-export ([get/1, post/2, put/2, delete/1]).

% add_vhost    <VHostPath>
% delete_vhost <VHostPath>
% list_vhosts

get("/vhosts")      -> {?MODULE, get_all_vhosts()};
get(_Path)          -> {"error", <<"unhandled">>}.

post("/vhosts", Data) ->
  Name = erlang:binary_to_list(proplists:get_value(<<"name">>, Data)),
  case rabint:call({rabbit_access_control, add_vhost, [Name]}) of
    ok -> {?MODULE, get_all_vhosts()};
    {Error, _} -> {?MODULE, Error}
  end;
  
post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete("/vhosts/"++Name) -> 
  case rabint:call({rabbit_access_control, delete_vhost, [Name]}) of
    ok -> {?MODULE, get_all_vhosts()};
    {Error, _} -> {?MODULE, Error}
  end;

delete(_Path) -> {"error", <<"unhandled">>}.

% PRIVATE
get_all_vhosts() ->
  case rabint:call({rabbit_access_control, list_vhosts, []}) of
    {ok, Bin} -> Bin;
    {_Error, _} -> erlang:list_to_binary([<<"no vhosts">>])
  end.