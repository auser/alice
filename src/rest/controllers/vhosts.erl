-module (vhosts).
-export ([get/1, post/2, put/2, delete/2]).

% add_vhost    <VHostPath>
% delete_vhost <VHostPath>
% list_vhosts

get([])      -> {?MODULE, get_all_vhosts()};
get(_Path)          -> {"error", <<"unhandled vhosts">>}.

post([], Data) ->
  Name = erlang:binary_to_list(proplists:get_value(<<"name">>, Data)),
  case rabint:call({rabbit_access_control, add_vhost, [Name]}) of    
    {Error, _} -> {?MODULE, Error};
    _ -> {?MODULE, get_all_vhosts()}
  end;
  
post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete([Name], _Data) ->
  case rabint:call({rabbit_access_control, delete_vhost, [Name]}) of
    ok -> {?MODULE, get_all_vhosts()};
    {Error, _} -> {?MODULE, Error}
  end;

delete(_Path, _Data) -> {"error", <<"unhandled delete">>}.

% PRIVATE
get_all_vhosts() ->
  case rabint:call({rabbit_access_control, list_vhosts, []}) of
    {_Error, _} -> erlang:list_to_binary([<<"no vhosts">>]);
    Bin -> Bin    
  end.