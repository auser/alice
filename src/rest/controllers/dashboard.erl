-module (dashboard).
-export ([get/1, post/2, put/2, delete/2]).

% . aggregate queue size (entries)
% . aggregate queue memory usage
% . rabbitmq process(es) total memory usage
% . available memory on host
get([]) -> 
  {_, VhostListing} = vhosts:get([]),
  
  VhostList = [ erlang:binary_to_list(V) || V <- VhostListing ],
  QueueList = lists:map(
    fun(Vhost) -> 
      % Fetch the queues for this vhost
      Queues = case queues:get([Vhost, "name", "memory", "messages", "consumers", "messages_unacknowledged"]) of
        {queues, Q} -> 
          lists:map(
            fun(Data) ->
              {struct, D} = Data,
              D
            end, Q);
        _E -> []
      end,
      
      % io:format("RECEIVED: ~p~n", [Queues]),
      % Now aggregate their data
      Data = lists:map(
        fun(Prop) ->
          % io:format("Q: ~p~n", [Q]),
          Data = reduce_prop(Prop, convert_prop_for_consumption(lists:flatten(Queues))),
          {Prop, erlang:list_to_binary(erlang:integer_to_list(Data))}
        end, [memory, messages, consumers, messages_unacknowledged]),
        
      % {struct, [{Vhost, {struct, Queues}}]}
      % {struct, [{"queues", Queues}]}
      {struct, [{Vhost, 
        {struct,
          [            
            {queues, [[ {struct, Q} || Q <- Queues ]]},
            {data, {struct, Data}}
          ]
        }}
      ]}
      
    end, VhostList),
  
  {?MODULE, QueueList};

get(["stats"]) -> 
  {_, VhostListing} = vhosts:get([]),

  VhostList = [ erlang:binary_to_list(V) || V <- VhostListing ],
  QueueList = lists:map(
    fun(Vhost) -> 
      % Fetch the queues for this vhost
      Queues = case queues:get([Vhost, [name, memory, messages, consumers, messages_unacknowledged]]) of
        {queues, Q} -> 
          lists:map(
            fun(Data) ->
              {struct, D} = Data,
              D
            end, Q);
        _E -> []
      end,

      % io:format("RECEIVED: ~p~n", [Queues]),
      % Now aggregate their data
      Data = lists:map(
        fun(Prop) ->
          % io:format("Q: ~p~n", [Q]),
          Data = reduce_prop(Prop, convert_prop_for_consumption(lists:flatten(Queues))),
          {Prop, erlang:list_to_binary(erlang:integer_to_list(Data))}
        end, [memory, messages, consumers, messages_unacknowledged]),

      % {struct, [{Vhost, 
      %   {struct,
      %     [
            {struct, Data}
      %     ]
      %   }}
      % ]}
    

    end, VhostList),

  {"stats", QueueList};

  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.



%%====================================================================
%% PRIVATE
%%====================================================================
convert_prop_for_consumption(Prop) ->
  convert_prop_for_consumption(Prop, []).
convert_prop_for_consumption([], Acc) ->
  lists:flatten(Acc);
convert_prop_for_consumption([Prop|Rest], Acc) ->
  convert_prop_for_consumption(Rest, [
  case Prop of
    {vhost, _}                                -> [];
    {"name", _}                               -> [];
    {P, Num}                                  -> {erlang:list_to_atom(P), binary_to_int(Num)};
    _                                         -> []
  end | Acc]).

binary_to_int(Num) ->
  erlang:list_to_integer(erlang:binary_to_list(Num)).
  
reduce_prop(Prop, Arr)      -> reduce_prop(Prop, Arr, 0).
reduce_prop(_Prop, [], Acc)  ->  Acc;
reduce_prop(Prop, [Ele|Rest], Acc) ->
  case Ele of
    {Prop, Num} -> 
      reduce_prop(Prop, Rest, Acc + Num);
    _Else -> 
      reduce_prop(Prop, Rest, Acc)
  end.  
  