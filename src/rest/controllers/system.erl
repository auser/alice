-module (system).
-export ([get/1, post/2, put/2, delete/2]).

% Get system data
get([]) ->   
  {ok, UsedMemory}    = get_used_memory(),
  {ok, PercentMemory} = get_percent_memory(),
  {ok, Messages}      = get_message_count(),
  {ok, SysMemory}     = get_memory_count(),
  {ok, Consumers}     = get_consumers_count(),
  {ok, UnAck}         = get_messages_unacknowledged_count(),
  
  Out = {struct,
    [            
      {system_memory, UsedMemory},
      {percent_memory, PercentMemory},
      {total_queue_memory, SysMemory},
      {consumers, Consumers},
      {messages_unacknowledged, UnAck},
      {total_messages, Messages}
    ]
  },  
  
  {?MODULE, Out};
  
get(_Path) -> {"error", <<"unhandled">>}.

post(_Path, _Data) -> {"error", <<"unhandled">>}.

put(_Path, _Data) -> {"error", <<"unhandled">>}.

delete(_Path, _Data) -> {"error", <<"unhandled">>}.

%%====================================================================
%% MEMORY
%%====================================================================

%%--------------------------------------------------------------------
%% Function: get_percent_memory () -> {ok, PercentMemory}
%% Description: Get the percentage of the total memory available
%%--------------------------------------------------------------------
get_percent_memory() ->
  MemoryList  = memsup:get_system_memory_data(),
  TotalMem    = proplists:get_value(total_memory, MemoryList),
  FreeMem     = proplists:get_value(free_memory, MemoryList),
  Tot         = (FreeMem/TotalMem)*100,
  [Perc|_]    = io_lib:fwrite("~.2f", [Tot]),
  {ok, erlang:list_to_float(Perc)}.

get_used_memory() ->
  MemoryList  = memsup:get_system_memory_data(),
  TotalMem    = proplists:get_value(total_memory, MemoryList),
  FreeMem     = proplists:get_value(free_memory, MemoryList),
  {ok, TotalMem - FreeMem}.

get_message_count() -> {ok, aggregate_vhosts(fun(Data, Sum) -> proplists:get_value(messages, Data) + Sum end, 0)}.
get_consumers_count() -> {ok, aggregate_vhosts(fun(Data, Sum) -> proplists:get_value(consumers, Data) + Sum end, 0)}.
get_messages_unacknowledged_count() -> {ok, aggregate_vhosts(fun(Data, Sum) -> proplists:get_value(messages_unacknowledged, Data) + Sum end, 0)}.
get_memory_count() -> {ok, aggregate_vhosts(fun(Data, Sum) -> proplists:get_value(memory, Data) + Sum end, 0)}.

aggregate_vhosts(Fun, Acc) ->
  {_, VhostListing} = vhosts:get([]),
  
  VhostList = [ erlang:binary_to_list(V) || V <- VhostListing ],
  NumArr = lists:map(
    fun(Vhost) -> 
      % Fetch the queues for this vhost
      case queues:get_info_for(Vhost, [name, memory, messages, consumers, messages_unacknowledged]) of
        []  -> 0;
        E   -> lists:foldl(Fun, Acc, E)
      end
    end, VhostList),
  lists:foldl(fun(X, S) -> X + S end, 0, NumArr).
    