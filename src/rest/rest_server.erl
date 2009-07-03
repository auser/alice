%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (rest_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        
        }).
-define(SERVER, ?MODULE).
-define(JSON_ENCODE(V), mochijson2:encode(V)).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
% TODO: Update port args with config variables
init([Args]) ->
  io:format("Starting ~p with ~p~n", [?MODULE, Args]),
  start_mochiweb(Args),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_mochiweb(Args) ->
  [Port] = Args,
  io:format("Starting mochiweb_http with ~p~n", [Port]),
  mochiweb_http:start([ {port, Port},
                        {loop, fun dispatch_requests/1}]).

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).
  
% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], ""});

handle(Path, Req) ->
  CleanPath = clean_path(Path),
  CAtom = erlang:list_to_atom(top_level_request(CleanPath)),    
  ControllerPath = parse_controller_path(CleanPath),
  
  case CAtom of
    home -> 
      {ok, IndexContents} = file:read_file("web/index.html"),
      Req:ok({"text/html", IndexContents});
    assets -> Req:ok(assets:get(ControllerPath));
    ControllerAtom -> 
      io:format("ControllerPath: ~p~n", [ControllerPath]),
      Body = case Req:get(method) of
        'GET' -> ControllerAtom:get(ControllerPath);
        'POST' -> ControllerAtom:post(ControllerPath, decode_data_from_request(Req));
        'PUT' -> ControllerAtom:put(ControllerPath, decode_data_from_request(Req));
        'DELETE' -> ControllerAtom:delete(ControllerPath, decode_data_from_request(Req));
        Other -> subst("Other ~p on: ~s~n", [users, Other])
      end,
      JsonBody = jsonify(Body),
      Req:ok({"text/json", JsonBody})
  end.

jsonify(JsonifiableBody) ->
  [ ?JSON_ENCODE({
        struct, [
          JsonifiableBody
        ]
    })
  ].
    
% Get the data off the request
decode_data_from_request(Req) ->
  Data = case Req:recv_body() of
    <<>> -> 
      erlang:list_to_binary("{}");
    Body -> Body
  end,
  {struct, Struct} = mochijson2:decode(Data),
  Struct.

subst(Template, Values) when is_list(Values) ->
  list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

% parse the controller path
parse_controller_path(CleanPath) ->
  CPath = case string:tokens(CleanPath, "/") of
    [] -> [];
    [_RootPath|Rest] -> Rest
  end.

% Get a clean path
% strips off the query string
clean_path(Path) ->
  case string:str(Path, "?") of
    0 -> Path;
    N -> string:substr(Path, 1, string:len(Path) - (N+1))
  end.

top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> CleanPath;
    [] -> "home"
  end.