%%%-------------------------------------------------------------------
%%% File    : rest_server.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Jun 26 17:14:22 PDT 2009
%%%-------------------------------------------------------------------

-module (rest_server).
-behaviour(gen_server).
-include ("alice.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export ([print_banner/0]).
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
init(_Args) ->
  ?INFO("Starting...", []),
	print_banner(),
	?INFO("Starting mochiweb", []),
	Args = lists:map(
      fun (Var) -> 
        case application:get_env(alice, Var) of
          {ok, Value} -> Value;
          _ -> 
            {ok, V} = config:get(Var),
            V
        end
      end,
      [port]),
  start_mochiweb(Args),
  ?INFO("Started mochiweb with: ~p~n", [Args]),
  spawn_link(fun() -> rabint:stay_connected_to_rabbit_node(0) end),
  {ok, #state{}}.


print_banner() ->
		PingStatus = case rabint:ping_rabbit() of
			pang -> 
        Cookie = erlang:get_cookie(),
			  io_lib:fwrite("false\n\tYour cookie is set to ~p\n\tMake sure your rabbitmq server's .erlang.cookie file matches", [Cookie]);
			pong -> "true"
		end,
    io:format("~s~n~s~n~n",
              [?SOFTWARE_NAME, ?COPYRIGHT_MESSAGE]),
    Settings = [{"rabbit node ",         rabint:rabbit_node()},
                {"connected ", PingStatus}],
    DescrLen = lists:max([length(K) || {K, _V} <- Settings]),
    Format = "~-" ++ integer_to_list(DescrLen) ++ "s: ~s~n",
    lists:foreach(fun ({K, V}) -> io:format(Format, [K, V]) end, Settings),
		io:format("---------------------------------\n"),
    io:nl().

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
  mochiweb_http:stop(),
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
  Path = Req:get(raw_path),
  Action = clean_path(Path),
  handle(Action, Req).
  
% Handle the requests
handle("/favicon.ico", Req) -> Req:respond({200, [{"Content-Type", "text/html"}], ""});

handle(Path, Req) ->
  CAtom = erlang:list_to_atom(top_level_request(Path)),
  QuotedControllerPath = parse_controller_path(Path),
  ControllerPath = [mochiweb_util:unquote(Element) || Element <- QuotedControllerPath],
  
  case CAtom of
    home ->
      IndexContents = case file:read_file("web/wonderland/index.html") of
        {ok, Contents} -> Contents;
        _ -> "
          <html><head>
            <title>Wonderland is not installed</title>
            <style type='text/css' media='screen'>
            body { margin: 20px 0 0 0;}

            .container {
              width: 888px;
              margin: 0 auto;
              padding-top: 50px;
              overflow: hidden;
            	text-align: center;
            }

            #header {
            	text-align: center;
            	min-height: 120px;
            	padding: 0;
            	border-bottom: 3px solid #eee;
            }
            
            #header .container {
            	height: 88px;
              width: auto;
              margin: 0;
            }            

            #header h1 a {
              color: #151515;
            	text-align: left;
              font-size: 62px;
              font-family: Palatino, 'Palatino Linotype', Serif;  
              text-decoration: none;
            	padding-left: 100px;	
            }

            #header span {color: #BE3081;}
            #content {width: 600px;text-align: left;}
            </style>
          </head><body>
          <div id=\"header\">
            <div class=\"container\">
              <h1><a href=\"/\">wonder<span>land</span></a></h1>
            </div>
          </div>          
          <div id='content'>
          <div class=\"container\">
          <h2>Wonderland is not installed</h2> 
          To use wonderland, type: <b>make wonderland</b>
          </div></div>
          </body></html>
        "
      end,
      Req:ok({"text/html", IndexContents});
		assets -> Req:ok(assets:get(ControllerPath));
    ControllerAtom -> 
    Meth = clean_method(Req:get(method)),
    case Meth of
      get -> run_controller(Req, ControllerAtom, Meth, [ControllerPath]);
      _ -> run_controller(Req, ControllerAtom, Meth, [ControllerPath, decode_data_from_request(Req)])
    end
  end.


% Call the controller action here
run_controller(Req, ControllerAtom, Meth, Args) ->
  case (catch erlang:apply(ControllerAtom, Meth, Args)) of
    {'EXIT', {undef, _}} = E ->
      ?INFO("(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE,E]),
      Req:ok({"text/html", "Unimplemented controller. There is nothing to see here, go back from where you came"});
    {'EXIT', E} -> 
      ?INFO("(~p:~p) Error in rest server: ~p~n", [?MODULE, ?LINE, E]),
      Req:not_found();
    Body -> 
			JsonBody = case (catch jsonify(Body)) of
				{'EXIT', Error} -> 
					io:format("Error: ~p~n", [Error]),
					Body;
				B -> B
			end,
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
  RecvBody = Req:recv_body(),
  Data = case RecvBody of
    undefined -> erlang:list_to_binary("{}");
    <<>> -> erlang:list_to_binary("{}");
    Bin -> Bin
  end,
  {struct, Struct} = mochijson2:decode(Data),
  Struct.

% Find the method used as a request. 
% This turns 'GET' into get
clean_method(M) ->
  case M of
    % This is a hack... FOR NOW
    'OPTIONS' -> get;
    _ -> erlang:list_to_atom(string:to_lower(erlang:atom_to_list(M)))
  end.

% parse the controller path
parse_controller_path(CleanPath) ->
  case string:tokens(CleanPath, "/") of
    [] -> [];
    [_RootPath|Rest] -> Rest
  end.

% Get a clean path
% strips off the query string
clean_path(Path) ->
  {CleanPath, _, _} = mochiweb_util:urlsplit_path(Path),
  CleanPath.

% Query about the top level request path is
top_level_request(Path) ->
  case string:tokens(Path, "/") of
    [CleanPath|_Others] -> CleanPath;
    [] -> "home"
  end.
