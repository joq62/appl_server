%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(appl_server).

-behaviour(gen_server).
-include("appl_server.hrl").
-include("log.api").
%% Server API
-export([
	 clone_build_release/1,
	 start_release/1,
	 wanted_applications/0,
	 active_applications/0,

	 running/0,
	 update/0
	 
	]).

%% Supporting and testing API
-export([
	 application_dir/1,
	 git_url/1,
	 exec_file_path/1,
	 nodename/1,
	 app/1,

	 all_info/1
	]).

%% System API
-export([ping/0,
	 start_link/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(LoopTime,30*1000).
-record(state, {
	       specs_dir
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%% ------------------- Supporting and testing API -------------------


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec all_info(ApplicationSpecFile::string()) -> {ok,Info::map()} | {error,Reason::term()}.
all_info(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{all_info,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec exec_file_path(ApplicationSpecFile::string()) -> {ok,Path::string()} | {error,Reason::term()}.
exec_file_path(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{exec_file_path,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec nodename(ApplicationSpecFile::string()) -> {ok,NodeName::string()} | {error,Reason::term()}.
nodename(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{nodename,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec app(ApplicationSpecFile::string()) -> {ok,App::atom()} | {error,Reason::term()}.
app(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{app,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec git_url(ApplicationSpecFile::string()) -> {ok,Url::string()} | {error,Reason::term()}.
git_url(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{git_url,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec application_dir(ApplicationSpecFile::string()) -> {ok,Dir::string()} | {error,Reason::term()}.
application_dir(ApplicationSpecFile)-> 
    gen_server:call(?SERVER,{application_dir,ApplicationSpecFile},infinity).



%% ------------------ Server API ------------------------------------


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec active_applications() -> {ok,[ApplicationSpecFile::string()]} | {error,Reason::term()}.
active_applications()-> 
    gen_server:call(?SERVER, {active_applications},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec wanted_applications() -> {ok,[ApplicationSpecFile::string()]} | {error,Reason::term()}.
wanted_applications()-> 
    gen_server:call(?SERVER, {wanted_applications},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec start_release(ApplicationSpecFile::string()) -> {ok,Node::node()} | {error,Reason::term()}.
start_release(ApplicationSpecFile)-> 
    gen_server:call(?SERVER, {start_release,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec clone_build_release(ApplicationSpecFile::string()) -> ok | {error,Reason::term()}.
clone_build_release(ApplicationSpecFile)-> 
    gen_server:call(?SERVER, {clone_build_release,ApplicationSpecFile},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec running() -> [Applications::atom()].
running()-> 
    gen_server:call(?SERVER, {running},infinity).




%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec update() -> ok.
update()-> 
    gen_server:cast(?SERVER, {update}).

%% ------------------ System  API ------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Ping
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong.

ping() ->
    gen_server:call(?SERVER,{ping},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->
    process_flag(trap_exit, true),
    %%Clean up application dirs and Mnesia dirs
%    {ok,Files}=file:list_dir("."),
  %  ApplicationDirs=lib_application:get_application_dirs(Files),
  %  _=[{Dir,file:del_dir_r(Dir)}||Dir<-ApplicationDirs],
  %  MnesiaDirs=lib_application:get_mnesia_dirs(Files),
  %  _=[{Dir,file:del_dir_r(Dir)}||Dir<-MnesiaDirs],

    file:del_dir_r(?SpecsDir),
    case cmn_server:git_update_repo(?SpecsDir) of
	{error,["Dir eexists ",_RepoDir]}->
	    case cmn_server:git_clone(?RepoGit) of
		ok->
		    ?LOG_NOTICE("Repo dir didnt existed so a succesful cloned action is executed",[?SpecsDir]);
		{error,Reason}->
		    ?LOG_WARNING("Failed during clone action ",[Reason])
	    end;
	{error,["Already updated ","application_specs"]}->
	    ok;
	{error,Reason}->
	    ?LOG_WARNING("Failed to update ",[Reason]);
	{ok,Info} ->
	    ?LOG_NOTICE("Repo dir actions",[Info,?SpecsDir]),
	    ok
    end,    
    Self=self(),
    spawn_link(fun()->repo_check_timeout_loop(Self) end),
    ?LOG_NOTICE("Server started ",[?MODULE]),
     
     spawn(fun()->loop() end),
    {ok, #state{
	    specs_dir=?SpecsDir
	  
	   }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

%% ------------------ Server API ------------------------------------

handle_call({wanted_applications}, _From, State) ->
    Reply=lib_appl_server:wanted_applications(State#state.specs_dir),
    {reply, Reply, State};

handle_call({active_applications}, _From, State) ->
    Reply=lib_appl_server:active_applications(State#state.specs_dir),
    {reply, Reply, State};



handle_call({clone_build_release,ApplicationSpecFile}, _From, State) ->
    Reply=lib_appl_server:clone_build_release(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};


handle_call({start_release,ApplicationSpecFile}, _From, State) ->
    Reply=lib_appl_server:start_release(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};

%% ------------------- Supporting and testing API -------------------

handle_call({app,ApplicationSpecFile}, _From, State) ->
    Reply = lib_appl_server:app(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};

handle_call({nodename,ApplicationSpecFile}, _From, State) ->
    Reply = lib_appl_server:nodename(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};

handle_call({all_info,ApplicationSpecFile}, _From, State) ->
    Reply = lib_appl_server:all_info(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};


handle_call({exec_file_path,ApplicationSpecFile}, _From, State) ->
     Reply = lib_appl_server:exec_file_path(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};


handle_call({git_url,ApplicationSpecFile}, _From, State) ->
     Reply = lib_appl_server:git_url(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};

handle_call({application_dir,ApplicationSpecFile}, _From, State) ->
    Reply = lib_appl_server:application_dir(State#state.specs_dir,ApplicationSpecFile),
    {reply, Reply, State};

%% ------------------ System  API ------------------------------------

handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
   ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_cast({update}, State) ->

    spawn(fun()->loop() end),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({timeout,check_repo_update}, State) ->
    case cmn_server:git_is_repo_updated(?SpecsDir) of
	true->
	    ok;
	{error,["RepoDir doesnt exists, need to clone",
		?SpecsDir]}->
	    case cmn_server:git_clone(?RepoGit) of
		ok->
		    ?LOG_NOTICE("Repo dir didnt existed so a succesful cloned action is executed",[?SpecsDir]);
		{error,Reason}->
		    ?LOG_WARNING("Failed during clone action ",[Reason])
	    end;
	false->
	    case cmn_server:git_update_repo(?SpecsDir) of
		{error,["Already updated ",?SpecsDir]}->
		    ok;
		{error,Reason}->
		    ?LOG_WARNING("Failed to update ",[Reason]);
		{ok,Info} ->
		    ?LOG_NOTICE("Repo dir actions",[Info,?SpecsDir]),
		    ok
	    end
    end,
    {noreply, State};
	
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop()->
    timer:sleep(?LoopTime),
    rpc:cast(node(),?MODULE,update,[]).


repo_check_timeout_loop(Parent)->
    timer:sleep(?CheckRepoInterval),
    Parent!{timeout,check_repo_update},
    repo_check_timeout_loop(Parent).
