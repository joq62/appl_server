%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_appl_server).

-include("log.api").  
-include("appl_server.hrl").

-define(CheckDelay,1000).
-define(NumCheck,20).
 
%% API

-export([
	 clone_build_release/2,
	 start_release/2,
	 application_dir/2,
	 git_url/2,
	 exec_file_path/2,
	 nodename/2,
	 app/2,
	 all_info/2

	]).


-export([
	 wanted_applications/1,
	 active_applications/1

	]).


%%--------------------------------------------------------------------
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
wanted_applications(SpecDir)->
    {ok,Files}=file:list_dir(SpecDir),
    FilesPath=[filename:join(SpecDir,SpecFile)||SpecFile<-Files],
    L1=[{filename:basename(SpecFile),file:consult(SpecFile)}||SpecFile<-FilesPath,
							      ?FileExt=:=filename:extension(SpecFile)],
    {ok,Host}=net:gethostname(), 
    ApplicationSpecFiles=[File||{File,{ok,[Map]}}<-L1,
				Host=:=maps:get(host,Map)],
    {ok,ApplicationSpecFiles}.	     


%%--------------------------------------------------------------------
%% @doc
%% Active: Node is running and the application responds to ping
%%   
%%   
%% @end
%%--------------------------------------------------------------------
active_applications(SpecsDir)->
    {ok,ApplicationSpecFiles}=wanted_applications(SpecsDir),
    active_applications(ApplicationSpecFiles,SpecsDir,[]).

active_applications([],_SpecsDir,Acc)->
    Active=[ApplicationSpecFile||{ok,ApplicationSpecFile}<-Acc],
    {ok,Active};
active_applications([ApplicationSpecFile|T],SpecsDir,Acc)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    NewAcc=case file:consult(SpecFile) of
	       {error,Reason}->
		   [{error,["Can not read file ",ApplicationSpecFile,SpecsDir,Reason]}|Acc];
	       {ok,[Map]}->
		   %% Check if node is running
		   NodeName=maps:get(nodename,Map),
		   {ok,Host}=net:gethostname(),
		   ApplNode=list_to_atom(NodeName++"@"++Host),
		   case lib_vm:check_started(ApplNode) of
			false->
			    [{error,["Application node is not running",
				     ApplNode,ApplicationSpecFile,SpecsDir]}|Acc];
			true->
			   %% Check if app is running
			   App=maps:get(app,Map),
			   case client:server_pid(App) of
			       {error,["undefined",App]}->
				   [{error,["Does not exists in service_discovery ",
					    App, ApplicationSpecFile,SpecsDir]}|Acc];
			       {ok,ServerPid}->
				   case client:call(ServerPid,{ping,[]},5000) of
				       {error,Reason}->
					   [{error,["Server don not respond to ping",
						    ServerPid,App,ApplicationSpecFile,SpecsDir,Reason]}|Acc];
				       {ok,pong}->
					   [{ok,ApplicationSpecFile}|Acc]
				   end
			   end
		   end
	   end,
    active_applications(T,SpecsDir,NewAcc).
		

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
app(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    {ok,maps:get(app,Map)}
    end. 
   

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
nodename(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    {ok,maps:get(nodename,Map)}
    end. 
   
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
application_dir(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    {ok,maps:get(application_dir,Map)}
    end.
	    
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
git_url(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    {ok,maps:get(git_url,Map)}
    end.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
exec_file_path(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    {ok,maps:get(exec_file_path,Map)}
    end.
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
all_info(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,[Reason]}->
	    {error,[Reason]};
	{ok,[Map]}->
	    {ok,Map}
    end.    

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
clone_build_release(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    ApplDir=maps:get(application_dir,Map),
	    % Clean up old dir
	    file:del_dir_r(ApplDir),
	   % ok=file:make_dir(ApplDir),
	    %% Clone
	    GitUrl=maps:get(git_url,Map),
	    CloenResult=os:cmd("git clone "++GitUrl),
	    {ok,Root}=file:get_cwd(),
	    FullPathApplDir=filename:join(Root,ApplDir),
	    ok=file:set_cwd(FullPathApplDir),
	    {ok,FullPathApplDir}=file:get_cwd(),
	    
	    %% rebar3 release
	    {ok,Rebar3}=case os:getenv("REBAR3") of
			    false ->
				case os:getenv("HOME") of
				    false->
					{error,["Environment variable HOME not set"]};
				    HomePath -> 
					{ok,filename:join(HomePath,"rebar3")}
					    
				end;
			    Path2Rebar3->
				{ok,Path2Rebar3}
			end, 
	    Rebar3Result=os:cmd(Rebar3++" "++"release"),
	    ok=file:set_cwd(Root),
	    {ok,CheckedCwd}=file:get_cwd(),
%	    io:format("Root, FullPathApplDir Rebar3,CheckedCwd,CloenResult,Rebar3Result ~p~n",[{Root, FullPathApplDir,Rebar3,CheckedCwd,CloenResult,Rebar3Result}]),
	    ExecFilePath=maps:get(exec_file_path,Map),
	    ExecFile=filename:join(FullPathApplDir,ExecFilePath),
	    case filelib:is_file(ExecFile) of
		true->
		    ok;
		false->
		    {error,["Failed to clone and build release with result",ApplicationSpecFile,CloenResult]}
	    
	    end
    end.
    

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
start_release(SpecsDir,ApplicationSpecFile)->
    SpecFile=filename:join(SpecsDir,ApplicationSpecFile),
    case file:consult(SpecFile) of
	{error,Reason}->
	    {error,Reason};
	{ok,[Map]}->
	    ApplDir=maps:get(application_dir,Map),
	    ExecFilePath=maps:get(exec_file_path,Map),
	    ExecFile=filename:join(ApplDir,ExecFilePath),
	    case filelib:is_file(ExecFile) of
		false->
		    {error,["Failed to start Application",ApplicationSpecFile,"no execfile"]};
		true->
		    NodeName=maps:get(nodename,Map),
		    {ok,Host}=net:gethostname(),
		    ApplNode=list_to_atom(NodeName++"@"++Host),
		    rpc:call(ApplNode,init,stop,[],6000),
		    true=lib_vm:check_stopped(ApplNode),
		    PrivDir = code:priv_dir(appl_server),
		    ScriptPath = filename:join([PrivDir, "start_release.sh"]),
		    StartResult=os:cmd(ScriptPath++" "++ExecFile++" "++"daemon"),
		    case lib_vm:check_started(ApplNode) of
			false->
			    {error,["Failed to start Application node",ApplNode,ApplicationSpecFile,StartResult]};
			true->
			    case net_kernel:connect_node(ApplNode) of
				false->
				    {error,["Failed to connect to start Application node",ApplNode,ApplicationSpecFile,StartResult]};
				true->
				    {ok,ApplNode}
			    end
		    end
	    end
    end.
					 
