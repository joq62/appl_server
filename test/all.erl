%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% test1(): Test to read application_specs files 
%%% test2(): config and update service_discovery: No application started 
%%% test3(): Clone_build and start release:
%%% test4(): Kill application node check active
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).



-define(Appl,appl_server).
-define(TestApplFile,"add_test.application").

%%---------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=test1(),
    ok=test2(),
    ok=test3(),
    ok=test4(),
    ok=cleanup(),      
      
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
 %   init:stop(),
    ok.






%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    application:start(cmn_server),
    pong=cmn_server:ping(),
    application:start(log),
    pong=log:ping(),
    application:start(service_discovery),
    pong=service_discovery:ping(),
    application:start(appl_server),
    pong=appl_server:ping(),

    {ok,NodeName}=appl_server:nodename(?TestApplFile),
    "add_test"=NodeName,
    {ok,Host}=net:gethostname(),
    Node=list_to_atom(NodeName++"@"++Host),
    rpc:call(Node,init,stop,[],5000),
    true=lib_vm:check_stopped(Node),

    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]), 
    {ok,"add_test"}=appl_server:application_dir(?TestApplFile),
    {ok,"https://github.com/joq62/add_test.git"}=appl_server:git_url(?TestApplFile),
    {ok,"_build/default/rel/add_test/bin/add_test"}=appl_server:exec_file_path(?TestApplFile),
    {ok,_}=appl_server:all_info(?TestApplFile),    
    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test2()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    {ok,[?TestApplFile]}=appl_server:wanted_applications(),
    {ok,[]}=appl_server:active_applications(),

    {ok,NodeName}=appl_server:nodename(?TestApplFile),
    {ok,Host}=net:gethostname(),
    Node=list_to_atom(NodeName++"@"++Host),
    false=net_kernel:connect_node(Node),
   
    {ok,App}=appl_server:app(?TestApplFile),
    add_test=App,
    service_discovery:config_needed([App]),
    service_discovery:update(),
    timer:sleep(5000),
    {error,["undefined",add_test]}=client:server_pid(App),
    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test3()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% Application not cloned and built 
    {error,["Failed to start Application",?TestApplFile,"no execfile"]}=appl_server:start_release(?TestApplFile),
   
    {ok,NodeName}=appl_server:nodename(?TestApplFile),
    {ok,Host}=net:gethostname(),
    ApplNode=list_to_atom(NodeName++"@"++Host),
    false=net_kernel:connect_node(ApplNode),

    ok=appl_server:clone_build_release(?TestApplFile),

    {ok,ApplNode}=appl_server:start_release(?TestApplFile),
    true=net_kernel:connect_node(ApplNode),
    service_discovery:update(),
    timer:sleep(10*1000),
  
    {ok,App}=appl_server:app(?TestApplFile),
    {ok,ServerPid}=client:server_pid(App),
    {ok,pong}=client:call(ServerPid,{ping,[]},5000),
    {ok,42}=client:call(ServerPid,{add,20,22},5000),
    {ok,[?TestApplFile]}=appl_server:active_applications(),
    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test4()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    %% Kill process
    {ok,App}=appl_server:app(?TestApplFile),
    {ok,ServerPid}=client:server_pid(App),
    {ok,pong}=client:call(ServerPid,{ping,[]},5000),
    true=erlang:exit(ServerPid,kill),
    timer:sleep(2000),
    
    {ok,[]}=appl_server:active_applications(),
   
    {ok,NodeName}=appl_server:nodename(?TestApplFile),
    {ok,Host}=net:gethostname(),
    ApplNode=list_to_atom(NodeName++"@"++Host),
    false=net_kernel:connect_node(ApplNode),
    ok.

%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
cleanup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    {ok,NodeName}=appl_server:nodename(?TestApplFile),
    "add_test"=NodeName,
    {ok,Host}=net:gethostname(),
    Node=list_to_atom(NodeName++"@"++Host),
    rpc:call(Node,init,stop,[],5000),
    true=lib_vm:check_stopped(Node),


    ok.
