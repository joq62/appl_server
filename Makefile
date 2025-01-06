all:	 
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf add_test;
	rm -rf test.rebar;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_specs;
	#INFO: Compile application
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	cp config/rebar.config .;
	rebar3 compile;
	rm -rf _build;
	rm -rf rebar.lock
	git status
	echo Ok there you go!
	#INFO: no_ebin_commit ENDED SUCCESSFUL
clean:
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf test.rebar;
	rm -rf add_test;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_specs;
	#INFO: Compile application
	cp config/rebar.config .;
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	rebar3 compile;
	rm -rf _build;
	rm -rf rebar.lock
#INFO: clean ENDED SUCCESSFUL
eunit:
	rm -rf erl_cra* rebar3_crashreport;
	rm -rf *~ */*~ */*/*~ */*/*/*~;
	rm -rf test_ebin;
	rm -rf *.beam */*.beam;
	rm -rf add_test;
	rm -rf test.rebar;
	rm -rf logs;
	rm -rf Mnesia.*;
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock;
	rm -rf *_specs;
#INFO: Creating eunit test code using test_ebin dir;
	rm -rf common_include;
	cp -r ~/erlang/common_include .
	mkdir test_ebin;
	cp test_config/test.rebar.config rebar.config;
	erlc  -I common_include -o test_ebin test/*.erl;
	rebar3 compile
	#INFO: Starts the eunit testing .................
	erl -pa test_ebin\
	 -pa _build/default/lib/cmn_server/ebin\
	 -pa _build/default/lib/log/ebin\
	 -pa _build/default/lib/service_discovery/ebin\
	 -pa _build/default/lib/$(appl)/ebin\
	 -sname connect\
	 -run $(m) start\
	 -setcookie a
