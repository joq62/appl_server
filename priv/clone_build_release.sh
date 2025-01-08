#!/bin/bash
# Support rebar compile
#appdir=$1
#giturl=$2
#
rm -rf erl_cra* rebar3_crashreport;
rm -rf *~ */*~ */*/*~ */*/*/*~;
rm -rf $1
mkdir $1
git clone $2
cd $1
rebar3 release
cd ..
