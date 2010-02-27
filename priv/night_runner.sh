#!/bin/bash

DATE=`date +%Y-%m-%d.%T`

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export HOME=/home/hypernumbers
REPO=http://svn.hypernumbers.com/hypernumbers/code/trunk
WEBROOT=www/dev.hypernumbers.com
TESTDIR=hn_test_stage
LASTRUN=$HOME/$WEBROOT/tests/last_run/

ERL_CALL=/usr/local/lib/erlang/lib/erl_interface-3.6.1/bin/erl_call 
COOKIE=abc
TARGET=arrian@$(hostname)

cd $HOME

svn co $REPO $HOME/$TESTDIR

cd $TESTDIR

## Compile, and run Hypernumbers
./hypernumbers build
./hypernumbers start -detached
## run detached.

## Generate Excel Tests
cd priv/testserver
ruby regen_tests.rb 1x
ruby regen_tests.rb 2x
cd ../../

## Generate System Tests
echo $($ERL_CALL -name $TARGET -c $COOKIE -a "testsys generate")

## Run tests

# Examples...
#$ERL_CALL -name $TARGET -c $COOKIE -a "test all"
#$ERL_CALL -name $TARGET -c $COOKIE -a "test excel"
#$ERL_CALL -name $TARGET -c $COOKIE -a "test sys"

echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1a"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1b"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1c"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1d"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1e"]')
#$ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["1f"]'

echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2a"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2b"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2c"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2d"]')
echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2e"]')
#$ERL_CALL -name $TARGET -c $COOKIE -a 'test excel ["2f"]'

echo $($ERL_CALL -name $TARGET -c $COOKIE -a 'test sys')

## Cleanup.
./hypernumbers stop

rm -rf $LASTRUN
mkdir $LASTRUN

cd priv/test_visualiser
#ruby visualise_tests.rb
cd ../..

cp -r var/tests/* $LASTRUN
cp var/tests/index.html $HOME/$WEBROOT/tests/$DATE.html

rm -rf $HOME/$TESTDIR
