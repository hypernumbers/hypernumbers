#!/bin/bash

DATE=`date +%Y-%m-%d.%T`

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export HOME=/home/hypernumbers
REPO=git@github.com:hypernumbers/hypernumbers.git
WEBROOT=www/dev.hypernumbers.com
TESTDIR=hn_test_stage
LASTRUN=$HOME/$WEBROOT/tests/last_run/

cd $HOME

git clone $REPO $HOME/$TESTDIR

cd $TESTDIR

## Compile, and run Hypernumbers
./hn build
./hn start
## run detached.

## Generate Excel Tests
cd priv/testserver
ruby regen_tests.rb 1x
ruby regen_tests.rb 2x
cd ../../

## Generate System Tests
echo $(./hn call 'testsys:generate().')

## Run tests
echo $(./hn call 'test:excel("1a").')
echo $(./hn call 'test:excel("1b").')
echo $(./hn call 'test:excel("1c").')
echo $(./hn call 'test:excel("1d").')
echo $(./hn call 'test:excel("1e").')

echo $(./hn call 'test:excel("2a").')
echo $(./hn call 'test:excel("2b").')
echo $(./hn call 'test:excel("2c").')
echo $(./hn call 'test:excel("2d").')
echo $(./hn call 'test:excel("2e").')

echo $(./hn call 'test:sys().')

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cd priv/test_visualiser
#ruby visualise_tests.rb
cd ../..

cp -r var/tests/* $LASTRUN
cp var/tests/index.html $HOME/$WEBROOT/tests/$DATE.html

rm -rf $HOME/$TESTDIR
