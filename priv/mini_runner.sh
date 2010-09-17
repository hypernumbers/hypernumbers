#!/bin/bash

DATE=`date +%Y-%m-%d.%T`

eval $(ssh-agent)

REPO=git@github.com:hypernumbers/hypernumbers.git
HNTOP=/Users/daleharvey/hypernumbers
WEBROOT=/Users/daleharvey/hn_tests/dev-www
TESTDIR=/Users/daleharvey/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP
/usr/local/git/bin/git clone $REPO $TESTDIR

## Compile, and run Hypernumbers
cd $TESTDIR
./hn build
/usr/bin/sed -i '/{127,0,0,1}, 9090}/d' var/sys.config
/usr/bin/sed -i '/{127,0,0,1}, 9091}/d' var/sys.config
./hn start

## Generate Excel Tests
cd $TESTDIR/priv/testserver
ruby regen_tests.rb 1x
ruby regen_tests.rb 2x
cd $TESTDIR

## Generate System Tests
echo $(./hn call 'testsys:generate().')
echo $(./hn call 'test:sys().')

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
echo $(./hn call 'test:excel("2f").')
echo $(./hn call 'test:excel("2g").')
echo $(./hn call 'test:excel("2x").')
echo $(./hn call 'test:excel("2y").')
echo $(./hn call 'test:excel("2z").')

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cp -r $TESTDIR/var/tests/* $LASTRUN
cp $TESTDIR/var/tests/index.html $WEBROOT/$DATE.html

rm -Rf $TESTDIR
