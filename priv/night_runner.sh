#!/bin/bash

DATE=`date +%Y-%m-%d.%T`

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
REPO=git@github.com:hypernumbers/hypernumbers.git
HNTOP=/hn
WEBROOT=/hn/dev-www
TESTDIR=/hn/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP
git clone $REPO $TESTDIR

## Compile, and run Hypernumbers
cd $TESTDIR
./hn build
sed -i '/{127,0,0,1}, 9090}/d' var/sys.config
sed -i '/{127,0,0,1}, 9091}/d' var/sys.config
./hn start

## Generate Excel Tests
cd $TESTDIR/priv/testserver
ruby regen_tests.rb 1x
ruby regen_tests.rb 2x
cd $TESTDIR

## Generate System Tests
echo $(./hn call 'testsys:generate().')

# Run System Tests
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

# Run Security Tests
#echo $(./hn call 'test:security()'.)

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cp -r $TESTDIR/var/tests/* $LASTRUN
cp $TESTDIR/var/tests/index.html $WEBROOT/$DATE.html

rm -Rf $TESTDIR
