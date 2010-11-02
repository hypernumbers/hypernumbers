#!/bin/bash

DATE=`date +%Y-%m-%d.%T`

eval $(ssh-agent -s)

REPO=git@github.com:hypernumbers/hypernumbers.git
HNTOP=/Users/daleharvey/hn_tests
WEBROOT=/Users/daleharvey/hn_tests/dev-www
TESTDIR=/Users/daleharvey/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP
echo "\n\ndirectory changed to " $HNTOP "\n\n"
/usr/local/git/bin/git clone $REPO $TESTDIR

## Compile, and run Hypernumbers
cd $TESTDIR
echo "\n\ndirectory changed to " $TESTDIR "\n\n"

./hn build
echo "\n\nmaking the starling driver executable\n\n"
chmod +x lib/starling/ebin/starling_drv
./hn start

## Generate Excel Tests
cd $TESTDIR/priv/testserver
ruby regen_tests.rb 1x
echo "\n\n"
ruby regen_tests.rb 2x
cd $TESTDIR

## Generate System Tests
echo $(./hn call 'testsys:generate().')
echo "\n\n"
echo $(./hn call 'test:sys().')
echo "\n\n"

## Run tests
echo $(./hn call 'test:excel("1a").')
echo "\n\n"
echo $(./hn call 'test:excel("1b").')
echo "\n\n"
echo $(./hn call 'test:excel("1c").')
echo "\n\n"
echo $(./hn call 'test:excel("1d").')
echo "\n\n"
echo $(./hn call 'test:excel("1e").')
echo "\n\n"

echo $(./hn call 'test:excel("2a").')
echo "\n\n"
echo $(./hn call 'test:excel("2b").')
echo "\n\n"
echo $(./hn call 'test:excel("2c").')
echo "\n\n"
echo $(./hn call 'test:excel("2d").')
echo "\n\n"
echo "\n\n"
echo $(./hn call 'test:excel("2e").')
echo "\n\n"
echo $(./hn call 'test:excel("2f").')
echo "\n\n"
echo $(./hn call 'test:excel("2g").')
echo "\n\n"
echo $(./hn call 'test:excel("2x").')
echo "\n\n"
echo $(./hn call 'test:excel("2y").')
echo "\n\n"
echo $(./hn call 'test:excel("2z").')
echo "\n\n"

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cp -r $TESTDIR/var/tests/* $LASTRUN
cp $TESTDIR/var/tests/index.html $WEBROOT/$DATE.html

rm -rf $TESTDIR

# clean up ssh agent
kill $SSH_AGENT_PID
