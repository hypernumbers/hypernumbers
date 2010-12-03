#!/bin/bash

echo -e "Running Version 1.0 of mini-runner"

DATE=`date +%Y-%m-%d.%T`

eval $(ssh-agent -s)

REPO=git@github.com:hypernumbers/hypernumbers.git
HNTOP=/Users/daleharvey/hn_tests
WEBROOT=/Users/daleharvey/hn_tests/dev-www
TESTDIR=/Users/daleharvey/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP
echo -e "\n\ndirectory changed to " $HNTOP "\n\n"
/usr/local/git/bin/git clone $REPO $TESTDIR

## Compile, and run Hypernumbers
cd $TESTDIR
echo -e "\n\ndirectory changed to " $TESTDIR "\n\n"

./hn build
echo -e "\n\nmaking the starling driver executable\n\n"
chmod +x $TESTDIR/lib/starling/ebin/starling_drv
ls
./hn start

## Generate Excel Tests
cd $TESTDIR/priv/testserver
ruby regen_tests.rb 1x
echo -e "\n\n"
ruby regen_tests.rb 2x
cd $TESTDIR

## Generate System Tests
echo -e $(./hn call 'testsys:generate().')
echo -e "\n\n"
echo -e $(./hn call 'test:sys().')
echo -e "\n\n"

## Run tests
echo -e $(./hn call 'test:excel("1a").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("1b").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("1c").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("1d").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("1e").')
echo -e "\n\n"

echo -e $(./hn call 'test:excel("2a").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2b").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2c").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2d").')
echo -e "\n\n"
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2e").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2f").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2g").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2x").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2y").')
echo -e "\n\n"
echo -e $(./hn call 'test:excel("2z").')
echo -e "\n\n"

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cp -r $TESTDIR/var/tests/* $LASTRUN
cp $TESTDIR/var/tests/index.html $WEBROOT/$DATE.html

#rm -rf $TESTDIR

# clean up ssh agent
kill $SSH_AGENT_PID
