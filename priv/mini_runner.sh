#!/bin/bash

printf "Running Version 1.0 of mini-runner"

DATE=`date +%Y-%m-%d.%T`

eval $(ssh-agent -s)

REPO=git@github.com:hypernumbers/hypernumbers.git
HNTOP=/Users/daleharvey/hn_tests
WEBROOT=/Users/daleharvey/hn_tests/dev-www
TESTDIR=/Users/daleharvey/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP
printf "\n\ndirectory changed to " $HNTOP "\n\n"
printf $(/usr/local/git/bin/git clone $REPO $TESTDIR)

## Compile, and run Hypernumbers
cd $TESTDIR
printf "\n\ndirectory changed to " $TESTDIR "\n\n"

printf $(./hn build)
printf "\n\nmaking the starling driver executable\n\n"
printf $(chmod +x $TESTDIR/lib/starling/ebin/starling_drv)

printf $(./hn start)

## Generate Excel Tests
cd $TESTDIR/priv/testserver
ruby regen_tests.rb 1x
printf "\n\n"
ruby regen_tests.rb 2x
cd $TESTDIR

## Generate System Tests
printf $(./hn call 'testsys:generate().')
printf "\n\n"
printf $(./hn call 'test:sys().')
printf "\n\n"

## Run tests
#printf $(./hn call 'test:excel("1a").')
#printf "\n\n"
#printf $(./hn call 'test:excel("1b").')
#printf "\n\n"
#printf $(./hn call 'test:excel("1c").')
#printf "\n\n"
#printf $(./hn call 'test:excel("1d").')
#printf "\n\n"
#printf $(./hn call 'test:excel("1e").')
#printf "\n\n"

printf $(./hn call 'test:excel("2a").')
printf "\n\n"
printf $(./hn call 'test:excel("2b").')
printf "\n\n"
#printf $(./hn call 'test:excel("2c").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2d").')
#printf "\n\n"
#printf "\n\n"
#printf $(./hn call 'test:excel("2e").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2f").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2g").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2x").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2y").')
#printf "\n\n"
#printf $(./hn call 'test:excel("2z").')
#printf "\n\n"

## Cleanup.
./hn stop

rm -rf $LASTRUN
mkdir -p $LASTRUN

cp -r $TESTDIR/var/tests/* $LASTRUN
cp $TESTDIR/var/tests/index.html $WEBROOT/$DATE.html

##rm -rf $TESTDIR

# clean up ssh agent
kill $SSH_AGENT_PID
