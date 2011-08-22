#!/bin/bash

HNTOP=/home/gordon/hypernumbers
WEBROOT=/home/gordon/hn_tests/dev-www
TESTDIR=/home/gordon/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP

## Generate Excel Tests
cd $HNTOP/priv/testserver
ruby regen_tests.rb 1x
printf "\n\n"
ruby regen_tests.rb 2x
cd $HNTOP

## Generate System Tests
printf $(./hn call 'testsys:generate().')
printf "\n\n"
printf $(./hn call 'test:sys().')
printf "\n\n"
printf $(./hn call 'test:security().')
printf "\n\n"

## Run tests
printf $(./hn call 'test:excel("1a").')
printf "\n\n"
printf $(./hn call 'test:excel("1b").')
printf "\n\n"
printf $(./hn call 'test:excel("1c").')
printf "\n\n"
printf $(./hn call 'test:excel("1d").')
printf "\n\n"
printf $(./hn call 'test:excel("1e").')
printf "\n\n"

printf $(./hn call 'test:excel("2a").')
printf "\n\n"
printf $(./hn call 'test:excel("2b").')
printf "\n\n"
printf $(./hn call 'test:excel("2c").')
printf "\n\n"
printf $(./hn call 'test:excel("2d").')
printf "\n\n"
printf "\n\n"
printf $(./hn call 'test:excel("2e").')
printf "\n\n"

## Cleanup.
./hn stop

