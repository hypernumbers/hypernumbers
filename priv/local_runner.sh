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

## Generate And Run System Tests
printf $(./hn call 'testsys:generate().')
printf "\ntestsys:generate complete\n"
printf $(./hn call 'test:sys().')
printf "\ntest:sys completd\n"

## Run the test of the hypernumbers tests
printf $(./hn call 'test:security().')
printf "\ntest:security complete\n"
printf $(./hn call 'test:auth().')
printf "\ntest:auth complete\n"
printf $(./hn call 'test:authorization().')
printf "\ntest:authorization complete\n"
printf $(./hn call 'test:auth().')
printf "\ntest:auth complete\n"
printf $(./hn call 'test:ztest().')
printf "\ntest:ztest complete\n"

## Run the Excel-compatible tests
printf $(./hn call 'test:excel("1a").')
printf "\ntest:excel 1a complete\n"
printf $(./hn call 'test:excel("1b").')
printf "\ntest:excel 1b complete\n"
printf $(./hn call 'test:excel("1c").')
printf "\ntest:excel 1c complete\n"
printf $(./hn call 'test:excel("1d").')
printf "\ntest:excel 1d complete\n"
printf $(./hn call 'test:excel("1e").')
printf "\ntest:excel 1e complete\n"

printf $(./hn call 'test:excel("2a").')
printf "\ntest:excel 2a complete\n"
printf $(./hn call 'test:excel("2b").')
printf "\ntest:excel 2b complete\n"
printf $(./hn call 'test:excel("2c").')
printf "\ntest:excel 2c complete\n"
printf $(./hn call 'test:excel("2d").')
printf "\ntest:excel 2d complete\n"
printf $(./hn call 'test:excel("2e").')
printf "\ntest:excel 2e complete\n"

## Cleanup.
./hn stop

