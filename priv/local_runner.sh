#!/bin/bash

HNTOP=/home/vagrant/hypernumbers
WEBROOT=/home/vagrant/hn_tests/dev-www
TESTDIR=/home/vagrant/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP

## Generate Excel Tests
cd $HNTOP/priv/testserver
ruby regen_tests.rb 1x
printf "\n\n"
ruby regen_tests.rb 2x
cd $HNTOP

## Generate System Tests
printf "Generate Tests\n"
printf $(./hn call 'testsys:generate().')
printf "\n\nGenerate Fuzz Tests\n"
printf $(./hn call 'test:generate_fuzz_tests().')
printf "\n\nRun Systems Tests\n"
printf $(./hn call 'test:sys().')
printf "\n\nRun Security Tests\n"
printf $(./hn call 'test:security().')

printf "\n\nRun Excel 1a Tests\n"
printf $(./hn call 'test:excel("1a").')
printf "\n\nRun Excel 1b Tests\n"
printf $(./hn call 'test:excel("1b").')
printf "\n\nRun Excel 1c Tests\n"
printf $(./hn call 'test:excel("1c").')
printf "\n\nRun Excel 1d Tests\n"
printf $(./hn call 'test:excel("1d").')
printf "\n\nRun Excel 1e Tests\n"
printf $(./hn call 'test:excel("1e").')
printf "\n\nRun Excel 2a Tests\n"
printf $(./hn call 'test:excel("2a").')
printf "\n\nRun Excel 2b Tests\n"
printf $(./hn call 'test:excel("2b").')
printf "\n\nRun Excel 2c Tests\n"
printf $(./hn call 'test:excel("2c").')
printf "\n\nRun Excel 2d Tests\n"
printf $(./hn call 'test:excel("2d").')
printf "\n\nRun Excel 2e Tests\n"
printf $(./hn call 'test:excel("2e").')

printf "\n\nRun Auth Tests\n"
printf $(./hn call 'test:auth().')
printf "\n\nRun Authorizaton Tests"
printf $(./hn call 'test:authorization().')
printf "\n\nRun ZTests"
printf $(./hn call 'test:ztest().')
printf "\n\nRun Fuzz Tests"
printf $(./hn call 'test:fuzz().')
printf "\n\nFinally Cleanup\n"

## Cleanup.
./hn stop

printf "Over and out...\n"

