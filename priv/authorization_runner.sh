#!/bin/bash

HNTOP=/home/vagrant/hypernumbers
WEBROOT=/home/vagrant/hn_tests/dev-www
TESTDIR=/home/vagrant/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP

## Generate System Tests
printf $(./hn call 'test:authorization().')
printf "\n\n"

## Cleanup.
## ./hn stop

