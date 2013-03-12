#!/bin/bash

HNTOP=/home/gordon/hypernumbers
WEBROOT=/home/gordon/hn_tests/dev-www
TESTDIR=/home/gordon/hn_tests/test-hypernumbers
LASTRUN=$WEBROOT/last_run

cd $HNTOP

## Generate System Tests
printf $(./hn call 'test:authorization().')
printf "\n\n"

## Cleanup.
## ./hn stop

