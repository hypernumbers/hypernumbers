#!/bin/sh

# Simple cron job to run the test suite nightly and email results to the list:
# $ crontab -l
# 0 1 * * * hassy ~/proj/hn-trunk-for-nightly-testrun/priv/testserver/night-runner.sh 

# <hasan@hypernumbers.com>

# update & build the code
cd ~/proj/hn-trunk-for-nightly-testrun/
svn up
./compile_code
# regenerate tests
cd priv/testserver/
ruby regen_tests.rb 1x
ruby regen_tests.rb 2x
# run tests and email the results
cd ../../tests/common_test-1.3.0/priv/bin
./run_all
cd ../../../../priv/misc/
ruby scrape_and_email.rb ../../common_test-1.3.0/priv/bin/index.html
