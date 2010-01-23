#!/bin/bash

# export a shell script from svn then execture it
# ./run_svn_script.sh http://svn.example.com/script.sh

SVN_FILE=$1
LOCAL_FILE="./script.sh"

svn export $SVN_FILE $LOCAL_FILE
$LOCAL_FILE
rm $LOCAL_FILE
