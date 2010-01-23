#!/bin/bash

FILE=$1

svn co $FILE ./script.sh
./script.sh
rm ./script.sh
