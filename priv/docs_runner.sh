#!/bin/sh                                                                       

export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
export HOME=/home/hypernumbers
WEBROOT=$HOME/dev.hypernumbers.com
REPO=http://svn.hypernumbers.com/hypernumbers/code/trunk
STAGE=docs_stage

cd $HOME

svn co $REPO $HOME/$STAGE

cd $STAGE

./hypernumbers docs

rm -rf $WEBROOT/docs/hypernumbers-1.0/*
rm -rf $WEBROOT/docs/formula_engine-1.0/*
rm -rf $WEBROOT/docs/read_excel-1.0/*

cp -r lib/hypernumbers-1.0/doc/* $WEBROOT/docs/hypernumbers-1.0
cp -r lib/formula_engine-1.0/doc/* $WEBROOT/docs/formula_engine-1.0
cp -r lib/read_excel-1.0/doc/* $WEBROOT/docs/read_excel-1.0

rm -rf $HOME/$STAGE
