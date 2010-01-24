#!/bin/bash

DATE=`date +%Y-%m-%d.%T`
HOME=$1
SITE=$2

cd $HOME

tarsnap --cachedir tarsnap-cache --keyfile www/$SITE/priv/tarsnap/$SITE.tarsnap.key -c -f $SITE-var-$DATE www/$SITE/var