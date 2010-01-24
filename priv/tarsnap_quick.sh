#!/bin/bash

DATE=`date +%Y-%m-%d.%T`
HOME=$1
SITE=$2

cd $HOME

echo "Starting tarsnap $DATE"

tarsnap --cachedir tarsnap-cache --keyfile www/$SITE/priv/tarsnap/$SITE.tarsnap.key -v -c -f $SITE-var-$DATE www/$SITE/var