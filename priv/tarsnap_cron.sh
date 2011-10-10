#!/bin/bash

DATE=`date +%Y-%m-%d.%T`
SITE=`/bin/hostname`

echo $SITE

cd /hn/tarsnap

/usr/local/bin/tarsnap --cachedir /hn/tarsnap/tarsnap-cache --keyfile /hn/hypernumbers/priv/tarsnap/$SITE.tarsnap.key -v -c -f $SITE-var-$DATE /hn/hypernumbers/var 2>&1

echo "finished " $DATE