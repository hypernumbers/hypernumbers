#!/bin/bash

DATE=`date +%Y-%m-%d.%T`
SITE=`/bin/hostname`

cd /hn/tarsnap

/usr/local/bin/tarsnap \
    --cachedir tarsnap-cache \
    --keyfile /hn/hypernumbers/priv/tarsnap/$SITE.tarsnap.key \
    -v -c -f $SITE-var-$DATE /hn/hypernumbers/var