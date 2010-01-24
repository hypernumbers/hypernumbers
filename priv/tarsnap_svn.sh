#!/bin/bash

DATE=`date +%Y-%m-%d.%T`
HOME=$1
SITE=alpha.hypernumbers.com

TO_BACKUP="/home/hypernumbers/svn /home/hypernumbers/trac.hypernumbers.com"

cd $HOME

echo "Starting tarsnap $DATE"

/usr/local/bin/tarsnap \
    --cachedir tarsnap-cache \
    --keyfile www/$SITE/priv/tarsnap/$SITE.tarsnap.key \
    -v -c -f svn_and_trac-$DATE $TO_BACKUP
    