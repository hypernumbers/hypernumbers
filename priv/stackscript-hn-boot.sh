#!/bin/bash

## This file is place in linode's 'StackScript' called HN Boot Script

## This is version 18 of the stack script
## (this version no will identify the script in github)

#<udf name="hostname" Label="Server hostname", default="setme.hypernumbers.com" >
#<udf name="erlang_url" Label="Erlang Source", default="http://erlang.org/download/otp_src_R14B01.tar.gz" >
#<udf name="tarsnap_url" Label="Tarsnap Source", default="https://www.tarsnap.com/download/tarsnap-autoconf-1.0.28.tgz" >
# <UDF name="db_password" Label="MySQL root Password" />

logfile="/root/log.txt"

#___  ____ ____ _ ____    ____ ____ ___ _  _ ___
#|__] |__| [__  | |       [__  |___  |  |  | |__]
#|__] |  | ___] | |___    ___] |___  |  |__| |
##################################################

echo "starting basic setup" >> $logfile

hostname ${HOSTNAME}
echo ${HOSTNAME} > /etc/hostname

sed -i 's/#deb/deb/g' /etc/apt/sources.list
apt-get -y update
apt-get -y install emacs23-nox ntp nginx libicu-dev \
    rake git-core e2fslibs-dev wordpress mysql-server lynx-cur
/etc/init.d/nginx stop

#____ ____ _    ____ _  _ ____
#|___ |__/ |    |__| |\ | | __
#|___ |  \ |___ |  | | \| |__]
##################################################

echo "getting and building erlang" >> $logfile

apt-get -y build-dep erlang
mkdir -p /tmp/build-erlang
cd $_
wget --tries=10 ${ERLANG_URL} -O erlang.tar.gz
tar xfz erlang.tar.gz
cd `ls -l | egrep '^d' | tr -s ' ' | cut -d' ' -f9`
./configure && make && make install >> $logfile


#___ ____ ____ ____ _  _ ____ ___
# |  |__| |__/ [__  |\ | |__| |__]
# |  |  | |  \ ___] | \| |  | |
##################################################

echo "getting and building tarsnap" >> $logfile

mkdir -p /tmp/build-tarsnap
cd $_
wget --tries=10 ${TARSNAP_URL} --no-check-certificate -O tarsnap.tar.gz
tar xfz tarsnap.tar.gz
cd `ls -l | egrep '^d' | tr -s ' ' | cut -d' ' -f9`
./configure
make all install clean >> $logfile


#_  _ ____ ____ ____ ____
#|  | [__  |___ |__/ [__
#|__| ___] |___ |  \ ___]
##################################################

echo "setting up users" >> $logfile

addgroup admin
echo "%admin ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

## Note, hypernumbers is not a 'root' user
useradd -m -s /bin/bash hypernumbers
mkdir -p /home/hypernumbers/.ssh
echo "Host github.com
    CheckHostIP no
    StrictHostKeyChecking no" > /home/hypernumbers/.ssh/config
echo "-----BEGIN RSA PRIVATE KEY-----

-----END RSA PRIVATE KEY-----" > /home/hypernumbers/.ssh/id_rsa
chown -R hypernumbers:hypernumbers /home/hypernumbers
chmod 600 /home/hypernumbers/.ssh/id_rsa

useradd -m -s /bin/bash -G admin gordon
mkdir -p /home/gordon/.ssh
# echo ssh-rsa add key > /home/gordon/.ssh/authorized_keys
chown -R gordon:gordon /home/gordon

#_  _ _  _    ___  ____ ____ ___ _ ___ _ ____ _  _
#|__| |\ |    |__] |__| |__/  |  |  |  | |  | |\ |
#|  | | \|    |    |  | |  \  |  |  |  | |__| | \|
##################################################

echo "setting up hn partition" >> $logfile

mkdir -p /hn
echo "/dev/xvdc  /hn  ext3  noatime,errors=remount-ro  0  1" >> /etc/fstab
mount -a
chown hypernumbers:hypernumbers /hn


#_  _ _   _ ___  ____ ____ _  _ _  _ _  _ ___  ____ ____ ____
#|__|  \_/  |__] |___ |__/ |\ | |  | |\/| |__] |___ |__/ [__
#|  |   |   |    |___ |  \ | \| |__| |  | |__] |___ |  \ ___]
#############################################################

echo "setting up hypernumbers on hn partition" >> $logfile

su hypernumbers -c 'mkdir -p /hn/dev-www'
su hypernumbers -c 'mkdir -p /hn/files-www'
su hypernumbers -c 'mkdir -p /hn/libs/ebin'
su hypernumbers -c 'mkdir -p /hn/tarsnap'

if [ ! -d "/hn/hypernumbers" ]; then
    cd /hn
    su hypernumbers -c 'git clone git@github.com:hypernumbers/hypernumbers.git'
else
    cd /hn/hypernumbers
    su hypernumbers -c 'git pull'
fi
cd /hn/hypernumbers
su hypernumbers -c './hn build'

rm -Rf /etc/nginx
ln -s /hn/hypernumbers/priv/nginx /etc/nginx
/etc/init.d/nginx restart

chown -R hypernumbers:admin /hn/files-www
chmod -R g+rwx /hn/files-www

su hypernumbers -c '/usr/local/bin/tarsnap --fsck \
    --cachedir /hn/tarsnap/tarsnap-cache \
    --keyfile /hn/hypernumbers/priv/tarsnap/`hostname`.tarsnap.key'

cp -p /hn/hypernumbers/priv/run_github_script /hn
cp -p /hn/hypernumbers/lib/mochiweb/ebin/mochijson.beam /hn/libs/ebin
crontab -u hypernumbers /hn/hypernumbers/priv/cron/standard

#     #
#  #  #  ####  #####  #####  #####  #####  ######  ####   ####
#  #  # #    # #    # #    # #    # #    # #      #      #
#  #  # #    # #    # #    # #    # #    # #####   ####   ####
#  #  # #    # #####  #    # #####  #####  #           #      #
#  #  # #    # #   #  #    # #      #   #  #      #    # #    #
 ## ##   ####  #    # #####  #      #    # ######  ####   ####

echo "setting up wordpress on hn partition" >> $logfile

mkdir /hn/mysql
mkdir /hn/wordpress

ln -s /hn/mysql /var/lib/mysql
ln -s /hn/wordpress /var/www/wordpress
cp -R /usr/share/wordpress/* /hn/wordpress

mysql_install "$DB_PASSWORD" && mysql_tune 40
echo "Mysql installed" >> $logfile

cd /usr/share/doc/wordpress/examples/
chmod +x setup-mysql
./setup-mysql -n wordpress ${HOSTNAME}

echo "Installation finished" >> $logfile
