#!/bin/bash

if [ -f  "/var/vagrant_provision" ]; then
	exit 0
fi

HOSTNAME="hypernumbers.dev"
ERLANG_URL="http://erlang.org/download/otp_src_R14B04.tar.gz"
TARSNAP_URL="https://www.tarsnap.com/download/tarsnap-autoconf-1.0.28.tgz"
logfile="/root/log.txt"

#___  ____ ____ _ ____    ____ ____ ___ _  _ ___
#|__] |__| [__  | |       [__  |___  |  |  | |__]
#|__] |  | ___] | |___    ___] |___  |  |__| |
##################################################

echo "starting basic setup" >> $logfile

hostname $HOSTNAME
echo $HOSTNAME > /etc/hostname
echo "127.0.0.1 hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 tests.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 auth.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 sys.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 security.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 a.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 b.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 c.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 d.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 e.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 f.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 g.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 h.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 i.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 j.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 k.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 l.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 m.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 n.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 o.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 p.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 q.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 r.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 s.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 t.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 u.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 v.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 w.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 x.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 y.hypernumbers.dev" >> /etc/hosts
echo "127.0.0.1 z.hypernumbers.dev" >> /etc/hosts

sed -i 's/#deb/deb/g' /etc/apt/sources.list 
sed -i 's/deb cdrom/# deb cdrom/g' /etc/apt/sources.list
apt-get -y update >> $logfile
apt-get -y install xemacs21 ntp libicu-dev \
    rake git-core e2fslibs-dev lynx-cur nginx >> $logfile

#____ ____ _    ____ _  _ ____
#|___ |__/ |    |__| |\ | | __
#|___ |  \ |___ |  | | \| |__]
##################################################

echo "getting and building erlang" >> $logfile

apt-get -y build-dep erlang >> $logfile
apt-get -y install libwxbase2.8 libwxgtk2.8-dev libqt4-opengl-dev libgtk2.0-dev >> $logfile
mkdir -p /tmp/build-erlang
cd $_
wget --tries=10 $ERLANG_URL -O erlang.tar.gz >> $logfile
tar xfz erlang.tar.gz
cd `ls -l | egrep '^d' | tr -s ' ' | cut -d' ' -f9`
echo "configure erlang" >> $logfile
./configure >> $logfile
echo "make erlang" >> $logfile
make >> $logfile
echo "install erlang" >> $logfile
make install >> $logfile

#_  _ ____ ____ ____ ____
#|  | [__  |___ |__/ [__
#|__| ___] |___ |  \ ___]
##################################################

echo "setting up users" >> $logfile

addgroup admin
echo "%admin ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

## Note, hypernumbers is not a 'root' user
useradd -m -s /bin/bash hypernumbers

#_  _ _  _    ___  ____ ____ ___ _ ___ _ ____ _  _
#|__| |\ |    |__] |__| |__/  |  |  |  | |  | |\ |
#|  | | \|    |    |  | |  \  |  |  |  | |__| | \|
##################################################

echo "setting up hn partition" >> $logfile

mkdir -p /hn
chown hypernumbers:hypernumbers /hn

#_  _ _   _ ___  ____ ____ _  _ _  _ _  _ ___  ____ ____ ____
#|__|  \_/  |__] |___ |__/ |\ | |  | |\/| |__] |___ |__/ [__
#|  |   |   |    |___ |  \ | \| |__| |  | |__] |___ |  \ ___]
#############################################################

echo "setting up hypernumbers on hn partition" >> $logfile

mkdir -p /hn/libs/ebin

if [ ! -d "/hn/hypernumbers" ]; then
    cd /hn
    git clone https://github.com/hypernumbers/hypernumbers.git >> $logfile
else
    cd /hn/hypernumbers
    git pull >> $logfile
fi
cd /hn/hypernumbers
./hn build >> $logfile

rm -Rf /etc/nginx
ln -s /hn/hypernumbers/priv/nginx /etc/nginx
/etc/init.d/nginx restart

chown -R vagrant:vagrant /hn/hypernumbers
chown -R vagrant:vagrant /hn/libs

cp -p /hn/hypernumbers/priv/run_github_script /hn
cp -p /hn/hypernumbers/lib/mochiweb/ebin/mochijson.beam /hn/libs/ebin

##
## Vagrant stuff
##

apt-get -y install firefox chromium-bsu>> $logfile
echo "# set up X Windows to export to host" > /home/vagrant/.bashrc
echo "export DISPLAY=10.0.2.2:0" > /home/vagrant/.bashrc

touch /var/vagrant_provision

echo "Installation finished" >> $logfile
