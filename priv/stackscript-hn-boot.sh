#!/bin/bash

## This file is place in linode's 'StackScript' called HN Boot Script

#<udf name="hostname" Label="Server hostname", default="setme.hypernumbers.com" >
#<udf name="erlang_url" Label="Erlang Source", default="http://erlang.org/download/otp_src_R13B04.tar.gz" >
#<udf name="tarsnap_url" Label="Tarsnap Source", default="https://www.tarsnap.com/download/tarsnap-autoconf-1.0.26.tgz" >

#___  ____ ____ _ ____    ____ ____ ___ _  _ ___  
#|__] |__| [__  | |       [__  |___  |  |  | |__] 
#|__] |  | ___] | |___    ___] |___  |  |__| |    
##################################################

hostname ${HOSTNAME}
echo ${HOSTNAME} > /etc/hostname

sed -i 's/#deb/deb/g' /etc/apt/sources.list
apt-get -y update
apt-get -y install emacs22-nox ntp nginx libicu-dev \
    rake git-core e2fslibs-dev
/etc/init.d/nginx stop


#____ ____ _    ____ _  _ ____ 
#|___ |__/ |    |__| |\ | | __ 
#|___ |  \ |___ |  | | \| |__] 
##################################################                              

apt-get -y build-dep erlang
mkdir -p /tmp/build-erlang
cd $_
wget --tries=10 ${ERLANG_URL} -O erlang.tar.gz
tar xfz erlang.tar.gz
cd `ls -l | egrep '^d' | tr -s ' ' | cut -d' ' -f9`
./configure && make && make install


#___ ____ ____ ____ _  _ ____ ___  
# |  |__| |__/ [__  |\ | |__| |__] 
# |  |  | |  \ ___] | \| |  | |    
##################################################

mkdir -p /tmp/build-tarsnap
cd $_
wget --tries=10 ${TARSNAP_URL} --no-check-certificate -O tarsnap.tar.gz
tar xfz tarsnap.tar.gz
cd `ls -l | egrep '^d' | tr -s ' ' | cut -d' ' -f9`
./configure
make all install clean


#_  _ ____ ____ ____ ____ 
#|  | [__  |___ |__/ [__  
#|__| ___] |___ |  \ ___] 
##################################################

addgroup admin
echo "%admin ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

## Note, hypernumbers is not a 'root' user
useradd -m -s /bin/bash hypernumbers
mkdir -p /home/hypernumbers/.ssh
echo "Host github.com
    CheckHostIP no
    StrictHostKeyChecking no" > /home/hypernumbers/.ssh/config
echo "-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAxvXUr0LE/e9nZlhxm6gQuA0Msc6GTRD5RHzsuBBA3Gex6mhX
AGUSmgke3DGwXK+F21SWZAYu1Yx9P/igsTBWX/aC0QHXb4KX1BdBHmKz1LhNeaFs
I+KpwWez1xF6pzaAMcTggrrTGXpDQ8ASfeHy20NROedg+H/T8RuKRSY4w/nDhdnA
Xlt/e6lIZdOq1K8F8pSXNnwZETcheXo1iHpx7uE2PDIfiOpyq9reb39GsZj1bGAh
kBBjnzCzTMJRM/wO1hqQ0jv9uYVHVnP/iHSxC5JupA9n1AzKvj2sWw5EqwcMH04P
8WHfeiu6KHylL/AWXL4r1j1xwABQ31VZEoYyoQIBIwKCAQEApNpRI4B3Vg+Bj1CY
pYtBDYcnxoaMiP9vc0LwBjlaUDipW9LLvn+hstRbZf1I/FbrQLPNEQxv8sTczprO
Sau1SDKm57ERluiMbedh11kYqPB6tTybbjDVzCK5lPE5vb9xiFKrZQEykXPulzi+
3VTQicoQGgjbUY58aLBrQJwDJgf/ySD5E7sBMYTn/uaTI154FQbWQ0L6fvrumSUj
H1pUzjqxHF+IqHBKXlk9JZE8/CNpBRdeNQYNwlVzp0maIiqX2IJ/nFz+NE2uUcgS
tRKUWik6KrFK0frxHfhAW2/JP0Whc8ARhjXUFYpuXNZDri6sfYPwic4ossHjNUsS
l4BoywKBgQDrhke4f4xL4aKtkrj29kZh9uiQDRm4Tr29v8aprsXdf9I1bJ45m0yn
OI5MdN2gqnCEd71c8oXMgY3uA5Y7UWKehk3PeqIiTVLMCuc/3BXI4TkZuZFQOYzu
vF+BzXjnyjChFMyKEq6XpkXVI1jjngnM8+x/XRpluIwZ2u3KPrNyWQKBgQDYQctW
2KbitomC1G5akGligTEEkZX0pM70UFHCkI4uFQ7J8/R19jDBJL2OzUqjC9GRGqEM
S+RjMxhUL9NBYRaVvUDpiTtHhVkkfuq6wC4AJ3cF4xrgnRM0lxOn6kNp/mfl+mTA
KxE2NXydnewyF/STttopEJz+OIo2dKMFoWPpiQKBgH/bLj+Vso+XvrX/MTWboozz
wBOvVxro6qjV0j7bODZp9cU69s7QpfRgh8Ma3rZN5U85r/Cg6YxGVFyi3IaZ5RQ6
R39CkoemUYS1djicC9Ny8xyX78x3A10HLIhK+H3UKQb8qYzIUCZwNImleWWe72Ch
rEUj6cIw+5kB0YsMGEVjAoGAGLcP7KsLwiN9bgmmNjxj0L5OvrGNewuFXcAJWBCF
RxhZdiqCVp/LDsJezn3cpOtoaFrQk7Dm5sQCx8rzknF+7IqoVTRBSgCceTMTgw6m
K+dAzXkKYtAfc7l+ltjxwviA5xVNV8poXfd79MGXVi6fm9piE1JbFb1Rn9LQzXGH
wusCgYEAtqPFXK0gp0zlnqKLaK2yd9ZPL4EplHuf/Wz8/gVI4K92S8v9L/OtwVp+
qZt6yuTtmxYcbBUxgTb9xVZNwccEgfyguImItsdMbLlTnSI6JUaiplSqHloW50pl
pBcUDegk435x/3CGOGWowghNuLgnxyPsn8w2D8jWiB1KiWti9YU=
-----END RSA PRIVATE KEY-----" > /home/hypernumbers/.ssh/id_rsa
chown -R hypernumbers:hypernumbers /home/hypernumbers
chmod 600 /home/hypernumbers/.ssh/id_rsa

useradd -m -s /bin/bash -G admin gordon
mkdir -p /home/gordon/.ssh
echo ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAovtLtzzV7AmNON8SLJGZLS3FH2xlitAMg0CWeuaQ/EAujZ8mB97b4PXrrsYyVQ8wV+Ucv7MUKJPIazp1zgJTK7Cjmv1Ln5uMjG4wo2Q33FXT/3TBWLUzweA6MffnqsTiuTwYaTxNSvIzLp+/UthbmZWV7nTu1JwdwAPzgdvXx5mX/xxyjqvbbcf8T8HOhEp4JafnY1zNrBjwJ1orRjM1A13whDr+wC38KjmEuZjk+zNYX30uwCu4u18rhomAXIJBZfY7L5KhAP8L7Ks/5CFpqfgV4BdWpcC3BHxDrFguZmrlfQ2Icco26DpyWFl4YMy8oPdXSw0LhLVNfA/hYHsocw== gordonguthrie@hypernumbers.com > /home/gordon/.ssh/authorized_keys
chown -R gordon:gordon /home/gordon

useradd -m -s /bin/bash -G admin dale
mkdir -p /home/dale/.ssh
echo ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA5zBO8Gp4vUv9ShxQ2zAah21n4F1iAYG0Fif2RVs6LcY7NeKL3khQbN1WQLLmCcLOmvwlMbQFDW2f79oupWPUFBWXTl2H8bqHxhu1C5H+2poAq00i/8XUuK/rhZGiktjpBUAaSxMRSkHqlXy/bWFrD0XCJGBPAiHY40pLSkGHaQAKC6JhKgLjFOMyNZihj0C68zpbq+WHKbAyuAg700N/ZJpdh/etk/vSMJEmtsvja04NVLQztHyAPYbKfEeAzrfevvTF3AYnhDJ6pOvsVlIHTAHb/zkSZ3e2clmuXPraET3JluhvGXF2HnfbUQdc+1q+g9eODcELS9KUWi3sYQ8cfQ== > /home/dale/.ssh/authorized_keys
chown -R dale:dale /home/dale

#_  _ _  _    ___  ____ ____ ___ _ ___ _ ____ _  _ 
#|__| |\ |    |__] |__| |__/  |  |  |  | |  | |\ | 
#|  | | \|    |    |  | |  \  |  |  |  | |__| | \| 
##################################################

mkdir -p /hn
echo "/dev/xvdc  /hn  ext3  noatime,errors=remount-ro  0  1" >> /etc/fstab
mount -a
chown hypernumbers:hypernumbers /hn


#_  _ _   _ ___  ____ ____ _  _ _  _ _  _ ___  ____ ____ ____ 
#|__|  \_/  |__] |___ |__/ |\ | |  | |\/| |__] |___ |__/ [__  
#|  |   |   |    |___ |  \ | \| |__| |  | |__] |___ |  \ ___] 
##################################################

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