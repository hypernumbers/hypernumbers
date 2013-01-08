server {
    listen       80;
    server_name  tim.revorama.com;

    location / {
        root /hn/files-www/tim/;
    }
}

server {
    listen       80;
    server_name  tim2.revorama.com;

    location / {
        root /hn/files-www/tim2/;
    }
}

server {
    listen       80;
    server_name  yuri.revorama.com;

    location / {
        root /hn/files-www/yuri.revorama.com/priv/www/;
    }
}

