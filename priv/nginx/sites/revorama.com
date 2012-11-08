server {
    listen       80;
    server_name  tim.revorama.com

    location / {
        root /hn/files-www/tim/;
    }
}

server {
    listen       80;
    server_name  tim2.revorama.com

    location / {
        root /hn/files-www/tim2/;
    }
}

