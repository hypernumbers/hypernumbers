server {
    listen       80;
    server_name  dinner.linlithgowyes.com;
    
    location / {
        root /hn/files-www/dinner.linlithgowyes.com/;
    }
}

server {
    listen       80;
    server_name  linlithgowyes.com;
    
    location / {
        root /hn/files-www/dinner.linlithgowyes.com/;
    }
}

server {
       server_name   www.dinner.linlithgowyes.com;
       rewrite ^(.*) http://dinner.linlithgowyes.com$1 permanent;
}

server {
       server_name   www.linlithgowyes.com;
       rewrite ^(.*) http://linlithgowyes.com$1 permanent;
}
