server {
    listen       80;
    server_name  doug.quinquagenarians.com;
    
    location / {
        root /hn/files-www/doug.quinquagenarians.com/;
    }
}

server {
    listen       80;
    server_name  debbie.quinquagenarians.com;
    
    location / {
        root /hn/files-www/debbie.quinquagenarians.com/;
    }
}

server {
    listen       80;
    server_name  janet.quinquagenarians.com;
    
    location / {
        root /hn/files-www/janet.quinquagenarians.com/;
    }
}

server {
    listen       80;
    server_name  gordon.quinquagenarians.com;
    
    location / {
        root /hn/files-www/gordon.quinquagenarians.com/;
    }
}

server {
       server_name   www.doug.quinquagenarians.com;
       rewrite ^(.*) http://doug.quinquagenarians.com$1 permanent;
}

server {
       server_name   www.debbie.quinquagenarians.com;
       rewrite ^(.*) http://debbie.quinquagenarians.com$1 permanent;
}

server {
       server_name   www.janet.quinquagenarians.com;
       rewrite ^(.*) http://janet.quinquagenarians.com$1 permanent;
}

server {
       server_name   www.gordon.quinquagenarians.com;
       rewrite ^(.*) http://gordon.quinquagenarians.com$1 permanent;
}
