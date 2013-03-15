server {
    ## Same as default.conf, but special case for vixo.com

    listen       80;
    server_name  vixo.com beta.vixo.com crm.vixo.com;
    error_page   502 503 504 /maintenance.html;

    location = /maintenance.html {
        internal;
        root /hn/vixo/priv/nginx/error_pages;
    }

    location / {
        proxy_pass              http://127.0.0.1:9090/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_buffering         off;
        proxy_read_timeout      3600;
    }
}

server {
       server_name   www.documentation.vixo.com;
       rewrite ^(.*) http://documentation.vixo.com$1 permanent;
}
server {
   listen        80;
   server_name   documentation.vixo.com;

   userid         on;
   userid_name    docouid;
   userid_domain  documentation.vixo.com;
   userid_path    /;
   userid_expires 365d;

   location / {
       access_log /var/log/nginx/vixodoco.log docoformat;
       root /hn/files-www/vixo2/documentation/;
   }
}

server {
       server_name   www.wordpress.vixo.com;
       rewrite ^(.*) http://wordpress.vixo.com$1 permanent;
}

server {
   listen        80;
   server_name   wordpress.vixo.com;

   userid         on;
   userid_name    wpuid;
   userid_domain  wordpress.vixo.com;
   userid_path    /;
   userid_expires 365d;

   location / {
       access_log /var/log/nginx/vixowordpress.log wordpressformat;
        proxy_pass              http://127.0.0.1:8765/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_set_header        Host                 wordpress.vixo.com;
        proxy_buffering         off;
        proxy_read_timeout      3600;
   }
}

server {
       server_name   www.blog.vixo.com;
       rewrite ^(.*) http://blog.vixo.com$1 permanent;
}

server {
   listen        80;
   server_name   blog.vixo.com;

   userid         on;
   userid_name    bloguid;
   userid_domain  blog.vixo.com;
   userid_path    /;
   userid_expires 365d;

   location / {
        proxy_pass              http://127.0.0.1:8765/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_set_header        Host                 $host;
        proxy_buffering         off;
        proxy_read_timeout      3600;
   }
}

server {
       server_name   www.testing.vixo.com;
       rewrite ^(.*) http://testing.vixo.com$1 permanent;
}

server {
   listen        80;
   server_name   testing.vixo.com;

   location / {
        proxy_pass              http://127.0.0.1:8765/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_set_header        Host                 $host;
        proxy_buffering         off;
        proxy_read_timeout      3600;
   }
}

server {
    listen       80;
    server_name  files.vixo.com;

    location / {
        root /hn/files-www;
    }
}
