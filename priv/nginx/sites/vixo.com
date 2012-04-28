server {
    ## Same as default.conf, but special case for vixo.com

    listen       80;
    server_name  vixo.com;
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
   listen        80;
   server_name   documentation.vixo.com;

   location / {
       root /hn/files-www/vixo/documentation/;
   }
}

server {
    listen       80;
    server_name  files.vixo.com;

    location / {
        root /hn/files-www;
    }
}
