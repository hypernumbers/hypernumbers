server {
    ## Same as default.conf, but special case for hypernumbers.com

    listen       80;
    server_name  hypernumbers.com;
    error_page   502 503 504 /maintenance.html;

    location = /maintenance.html {
        internal;
        root /hn/hypernumbers/priv/nginx/error_pages;
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
    ## Same as default.conf, but special case for stephen.

    listen       80;
    server_name  stephen.hypernumbers.com mccrory.hypernumbers.com;
    error_page   502 503 504 /maintenance.html;

    location = /maintenance.html {
        internal;
        root /hn/hypernumbers/priv/nginx/error_pages;
    }

    location / {
        proxy_pass              http://127.0.0.1:9091/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_buffering         off;
        proxy_read_timeout      3600;
    }
}

server {
    listen   8080;
    server_name dev.hypernumbers.com;
    error_page  502 503 504 /maintenance.html;

    location = /maintenance.html {
         internal;
         root /hn/hypernumbers/priv/nginx/error_pages;
    }

    location / {
        proxy_pass              http://127.0.0.1:8888/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_buffering         off;
        proxy_read_timeout      3600;
    }
}

server {
    listen   9999;
    server_name twilio.hypernumbers.com;
    error_page  502 503 504 /maintenance.html;

    location = /maintenance.html {
         internal;
         root /hn/hypernumbers/priv/nginx/error_pages;
    }

    location / {
        proxy_pass              http://127.0.0.1:1234/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;
        proxy_buffering         off;
        proxy_read_timeout      3600;
    }
}

server {
    listen       80;
    server_name  download.hypernumbers.com files.hypernumbers.com;

    location / {
        root /hn/files-www;
    }
}


server {
   listen        80;
   server_name   documentation.hypernumbers.com;

   location / {
       root /hn/files-www/documentation/;
   }
}

server {
   listen        80;
   server_name   escape-from-spreadsheet-hell.hypernumbers.com;

   location / {
       root /hn/files-www/escape/;
   }
}

server {
   listen        80;
   server_name   testing.hypernumbers.com;

   location / {
       root /hn/files-www/testing/;
   }
}