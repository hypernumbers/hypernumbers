server
{
    listen        80;
    server_name   revorama.com;
    error_page    502 503 504 /maintenance.html;

    location = /maintenance.html {
        internal;
        root /hn/hypernumbers/priv/nginx/error_pages;
    }

    location / {
        proxy_pass              http://127.0.0.1:9000;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;

        proxy_buffering         off;
        proxy_read_timeout      3600;
    }        

}

