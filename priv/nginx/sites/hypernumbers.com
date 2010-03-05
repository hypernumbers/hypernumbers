server {

    listen       80 default;
    server_name  hypernumbers.com *.hypernumbers.com;
    error_page   502 503 504  
        /home/hypernumbers/www/hypernumbers.com/priv/nginx/error_pages/maintenance.html; 

    location / {
        proxy_pass              http://127.0.0.1:9000/;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;

        proxy_buffering off;
        proxy_read_timeout 3600;
    }
}
