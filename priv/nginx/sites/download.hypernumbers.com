server {
    listen       80 default;
    server_name  download.hypernumbers.com;
    
    location / {
        root /home/hypernumbers/www/files;
    }
}
