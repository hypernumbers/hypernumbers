server {
    listen       80;
    server_name  download.hypernumbers.com;
    
    location / {
        root /home/hypernumbers/www/files;
    }
}
