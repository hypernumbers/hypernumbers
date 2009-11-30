server {

        listen       80;
        server_name  dev.hypernumbers.com;

        # Main location

        location / {
	      auth_basic            "Restricted";
	      auth_basic_user_file  /etc/nginx/conf/.htpasswd;
	      autoindex on;
	      root   /home/hypernumbers/dev.hypernumbers.com;
        }



    }