server {

        listen       80;
        server_name  svn.hypernumbers.com;

        # Main location

        location / {

	    proxy_pass         http://127.0.0.1:8000/;
            proxy_redirect     off;

            proxy_set_header   Host             $host;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;

            proxy_buffering off;
            proxy_read_timeout 36000; 

        }



    }