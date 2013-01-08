server {
        listen           80;
        server_name      wordpress-markdown-to-github-markdown.com;

        access_log      /var/log/nginx/example.com.access_log;
        error_log       /var/log/nginx/example.com.error_log warn;

        root            /hn/files-www/wordpress-markdown-to-github-markdown;

        index           index.php index.html;
        fastcgi_index   index.php;

        location ~ \.php {
	      # Workaround PHP vulnerability:
	      # http://forum.nginx.org/read.php?2,88845,page=3
                try_files $uri =404;
                # Alternatively you can set
                # cgi.fix_pathinfo = false
                # in php.ini

                include /etc/nginx/fastcgi_params;
                keepalive_timeout 0;
                fastcgi_param   SCRIPT_FILENAME  $document_root$fastcgi_script_name;
	      fastcgi_pass    127.0.0.1:9000;
        }
}