server
{
    listen        80;
    server_name   alpha.hypernumbers.com;
    #root	      /Users/tom/code/hypernumbers/code;

    location / {
        proxy_pass              http://127.0.0.1:9001;
        proxy_set_header        X-Forwarded-For      $proxy_add_x_forwarded_for;
        proxy_set_header        HN-Host              $host;
        proxy_set_header        HN-Port              $server_port;

        proxy_buffering         off;
        proxy_read_timeout      3600;
    }        
}
