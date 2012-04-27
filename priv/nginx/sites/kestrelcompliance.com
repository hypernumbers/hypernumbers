server {
    listen       80;
    server_name  kestrelcompliance.com;
    rewrite ^ http://kestrelcompliance.co.uk$request_uri? permanent;
}

