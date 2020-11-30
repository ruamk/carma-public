#!/bin/sh

# The nginx configuration used:
#        server {
#                listen 443 ssl;
#                server_name localhost;
#                client_max_body_size 1m;
#
#                ssl_certificate         /home/sz/certs/localhost/localhost.crt;
#                ssl_certificate_key     /home/sz/certs/localhost/localhost.key;
#
#                location / {
#                        proxy_pass http://127.0.0.1:8000/;
#                        proxy_read_timeout 600s;
#                }
#                location /search {
#                        proxy_pass http://127.0.0.1:8167;
#                }
#        }

set -e
#trap 'echo TRAP; pkill -P $$' SIGINT SIGTERM

echo ====
stack run carma &
cd ../carma-proxies && stack run carma-proxies

