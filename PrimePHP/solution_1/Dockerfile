FROM php:8.0-cli-alpine3.13

WORKDIR /opt/app

COPY *.php .
COPY opcache.ini /usr/local/etc/php/conf.d/

ENTRYPOINT [ "php", "-d opcache.enable=1", "-d opcache.enable_cli=1", "-d opcache.jit=1255","-d opcache.jit_buffer_size=100M", "PrimePHP.php" ]