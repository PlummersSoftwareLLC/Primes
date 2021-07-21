FROM php:8.0-cli-alpine3.13

WORKDIR /opt/app

COPY *.php .
COPY opcache.ini /usr/local/etc/php/conf.d/

ENTRYPOINT [ "php", "PrimePHP.php" ]