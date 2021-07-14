FROM php:8.0-cli-alpine3.13

WORKDIR /opt/app

RUN docker-php-ext-install opcache

COPY *.php .

ENTRYPOINT [ "php", "-dopcache.enable_cli=1", "PrimePHP.php" ]
