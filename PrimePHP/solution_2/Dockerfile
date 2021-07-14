FROM php:8.0-cli-alpine3.13

WORKDIR /opt/app

COPY *.php .

ENTRYPOINT [ "php", "-dopcache.enable_cli=1", "-dopcache.enable=1", "-dopcache.jit_buffer_size=100M", "-dopcache.jit=1255", "-dxdebug.mode=off", "PrimePHP.php" ]
