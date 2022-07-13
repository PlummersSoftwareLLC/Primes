FROM php:8.1.7-cli

# Use the default production configuration, enable opcache & JIT
RUN mv "$PHP_INI_DIR/php.ini-production" "$PHP_INI_DIR/php.ini" && \
    printf "zend_extension=opcache.so\nopcache.enable=1\nopcache.enable_cli=1\nopcache.jit_buffer_size=100M\nopcache.jit=tracing\nmemory_limit=-1\n" >> "$PHP_INI_DIR/php.ini"

WORKDIR /opt/app
COPY *.php .

ENTRYPOINT [ "php", "PrimePHP.php" ]
