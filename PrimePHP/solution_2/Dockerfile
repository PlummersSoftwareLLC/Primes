FROM php:8.5.12-cli-alpine3.24@sha256:1ff5b45922ab30f418447c6089dbe2eba5f01e0b2742863d0327ac349305d70f
WORKDIR /opt/app
RUN printf '%s\n' 'opcache.enable_cli=1' 'opcache.file_update_protection=0' 'opcache.jit_buffer_size=64M' 'opcache.jit=tracing' > "$PHP_INI_DIR/conf.d/sieve.ini"
COPY PrimePHP.php WheelSieve.php parallel.php check.php ./
ENTRYPOINT ["php"]
CMD ["PrimePHP.php"]
