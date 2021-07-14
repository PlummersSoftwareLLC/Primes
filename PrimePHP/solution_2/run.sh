#!/bin/sh

php -dopcache.enable_cli=1 -dopcache.enable=1 -dopcache.jit_buffer_size=100M -dopcache.jit=1255 PrimePHP.php
