FROM ruby:3.4-alpine3.24

# numo-narray 0.9.2.1 predates GCC 15: build it with Ruby's default CFLAGS plus C17 and a non-fatal pointer type warning
RUN apk add --no-cache build-base \
    && gem install numo-narray:0.9.2.1 -- --with-cflags="$(ruby -e 'print RbConfig::CONFIG["CFLAGS"]') -std=gnu17 -Wno-error=incompatible-pointer-types" \
    && gem install concurrent-ruby:1.2.2

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "--jit", "-W0", "prime.rb" ]
