FROM ruby:3.1-alpine3.14

RUN apk add --no-cache build-base && gem install numo-narray:0.9.2.1 && gem install concurrent-ruby:1.1.10

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "--jit", "-W0", "prime.rb" ]
