FROM ruby:3.1-alpine3.14

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "--jit", "prime.rb" ]
