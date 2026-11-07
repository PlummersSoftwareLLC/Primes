FROM ruby:3.4-alpine3.24

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "--jit", "prime.rb" ]
