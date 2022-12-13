FROM ruby:3.1-alpine3.17

RUN gem install concurrent-ruby

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "--jit", "prime.rb" ]
