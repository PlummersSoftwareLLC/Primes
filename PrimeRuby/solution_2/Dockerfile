FROM ruby:3.1-alpine3.14

RUN apk add build-base
RUN gem install numo-narray

WORKDIR /opt/app
COPY *.rb .

ENTRYPOINT [ "ruby", "prime.rb" ]
