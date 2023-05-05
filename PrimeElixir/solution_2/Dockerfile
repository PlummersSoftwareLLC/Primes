FROM elixir:1.14-otp-24-alpine

WORKDIR /opt/app
COPY . .
RUN mix test
ENV MIX_ENV=prod \
    MIX_HOME=/opt/mix
RUN mix compile
ENTRYPOINT [ "mix" ]
CMD  [ "run", "-e", "PrimeSieve.main" ]
