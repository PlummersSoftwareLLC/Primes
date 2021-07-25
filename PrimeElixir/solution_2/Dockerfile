FROM elixir:1.12-alpine

WORKDIR /opt/app
COPY . .
RUN mix test
ENV MIX_ENV=prod
ENV MIX_HOME=/opt/mix
ENTRYPOINT [ "mix" ]
CMD  [ "run", "-e", "PrimeSieve.main" ]
