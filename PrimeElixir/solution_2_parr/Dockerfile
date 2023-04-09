FROM elixir:alpine

WORKDIR /opt/app
COPY . .
RUN mix test
ENV MIX_ENV=prod \
    MIX_HOME=/opt/mix
RUN mix compile
ENTRYPOINT [ "mix" ]
CMD  [ "run", "-e", "PrimeSieve.main" ]
