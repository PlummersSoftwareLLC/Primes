FROM elixir:1.12-alpine

WORKDIR /opt/app
COPY . .
RUN mix local.hex --force
RUN mix deps.get
RUN mix test
RUN MIX_ENV=prod mix escript.build

ENTRYPOINT [ "./prime_sieve" ]
