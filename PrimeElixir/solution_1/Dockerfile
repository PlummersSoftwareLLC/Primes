FROM elixir:1.12-alpine

WORKDIR /opt/app
COPY . .
RUN mix local.hex --force \
    && mix deps.get \
    && mix test \
    && MIX_ENV=prod mix escript.build

ENTRYPOINT [ "./prime_sieve" ]