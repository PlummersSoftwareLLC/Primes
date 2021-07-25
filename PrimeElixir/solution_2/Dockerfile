FROM elixir:1.12-alpine

WORKDIR /opt/app
COPY . .
RUN mix test
ENV MIX_ENV=prod
CMD  [ "mix" "run" "-e" "PrimeSieve.main" ]
