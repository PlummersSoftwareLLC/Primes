#!/bin/bash
QUIET=t rebar3 escriptize >/dev/null
exec _build/default/bin/PrimeErlang
