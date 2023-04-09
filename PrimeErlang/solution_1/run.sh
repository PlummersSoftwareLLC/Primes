#!/bin/bash
QUIET=t rebar3 escriptize
exec _build/default/bin/PrimeErlang
