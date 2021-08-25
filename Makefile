SHELL := /bin/bash

.ONESHELL:

DIRECTORY := $(shell pwd)
FORMATTER := "table"

.PHONY: all
all: benchmark

.PHONY: benchmark
benchmark: check-env
	@REALPATH=$$(cd "$${DIRECTORY}" && pwd); \
	ARGS=("-d $${REALPATH}" "-f $(FORMATTER)"); \
	[ ! -z $${OUTPUT_FILE} ] && ARGS+=( "-o $${OUTPUT_FILE}" ); \
	[ ! -z $${UNCONFINED} ] && ARGS+=( "--unconfined" ); \
	cd ./tools; npm ci --silent && npm start --silent -- benchmark $${ARGS[@]}

.PHONY: check-env
check-env: check-cc-works check-docker-works check-node-works

.PHONY: check-cc-works
check-cc-works:
	@cc --version >/dev/null 2>&1 || (echo 'Please install a C compiler. See https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/BENCHMARK.md for more information.' && exit 1)

.PHONY: check-node-works
check-node-works:
	@npm --version >/dev/null 2>&1 || (echo 'Please install Node.js. See https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/BENCHMARK.md for more information.' && exit 1)

.PHONY: check-docker-works
check-docker-works:
	@docker --version >/dev/null 2>&1 || (echo 'Please install docker. See https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/BENCHMARK.md for more information.' && exit 1)
	@docker ps >/dev/null
