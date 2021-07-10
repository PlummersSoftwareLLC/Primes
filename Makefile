SHELL := /bin/bash

.ONESHELL:

SOLUTIONS  := $(shell find Prime* -type f -name Dockerfile -exec dirname {} \; | sed -e 's|^./||' | sort)
OUTPUT_DIR := $(shell mktemp -d)
ARCH_FILE  := ${shell case $$(uname -m) in x86_64) echo arch-amd64 ;; aarch64) echo arch-arm64 ;; esac}
FORMATTER  := "table"

.PHONY: all
all: report

.PHONY: benchmark
benchmark: check-docker-works $(SOLUTIONS)
	@echo "--- Output files available in $(OUTPUT_DIR)"

	@for SOLUTION in $(SOLUTIONS); do \
		NAME=$$(echo $${SOLUTION//\//-} | tr '[:upper:]' '[:lower:]'); \
		ls $${SOLUTION}/arch-* > /dev/null 2>&1; \
		if [[ -z "$(ARCH_FILE)" || -f "$${SOLUTION}/$(ARCH_FILE)" || "$$?" -ne 0 ]]; then \
			OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
			echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$SOLUTION) | tee "$${OUTPUT}"; \
		else \
			echo "[*] Skipping $${NAME} due to architecture mismatch"; \
		fi; \
	done

.PHONY: report
report: check-node-works benchmark
	@cd tools/; \
	npm ci && npm start -- report -d "$(OUTPUT_DIR)" -f "$(FORMATTER)"

.PHONY: one
one: check-env
	@if [[ ! -z "$${SOLUTION}" ]]; then \
		NAME=$$(echo $${SOLUTION//\//-} | tr '[:upper:]' '[:lower:]'); \
		OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
		echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$SOLUTION) | tee "$${OUTPUT}"; \
		cd tools/; \
		npm ci && npm start -- report -d "$(OUTPUT_DIR)" -f "$(FORMATTER)" \
	else \
		echo "Not specified!"; \
	fi

.PHONY: check-env
check-env: check-docker-works check-node-works

.PHONY: check-node-works
check-node-works:
	@# Check if Node.js is installed. Needed to generate report.
	@npm --version >/dev/null 2>&1 || (echo 'Please install Node.js. See https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/BENCHMARK.md for more information.' && exit 1)

.PHONY: check-docker-works
check-docker-works:
	@# Check if docker engine is installed
	@docker --version >/dev/null 2>&1 || (echo 'Please install docker. See https://github.com/PlummersSoftwareLLC/Primes/blob/drag-race/BENCHMARK.md for more information.' && exit 1)
	
	@# Check if docker is running and that we can connect to it
	@docker ps >/dev/null
