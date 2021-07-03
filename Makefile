SHELL := /bin/bash

.ONESHELL:

SOLUTIONS  := $(shell find Prime* -type f -name Dockerfile -exec dirname {} \; | sed -e 's|^./||' | sort)
OUTPUT_DIR := $(shell mktemp -d)
ARCH_FILE  := ${shell case $$(uname -m) in x86_64) echo arch-amd64 ;; aarch64) echo arch-arm64 ;; esac}

all: report

benchmark: sanity-docker $(SOLUTIONS)
	@echo "--- Output files available in $(OUTPUT_DIR)"

	@for s in $(SOLUTIONS); do \
		NAME=$$(echo "$${s}" | sed -r 's/\//-/g' | tr '[:upper:]' '[:lower:]'); \
		ls $${s}/arch-* > /dev/null 2>&1; \
		if [[ -z "$(ARCH_FILE)" || -f "$${s}/$(ARCH_FILE)" || "$$?" -ne 0 ]]; then \
			OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
			echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$s) | tee "$${OUTPUT}"; \
		else \
			echo "[*] Skipping $${NAME} due to architecture mismatch"; \
		fi; \
	done

report: sanity-node benchmark
	@cd tools/; \
	npm ci && npm start -- report -d "$(OUTPUT_DIR)"

one:
	@if [[ ! -z "$${SOLUTION}" ]]; then \
		NAME=$$(echo "$${SOLUTION}" | sed -r 's/\//-/g' | tr '[:upper:]' '[:lower:]'); \
		OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
		echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$SOLUTION) | tee tee "$${OUTPUT}"; \
		cd tools/; \
		npm ci && npm start -- report -d "$(OUTPUT_DIR)" \
	else \
		echo "Not specified!"; \
	fi

sanity-checks: sanity-docker sanity-node

sanity-node:
	@# Check it node.js is installed. Needed to generate report.
	@node --version >/dev/null 2>&1 || (echo 'Please install Node.js. https://nodejs.org/en/download' && exit 1)
	@npm --version >/dev/null 2>&1 || (echo 'How is Npm not installed but Node.js is?' && exit 1)

sanity-docker:
	@# Check if docker engine is installed
	@docker --version >/dev/null 2>&1 || (echo 'Please install docker. https://docs.docker.com/engine/install' && exit 1)
	
	@# Check if docker is running and that we can connect to it
	@docker ps >/dev/null
