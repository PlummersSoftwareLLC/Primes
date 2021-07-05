SHELL := /bin/bash

.ONESHELL:

SOLUTIONS  := $(shell find Prime* -type f -name Dockerfile -exec dirname {} \; | sed -e 's|^./||' | sort)
OUTPUT_DIR := $(shell mktemp -d)
ARCH_FILE  := ${shell case $$(uname -m) in x86_64) echo arch-amd64 ;; aarch64) echo arch-arm64 ;; esac}

all: report

benchmark: $(SOLUTIONS)
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

report: benchmark
	@docker run --rm -v "$(CURDIR)/tools":/tools -v "$(OUTPUT_DIR)":/out -w /tools -e CI=true node:alpine sh -c "npm ci && npm start -- report -d /out"

one:
	@if [[ ! -z "$${SOLUTION}" ]]; then \
		NAME=$$(echo $${SOLUTION//\//-} | tr '[:upper:]' '[:lower:]'); \
		OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
		echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$SOLUTION) | tee "$${OUTPUT}"; \
		docker run --rm -v "$(CURDIR)/tools":/tools -v "$(OUTPUT_DIR)":/out -w /tools -e CI=true node:alpine sh -c "npm ci && npm start -- report -d /out" \
	else \
		echo "Not specified!"; \
	fi