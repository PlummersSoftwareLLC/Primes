.ONESHELL:

SOLUTIONS  := $(shell find Prime* -type f -name Dockerfile -exec dirname {} \; | sed -e 's|^./||' | sort)
OUTPUT_DIR := $(shell mktemp -d)

all: report
	@echo "Output files available in $(OUTPUT_DIR)"

benchmark: $(SOLUTIONS)
	@for s in $(SOLUTIONS); do \
		NAME=$$(echo "$${s}" | sed -r 's/\//-/g' | tr '[:upper:]' '[:lower:]'); \
		OUTPUT="$(OUTPUT_DIR)/$${NAME}.out"; \
		echo "[*] Running $${NAME}" && docker run --rm $$(docker build -q $$s) | tee "$${OUTPUT}"; \
	done

report: benchmark
	@docker run --rm -v "$(OUTPUT_DIR)":/opt/session $$(docker build -q _tools) report.py -d /opt/session