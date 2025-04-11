# Makefile for Prime Sieve project

# Default target builds all variants
all: 
	@echo "Building all variants..."
	$(MAKE) sieve_extend
	$(MAKE) sieve_classic
	$(MAKE) sieve_base
	./sieve runall

# Rule to build each variant
sieve_extend:
	@echo "Building variant: sieve_extend"
	./sieve sieve_extend compile

sieve_classic:
	@echo "Building variant: sieve_classic"
	./sieve sieve_classic64 compile
	./sieve sieve_classic8 compile

sieve_base:
	@echo "Building variant: sieve_base"
	./sieve sieve_base compile

# Clean target to remove generated files
clean:
	@echo "Cleaning build artifacts..."
	rm -f bin/*
	rm -rf dev/build/*
	rm -rf dev/docker/dockerfiles/*
	@echo "Clean complete"

# Help target to show available targets
help:
	@echo "Prime Sieve Makefile"
	@echo "===================="
	@echo "Available targets:"
	@echo "  make                - Build all variants"
	@echo "  make bin/sieve_<variant> - Build a specific variant"
	@echo "  make clean          - Remove all build artifacts"
	@echo "  make help           - Show this help message"
	@echo ""
	@echo "Available variants: $(VARIANTS:%=bin/sieve_%)"

# Force rebuilding variants even if files with those names exist
.PHONY: all clean help sieve_extend sieve_classic sieve_base
