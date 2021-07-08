CXXFLAGS = -std=c++20 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native

ifeq "$(CXX)" "g++-11"
	CXXFLAGS += -funroll-all-loops -fconstexpr-loop-limit=2000000 -fconstexpr-ops-limit=200000000
endif

ifeq "$(CXX)" "clang++-12"
	CXXFLAGS += -fconstexpr-steps=50000000 -fbracket-depth=1024
endif

LDLIBS = -pthread

.PHONY: all
all: prime_cpp prime_cpp_test

.PHONY: clean
clean:
	$(RM) prime_cpp prime_cpp_test

prime_cpp: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) $^ $(LDLIBS) -o $@

prime_cpp_test: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) -DRUN_TESTS $^ $(LDLIBS) -o $@

run: prime_cpp
	./prime_cpp

test: prime_cpp_test
	./prime_cpp_test
