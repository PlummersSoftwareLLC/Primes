ifndef CXX_CLANG
CXX_CLANG=$(CXX)
endif

ifndef CXX_GCC
CXX_GCC=$(CXX)
endif

CXXFLAGS = -std=c++20 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native
GCC_CXXFLAGS = $(CXXFLAGS) -funroll-all-loops -fconstexpr-loop-limit=2000000 -fconstexpr-ops-limit=200000000
CLANG_CXXFLAGS = $(CXXFLAGS) -fconstexpr-steps=50000000 -fbracket-depth=1024

LDLIBS = -pthread

.PHONY: all
all: prime_cpp prime_cpp_pregen prime_cpp_test

.PHONY: clean
clean:
	$(RM) prime_cpp prime_cpp_pregen prime_cpp_test

prime_cpp: prime_cpp.cpp
	$(CXX_CLANG) $(CLANG_CXXFLAGS) $^ $(LDLIBS) -o $@

prime_cpp_pregen: prime_cpp.cpp
	$(CXX_GCC) $(GCC_CXXFLAGS) -DRUN_PREGEN $^ $(LDLIBS) -o $@

prime_cpp_test: prime_cpp.cpp
	$(CXX_CLANG) $(CLANG_CXXFLAGS) -DRUN_TESTS $^ $(LDLIBS) -o $@

run: prime_cpp prime_cpp_pregen
	./prime_cpp
	./prime_cpp_pregen

test: prime_cpp_test
	./prime_cpp_test

run_and_test: run test
