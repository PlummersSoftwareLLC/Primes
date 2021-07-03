CXXFLAGS_COMMON = -std=c++20 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native
CXXFLAGS_CLANG = $(CXXFLAGS_COMMON) -fconstexpr-steps=50000000 -fbracket-depth=1024
CXXFLAGS_GCC = $(CXXFLAGS_COMMON) -funroll-all-loops -fconstexpr-loop-limit=10000000 -fconstexpr-ops-limit=1000000000

.PHONY: all
all: prime_cpp_gcc prime_cpp_clang prime_cpp_pregen_gcc prime_cpp_pregen_clang prime_cpp_test_gcc prime_cpp_test_clang prime_cpp_suite_gcc prime_cpp_suite_clang

.PHONY: clean
clean:
	$(RM) prime_cpp_gcc prime_cpp_clang prime_cpp_pregen_gcc prime_cpp_pregen_clang prime_cpp_test_gcc prime_cpp_test_clang prime_cpp_suite_gcc prime_cpp_suite_clang

prime_cpp_gcc: prime_cpp.cpp
	$(CXX_GCC) $(CXXFLAGS_GCC) $(CXXFLAGS_GCC) $^ -o $@

prime_cpp_clang: prime_cpp.cpp
	$(CXX_CLANG) $(CXXFLAGS_CLANG) $^ -o $@

prime_cpp_pregen_gcc: prime_cpp.cpp
	$(CXX_GCC) $(CXXFLAGS_GCC) -DRUN_PREGEN $^ -o $@

prime_cpp_pregen_clang: prime_cpp.cpp
	$(CXX_CLANG) $(CXXFLAGS_CLANG) -DRUN_PREGEN $^ -o $@

prime_cpp_test_gcc: prime_cpp.cpp
	$(CXX_GCC) $(CXXFLAGS_GCC) -DRUN_TESTS $^ -o $@

prime_cpp_test_clang: prime_cpp.cpp
	$(CXX_CLANG) $(CXXFLAGS_CLANG) -DRUN_TESTS $^ -o $@

prime_cpp_suite_gcc: prime_cpp.cpp
	$(CXX_GCC) $(CXXFLAGS_GCC) -DRUN_SUITE $^ -o $@

prime_cpp_suite_clang: prime_cpp.cpp
	$(CXX_CLANG) $(CXXFLAGS_CLANG) -DRUN_SUITE $^ -o $@

run_all: all
	./prime_cpp_gcc
	./prime_cpp_clang
	./prime_cpp_pregen_gcc
	./prime_cpp_pregen_clang
	./prime_cpp_test_gcc
	./prime_cpp_test_clang
	./prime_cpp_suite_gcc
	./prime_cpp_suite_clang

run: prime_cpp_gcc prime_cpp_clang
	./prime_cpp_gcc
	./prime_cpp_clang

run_pregen: prime_cpp_pregen_gcc prime_cpp_pregen_clang
	./prime_cpp_pregen_gcc
	./prime_cpp_pregen_clang

run_tests: prime_cpp_test_gcc prime_cpp_test_clang
	./prime_cpp_test_gcc
	./prime_cpp_test_clang

run_suite: prime_cpp_suite_gcc prime_cpp_suite_clang
	./prime_cpp_suite_gcc
	./prime_cpp_suite_clang

run_bench: prime_cpp_pregen_gcc prime_cpp_pregen_clang prime_cpp_gcc prime_cpp_clang
	./prime_cpp_pregen_gcc
	./prime_cpp_pregen_clang
	./prime_cpp_gcc
	./prime_cpp_clang
