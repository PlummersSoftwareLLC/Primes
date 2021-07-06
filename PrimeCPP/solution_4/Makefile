CXXFLAGS = -std=c++20 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native -funroll-all-loops
LDLIBS = -pthread

.PHONY: all
all: prime_cpp prime_cpp_test

.PHONY: clean
clean:
	$(RM) prime_cpp prime_cpp_test

prime_cpp: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) $^ $(LDLIBS) -o $@

prime_cpp_test: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) -fconstexpr-loop-limit=2000000 -fconstexpr-ops-limit=200000000 -DRUN_TESTS $^ $(LDLIBS) -o $@

run: prime_cpp
	./prime_cpp

test: prime_cpp_test
	./prime_cpp_test
