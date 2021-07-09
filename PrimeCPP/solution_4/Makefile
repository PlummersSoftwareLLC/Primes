CXXFLAGS = -std=c++20 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native -fconstexpr-steps=50000000 -fbracket-depth=1024

.PHONY: all
all: prime_cpp prime_cpp_pregen prime_cpp_test

.PHONY: clean
clean:
	$(RM) prime_cpp prime_cpp_pregen prime_cpp_test

prime_cpp: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) $^ -o $@

prime_cpp_pregen: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) -DRUN_PREGEN $^ -o $@

prime_cpp_test: prime_cpp.cpp
	$(CXX) $(CXXFLAGS) -DRUN_TESTS $^ -o $@

run: prime_cpp_pregen prime_cpp
	./prime_cpp_pregen
	./prime_cpp

test: prime_cpp_test
	./prime_cpp_test
