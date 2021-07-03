CXXFLAGS = -std=c++17 -Wall -Wextra -pedantic -pthread -O3 -march=native -mtune=native -funroll-all-loops
LDLIBS = -pthread

.PHONY: all
all: primeCPP_base_vector

.PHONY: clean
clean:
	$(RM) primeCPP_base_vector

primeCPP_base_vector: PrimeCPP.cpp
	$(CXX) $(CXXFLAGS) $^ $(LDLIBS) -o $@

run: primeCPP_base_vector
	./primeCPP_base_vector
