CUDA_DIR = /usr/local/cuda
GPU_ARCH = compute_60
GPU_CODE = sm_60,sm_61,sm_62

NVCC=$(CUDA_DIR)/bin/nvcc
NVCC_FLAGS=-ccbin g++ -std=c++14 -arch=$(GPU_ARCH) -code=$(GPU_CODE) -Xcompiler -march=native,-Ofast -lineinfo -lm -lstdc++ -I $(CUDA_DIR)/include/

all: primes

%: %.cu
	$(NVCC) $(NVCC_FLAGS) -o $@ $<
