CUDA_DIR = /usr/local/cuda
GPU_ARCH = compute_60
GPU_CODE = sm_60,sm_61,sm_62

NVCC=/usr/local/cuda/bin/nvcc
NVCC_FLAGS=-ccbin g++ -std=c++14 -arch=$(GPU_ARCH) -code=$(GPU_CODE) --ptxas-options=-dlcm=cs -lineinfo -lm -lstdc++ -I $(CUDA_DIR)/include/

all: primes

%: %.cu
	$(NVCC) $(NVCC_FLAGS) -o $@ $<
