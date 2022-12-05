# Note: these three variables must match their counterparts in CUDASieve/makefile
CUDA_DIR = /usr/local/cuda
GPU_ARCH = compute_60
GPU_CODE = sm_60,sm_61,sm_62

# Location of CUDASieve
CUDASIEVE_DIR = CUDASieve

# Compilers to use
NVCC = $(CUDA_DIR)/bin/nvcc
CC = g++

# Flags for nvcc
# ptxas-options=-dlcm=cg (vs. default of ca) is about a 2% performance gain
NVCC_FLAGS = -ccbin $(CC) -std=c++14 -arch=$(GPU_ARCH) -code=$(GPU_CODE) --ptxas-options=-dlcm=cs -lineinfo

INCLUDES = -I $(CUDASIEVE_DIR)/include/ -I $(CUDASIEVE_DIR)/src/ -I $(CUDA_DIR)/include/
LIB_DIR = -L $(CUDASIEVE_DIR)/
CC_LIBS = -lm -lstdc++

CS_MAIN = cudasieve
CS_LIB = lib$(CS_MAIN).a

all: benchmark

clean:
	rm -f benchmark

benchmark: benchmark.cu $(CUDASIEVE_DIR)/$(CS_LIB)
	$(NVCC) $(NVCC_FLAGS) $(CC_LIBS) $(INCLUDES) $(LIB_DIR) -l$(CS_MAIN) $< -o $@
