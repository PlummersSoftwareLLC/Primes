# Verilog solution by alwayslinux2

![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)

Verilog is a hardware description language (HDL), used to describe a digital circuit that does something. 
Usually Verilog designs are synthesized into a bitfile and loaded into an FPGA or sent to a silicon foundry and converted into an ASIC. 
This can be very expensive, so various ways to simulate such a circuit ahead of time were devised. One of them is Verilator. It transpiles
Verilog source code into a C++ simulation of the circuit, then compiles that into an executable.

This solution uses two files: 
prime_number_core.v This is the synthesizeable Verilog "core", which could be dropped into any digital circuit design and treated as a dedicated "chip" that just does prime number stuff.
prime_number_core_tb.cpp  This is the C++ "Test Bench". Some Verilog test environments use SystemVerilog files to describe the simulation test scenario, but Verilator uses C++ for this purpose.

I have run this simulation on a PC with an i7-10700 @2.9Ghz, and got about 216 iterations, but I also synthesized the same prime_number_core.v in Vivado, and loaded it on a Xilinx Atrix-7, clocked at 100Mhz, along with a MicroBlaze soft CPU, and a similar C sieve implementation for comparisson, and was able to get 382 iterations from the prime number core, vs only 12 iterations from the C implementation running on the soft CPU, both running side by side inside the Atrix-7, at 100Mhz. 

Verilog isn't really meant to be a general purpose PC language, and the performance isn't stellar on a PC, but clock cycle for clock cycle, it blows C out of the water on hardware. Note the 100Mhz FPGA even outran the 2.9Ghz PC CPU in this scenario.

## Run instructions

docker build -t verilog .
docker run -t verilog

## Output

alwayslinux2-verilog;216;5.0;1;algorithm=base,faithful=yes,bits=1