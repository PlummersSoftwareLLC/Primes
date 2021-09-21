FROM verilator/verilator:4.210 as build
WORKDIR /work
COPY *.v .
COPY *.cpp .
RUN verilator --cc prime_number_core.v --exe prime_number_core_tb.cpp --top prime_number_core
WORKDIR /work/obj_dir
RUN make -j -f Vprime_number_core.mk Vprime_number_core

FROM ubuntu:20.04 as prime
COPY --from=build /work/obj_dir/Vprime_number_core /usr/local/bin/Vprime_number_core

ENTRYPOINT ["Vprime_number_core"]



