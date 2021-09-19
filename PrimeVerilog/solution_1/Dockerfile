FROM verilator/verilator:latest as build
COPY *.v *.cpp .
RUN verilator --cc prime_number_core.v --exe prime_number_core_tb.cpp --top prime_number_core; cd obj_dir; make -j -f Vprime_number_core.mk Vprime_number_core

FROM ubuntu
COPY --from=build /work/obj_dir/Vprime_number_core /usr/local/bin/Vprime_number_core

ENTRYPOINT ["Vprime_number_core"]



