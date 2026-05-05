FROM gcc:14-bookworm

WORKDIR /opt/app

COPY build.sh run.sh cwager_fortran_mt.f90 ./

RUN chmod +x build.sh run.sh \
    && ./build.sh

CMD ["./run.sh"]
