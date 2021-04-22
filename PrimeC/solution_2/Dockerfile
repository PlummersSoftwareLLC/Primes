FROM alpine:3.13

RUN apk add --update build-base

WORKDIR /opt/app

COPY compile.sh .
COPY run.sh .
COPY sieve_1of2.c .
COPY sieve_1of2_epar.c .
COPY sieve_1of2_par.c .
COPY sieve_8of30.c .
COPY sieve_8of30_epar.c .
COPY sieve_8of30_only_write_read_bits.c .
COPY sieve_8of30_par.c .
COPY sieve_48of210.c .
COPY sieve_48of210_epar.c .
COPY sieve_48of210_only_write_read_bits.c .
COPY sieve_48of210_par.c .
COPY sieve_480of2310_epar.c .
COPY sieve_480of2310_only_write_read_bits.c .
COPY sieve_480of2310_par.c .
COPY sieve_5760of30030_epar.c .
COPY sieve_5760of30030_only_write_read_bits.c .
COPY sieve_5760of30030_par.c .

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]


