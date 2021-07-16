# Build and run like this:
#
# $ cd PrimeLisp/solution_2
# $ podman build -t lisp2 .
# $ podman run --rm lisp2

#FROM clfoundation/sbcl:2.1.6-slim-buster
FROM daewok/sbcl:2.1.6-alpine3.13

WORKDIR /opt/app

COPY *.lisp .
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
