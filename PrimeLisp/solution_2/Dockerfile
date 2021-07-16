FROM daewok/sbcl:2.1.6-alpine3.13

WORKDIR /opt/app

COPY *.lisp .
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
