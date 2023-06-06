FROM daewok/sbcl:2.2.0-alpine3.15

WORKDIR /opt/app

COPY *.lisp run.sh ./

ENTRYPOINT [ "./run.sh" ]
