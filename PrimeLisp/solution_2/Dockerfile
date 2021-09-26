FROM daewok/sbcl:2.1.7-alpine3.14

WORKDIR /opt/app

COPY *.lisp run.sh ./

ENTRYPOINT [ "./run.sh" ]
