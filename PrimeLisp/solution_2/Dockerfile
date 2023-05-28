FROM fukamachi/sbcl:2.3.4-alpine

WORKDIR /opt/app

COPY *.lisp run.sh ./

ENTRYPOINT [ "./run.sh" ]
