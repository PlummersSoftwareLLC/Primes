FROM daewok/sbcl:2.1.8-alpine3.14

WORKDIR /opt/app

COPY *.lisp ./
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
