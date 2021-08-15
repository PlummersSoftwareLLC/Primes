FROM gnuoctave/octave:6.2.0

WORKDIR /opt/app
COPY *.m ./

ENTRYPOINT [ "octave", "-q", "-W", "--norc", "run.m" ]
