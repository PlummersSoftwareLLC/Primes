FROM gnuoctave/octave:11.3.0

WORKDIR /opt/app
COPY *.m ./

ENTRYPOINT [ "octave", "-q", "-W", "--norc", "run.m" ]
