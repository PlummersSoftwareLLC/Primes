# bash (with coreutils) in alpine seems to be less optimized than ubuntu
# bash 5.1 seems to have much better performance than 5.0 shipped in 20.04
# bash 5.2 in 24.04 needs BASH_COMPAT=51, which PrimeBash.common sets
FROM ubuntu:24.04

WORKDIR /opt/app
COPY *.sh *.common ./

ENTRYPOINT [ "./run.sh" ]
