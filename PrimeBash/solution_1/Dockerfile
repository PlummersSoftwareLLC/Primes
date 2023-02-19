# bash (with coreutils) in alpine seems to be less optimized than ubuntu
# bash 5.1 seems to have much better performance than 5.0 shipped in 20.04
FROM ubuntu:22.04

WORKDIR /opt/app
COPY *.sh *.common ./

ENTRYPOINT [ "./run.sh" ]
