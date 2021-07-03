FROM ubuntu:21.04

WORKDIR /opt/app

COPY prime_race.tex prime_functions.tex run.sh ./
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y texlive-latex-base texlive-latex-recommended

ENTRYPOINT [ "bash", "run.sh" ]