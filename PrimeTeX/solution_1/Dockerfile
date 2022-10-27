# alpine 3.13 and Ubuntu 20.04 LTS do not have a recent enough texlive (the
# code uses fairly recent LaTeX3 features)
# There is a specific texlive image, but that includes the full distribution and
# appropriately massive. This is smaller.
FROM ubuntu:22.04

WORKDIR /opt/app

COPY prime_race.tex prime_functions.tex run.sh ./
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y texlive-latex-base texlive-latex-recommended \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* 

ENTRYPOINT [ "bash", "run.sh" ]