FROM phipsgabler/texlive-minimal@sha256:f1e37453ef11783e582033adc07ba908839c00889634d17364a3a7869bc7f56d

WORKDIR /opt/app

COPY shared_batteries.tex erato_sieve.tex erato_benchmark.tex wheel_sieve.tex wheel_benchmark.tex run.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
