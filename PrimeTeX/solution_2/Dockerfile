FROM phipsgabler/texlive-minimal@sha256:f1e37453ef11783e582033adc07ba908839c00889634d17364a3a7869bc7f56d

WORKDIR /opt/app

COPY shared_batteries.tex *_sieve.tex *_benchmark.tex runpdftex.sh texmf.cnf ./

ENTRYPOINT [ "sh", "runpdftex.sh" ]
