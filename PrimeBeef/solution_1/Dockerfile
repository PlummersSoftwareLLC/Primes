FROM rzuckerm/beef:0.43.5a1-3

WORKDIR /opt/app
COPY *.sh ./
COPY Primes/*.toml ./Primes/
COPY Primes/src/*.bf ./Primes/src/
RUN BeefBuild -workspace=Primes -config=Release
ENTRYPOINT ["./run_primes.sh"]
