FROM erlang:28

WORKDIR /home/primes

COPY rebar.config run.sh ./
COPY src/*.erl src/*.src ./src/
ENTRYPOINT ["./run.sh"]
