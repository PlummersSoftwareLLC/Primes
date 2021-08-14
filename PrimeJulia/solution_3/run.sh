for primes_jl_f in primes_*.jl; do
    julia "$primes_jl_f" "$@"
    echo >&2
done