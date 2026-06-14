#!/bin/sh
LIMIT=1000000
TIME=5.0
SHOW_RESULTS=0
while [ $# -gt 0 ]
do
    case "$1" in
        --limit|-l)
            shift
            LIMIT="$1"
            shift
            ;;
        --time|-t)
            shift
            TIME="$1"
            shift
            ;;
        --show-results|-s)
            SHOW_RESULTS=1
            shift
            ;;
        *)
            echo "Invalid option"
            exit 1
            ;;
    esac
done

for script in primes
do
    printf "%s\n" "${LIMIT}" "${TIME}" "${SHOW_RESULTS}" | ./${script} "$@" | sed 's/ *;/;/g'
    echo ""
done
