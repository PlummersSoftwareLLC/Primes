import cmdline as C
import string-dict as D
import error as ERR
import format as F

fun println(str) block:
  print(str)
  print("\n")
end

fun show-help(options :: D.StringDict, message :: String) block:
  each(println, link(message, C.usage-info(options)))
  raise(ERR.exit-quiet(1))
end

fun run-sieve(opts :: D.StringDict) block:
  limit = opts.get-value("limit")
  time-limit = opts.get-value("time") * 1000

  start-time = time-now()
  rec timed-do-sieve =
    lam(results) block:
      elapsed-time = time-now() - start-time
      if elapsed-time >= time-limit:
        {passes: results.passes, sieve-bits: results.sieve-bits, elapsed-time: elapsed-time}
      else:
        timed-do-sieve(
          {passes: results.passes + 1, sieve-bits: do-sieve(limit), elapsed-time: elapsed-time}
        )
      end
    end

  sieve-results = timed-do-sieve({passes: 0, sieve-bits: empty, elapsed-time: 0})
  when opts.has-key("s"):
    print-primes(sieve-results.sieve-bits)
  end

  prime-count = count-primes(sieve-results.sieve-bits)
  is-valid = validate-primes-count(limit, prime-count)
  print(
    F.format(
      "Passes: ~a, Time: ~ams, Avg: ~ams, Limit: ~a, Count: ~a, Valid: ~a\n",
      [list:
        sieve-results.passes,
        sieve-results.elapsed-time,
        format-float(sieve-results.elapsed-time / sieve-results.passes),
        limit,
        prime-count,
        is-valid
      ]
    )
  )
  print(
    F.format(
      "rzuckerm;~a;~a;1;algorithm=base,faithful=yes\n",
      [list:
        sieve-results.passes,
        format-float(sieve-results.elapsed-time / 1000)
      ]
    )
  )
end

fun do-sieve(limit :: Number) -> RawArray<Boolean> block:
  num-bits = num-floor((limit - 1) / 2)
  sieve-bits = raw-array-of(true, num-bits)

  q = num-floor((-3 + num-sqrt(3 + (2 * num-bits))) / 2)
  clr-bit = {(k :: Number): raw-array-set(sieve-bits, k, false)}
  for each(bit from range(0, q + 1)):
    when raw-array-get(sieve-bits, bit):
      range-by((2 * (bit + 1) * (bit + 2)) - 1, num-bits, (2 * bit) + 3)
        .each(clr-bit)
    end
  end

  sieve-bits
end

fun print-primes(sieve-bits :: RawArray<Boolean>) block:
  num-bits = raw-array-length(sieve-bits)
  is-bit-set = {(x :: Number) -> Boolean: raw-array-get(sieve-bits, num-floor((x - 3) / 2))}
  print(
    "2, " + 
    range-by(3, (2 * num-bits) + 3, 2)
      .filter(is-bit-set)
      .map(num-to-string)
      .join-str(", ")
      + "\n"
  )
end

fun count-primes(sieve-bits :: RawArray<Boolean>) -> Number block:
  1 + raw-array-to-list(sieve-bits)
    .filter({(x :: Boolean) -> Boolean: x})
    .length()
end

fun get-expected-primes-count(limit :: Number) -> Number:
  ask:
    | limit == 10 then: 4
    | limit == 100 then: 25
    | limit == 1000 then: 168
    | limit == 10000 then: 1229
    | limit == 100000 then: 9592
    | limit == 1000000 then: 78498
    | otherwise: -1
  end
end

fun validate-primes-count(limit :: Number, prime-count :: Number) -> Boolean:
  get-expected-primes-count(limit) == prime-count
end

fun format-float(n :: Number) -> String block:
  str = num-to-string(num-to-roughnum(n))
  string-substring(str, 1, string-length(str))
end

options = [D.string-dict:
  "limit", C.equals-val-default(C.Num, 1000000, none, C.once, "Upper limit for calculating primes"),
  "time", C.equals-val-default(C.Num, 5, none, C.once, "Time limit in seconds"),
  "s", C.flag(C.once, "Print found prime numbers"),
  "h", C.flag(C.once, "Show help"),
]
parsed = C.parse-cmdline(options)
cases (C.ParsedArguments) parsed:
  | success(opts, _) =>
    if opts.has-key("h"): show-help(options, "")
    else: run-sieve(opts)
    end
  | arg-error(message, _) => show-help(options, message)
end
