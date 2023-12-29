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

data SieveValues:
  | sieve-values(limit :: Number, num-bits :: Number, sieve-bits :: RawArray<Boolean>) with:
    method clear-bits(self, start-bit :: Number, end-bit :: Number, inc :: Number):
      range-by(start-bit, end-bit, inc)
        .each({(k :: Number): raw-array-set(self.sieve-bits, k, false)})
    end,

    method is-bit-set(self, bit :: Number) -> Boolean:
      raw-array-get(self.sieve-bits, bit)
    end,

    method do-sieve(self):
      q = num-floor((-3 + num-sqrt(3 + (2 * self.num-bits))) / 2)
      for each(bit from range(0, q + 1)):
        when self.is-bit-set(bit):
          self.clear-bits((2 * (bit + 1) * (bit + 2)) - 1, self.num-bits, (2 * bit) + 3)
        end
      end
    end,

    method print-primes(self) block:
      print(
        "2, " + 
        range(0, self.num-bits)
          .filter(self.is-bit-set)
          .map({(k :: Number) -> String: num-to-string((2 * k) + 3)})
          .join-str(", ")
          + "\n"
        )
    end,

    method count-primes(self) -> Number:
      1 + raw-array-to-list(self.sieve-bits)
        .filter({(x :: Boolean) -> Boolean: x})
        .length()
    end,

    method get-expected-primes-count(self) -> Number:
      ask:
        | self.limit == 10 then: 4
        | self.limit == 100 then: 25
        | self.limit == 1000 then: 168
        | self.limit == 10000 then: 1229
        | self.limit == 100000 then: 9592
        | self.limit == 1000000 then: 78498
        | otherwise: -1
      end
    end,

    method validate-primes-count(self, prime-count :: Number) -> Boolean:
      self.get-expected-primes-count() == prime-count
    end
end

data SieveResults:
  | sieve-results(passes :: Number, sieve-vals :: SieveValues, elapsed-time :: Number) with:
    method print-results(self, show-primes :: Boolean) block:
      when show-primes:
        self.sieve-vals.print-primes()
      end

      prime-count = self.sieve-vals.count-primes()
      is-valid = self.sieve-vals.validate-primes-count(prime-count)
      print(
        F.format(
          "Passes: ~a, Time: ~ams, Avg: ~ams, Limit: ~a, Count: ~a, Valid: ~a\n",
          [list:
            self.passes,
            self.elapsed-time,
            format-float(self.elapsed-time / self.passes),
            self.sieve-vals.limit,
            prime-count,
            is-valid
          ]
        )
      )
      print(
        F.format(
          "rzuckerm;~a;~a;1;algorithm=base,faithful=yes\n",
          [list:
            self.passes,
            format-float(self.elapsed-time / 1000)
          ]
        )
      )
    end
end

fun timed-do-sieve(limit :: Number, time-limit :: Number) -> SieveResults:
  start-time = time-now()
  rec inner-timed-do-sieve =
    lam(results :: SieveResults) -> SieveResults block:
      elapsed-time = time-now() - start-time
      if elapsed-time < time-limit block:
        num-bits = num-floor((limit - 1) / 2)
        values = sieve-values(limit, num-bits, raw-array-of(true, num-bits))
        values.do-sieve()
        inner-timed-do-sieve(sieve-results(results.passes + 1, values, elapsed-time))
      else:
        sieve-results(results.passes, results.sieve-vals, elapsed-time)
      end
    end

  values = sieve-values(0, 0, [raw-array:])
  inner-timed-do-sieve(sieve-results(0, values, 0))
end

fun run-sieve(opts :: D.StringDict) block:
  limit = opts.get-value("limit")
  time-limit = opts.get-value("time") * 1000
  show-primes = opts.has-key("s")

  sieve-res = timed-do-sieve(limit, time-limit)
  sieve-res.print-results(show-primes)
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
