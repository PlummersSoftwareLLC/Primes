import std/io
import std/time
import std/field
import std/control

pub event tick { deadline: i128, passes: i64 }
| live { deadline: i128, passes: i64 }
| expired i64

pub event run { deadline: i128 } -> i64

tick = std/time:now()
| t n |> if(n < deadline)
    | then => live { deadline, passes }
    | else => expired passes

run = #L tick(deadline, passes: 0)
| live l |> std/field:new(bits: 500000)
    | field f |> for(1..500)
        ! each i |> std/field:test(f, i): pv |> if(pv == 0)
            | then |> std/field:mark-multiples(f, 2 * i * (i + 1), 2 * i + 1, 499999)
        | done |> std/field:free(f) |> @L(l.deadline, passes: l.passes + 1)
    | err _ |> _
| expired e -> e

std/time:now()
| t t0 |> run(deadline: t0 + 5000000000): n |> std/time:now()
    | t t1 |> std/field:new(bits: 500000)
        | field g |> for(1..500)
            ! each i |> std/field:test(g, i): pv |> if(pv == 0)
                | then |> std/field:mark-multiples(g, 2 * i * (i + 1), 2 * i + 1, 499999)
            | done |> std/field:count-zeros(g, 1, 500000): c |> std/io:eprint.ln("validated primes: {{ c + 1:d }}") |> std/io:print.ln("korulang;{{ n:d }};{{ @as(f64, @floatFromInt(t1 - t0)) / 1000000000.0:f }};1;algorithm=base,faithful=yes,bits=1") |> std/field:free(g)
        | err _ |> _
