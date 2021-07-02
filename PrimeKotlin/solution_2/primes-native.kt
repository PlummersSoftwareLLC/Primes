import kotlin.math.*
import kotlin.*

fun getSystemTimeInMillis() = kotlin.system.getTimeMillis()

val KNOWN_PRIMES = mapOf (
  10 to 4,
  100 to 25,
  1000 to 168,
  10000 to 1229,
  100000 to 9592,
  1000000 to 78498,
  10000000 to 664579,
  100000000 to 5761455
)

class PrimeSieve (max_limit: Int) {
  val limit: Int = max_limit
  // val bit_size = (limit + 1) >> 1
  val bit_size: Int = limit
  
  private var bits: BooleanArray = BooleanArray(bit_size)

  fun run_sieve() {
		var factor: Int = 3
		val q: Int = floor(sqrt(limit.toDouble())).toInt()
		
		while (factor <= q) {
			for (i in factor until bit_size step 2) {
				if (bits.get(i) == false)	{
					factor = i
					break
				}
			}
			
			for (i in factor * factor until bit_size step factor * 2) {
				bits.set(i,true)
      }
			
			factor += 2
		}
  }

  fun count_primes(): Int {
    var count: Int = 1
    for (i in 3 until bit_size step 2) {
				if (bits.get(i) == false)	{
          count++
        }
    }
    return count
  }

  fun validate_result(count: Int) : Boolean {
    return (KNOWN_PRIMES[limit] ?: 0 == count)
  }

  fun print_primes() {
    println(2)
    for (i in 3 until bit_size step 2) {
				if (bits.get(i) == false)	{
          println(i)
        }
    }
  }

  fun print_results(show_results: Boolean, duration:Float, passes:Int) {
    val count: Int = count_primes()
    val valid: Boolean = validate_result(count)
    val avg: Float = duration / passes

    if (show_results) {
      print_primes()
    }
    
    println("Passes: $passes, Time: $duration, Avg: $avg (sec/pass), Limit: $limit, Count: $count, Valid: $valid")
    // Following 2 lines are to conform to drag race output format
    println("")
    println("fvbakel_Kotlin_native;$passes;$duration;1;algorithm=base,faithful=yes,bits=1")

  }

}

fun main() {
  val max_limit: Int = 1_000_000
  val max_time: Int = 5
  val show_results : Boolean = false

  val max_time_ms: Int = max_time *1000
  var duration_ms : Int = 0
  var passes : Int = 0
  val start_time: Long = getSystemTimeInMillis()
  var sieve : PrimeSieve? = null


  while (duration_ms <= max_time_ms) {
    passes++
    sieve = PrimeSieve(max_limit)
    sieve.run_sieve()
    duration_ms = (getSystemTimeInMillis() - start_time).toInt()
  }

  sieve?.print_results(show_results,(duration_ms / 1000.toFloat()),passes)

}