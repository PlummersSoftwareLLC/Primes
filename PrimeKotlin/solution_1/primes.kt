import kotlin.math.*
import kotlin.*

class PrimeSieve (max_limit: Int) {
  val limit: Int = max_limit
  // val bit_size = (limit + 1) >> 1
  val bit_size: Int = limit
  
  private var bits: BooleanArray = BooleanArray(bit_size)

  fun run_sieve() {
		var factor: Int = 3
		val q: Int = floor(sqrt(limit.toDouble())).toInt()
		
		while (factor < q) {
			for (i in factor until bit_size) {
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

  fun print_primes() {
    println(2)
    for (i in 3 until bit_size step 2) {
				if (bits.get(i) == false)	{
          println(i)
        }
    }
  }

}



fun main() {
  println("Hello World!")

  var sieve = PrimeSieve(100)
  sieve.run_sieve()
  println(sieve.count_primes())
  sieve.print_primes()

}