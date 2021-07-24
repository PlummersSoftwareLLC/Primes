// The stdio package gives us access to console printing functions.
//
// This is a built-in package
import core.stdc.stdio;

// We have the math package included to have access to the sqrt function
//
// This is a built-in package
import std.math;

// The stopwatch package is what we use to time the actual running of the sieve
//
// This is a built-in package
import std.datetime.stopwatch;

// The conv package allows us to convert values to other types
//
// This is a built-in package
import std.conv;

/**
 * This class encapsulates all the functionality and the state of the prime
 * sieve.
 *
 * You may notice that classes in D look similar to a lot of different
 * languages, but really similar (in my opinion) to C#, but then again C# is
 * my primary language. There are a few other oddities that we'll discuss as
 * we go along.
 */
class PrimeSieve {
  /**
   * This property holds the size of the sieve.
   *
   * Also, notice that in D property declarations are similar to TypeScript and
   * Java. It's also similar to C#, with the exception that getter and setter
   * declarations aren't supported.
   */
  private long sieveSize = 0;

  /**
   * This property holds the bit array that we use to count the number of
   * primes.
   *
   * Note: we use a boolean array instead of a BitArray, which is built into
   * D because initializing the BitArray is kind of a pain.
   */
  private bool[] bits;

  /**
   * This is what D calls an associative array. In concept, it's similar to a
   * map or a hashmap in other programming languages. When declaring an
   * associative array, the type of the values is outside the brackets, and the
   * type of the keys is inside the brackets. So, this associative array
   * contains long keys and int values.
   *
   * You may notice that altough this property is immutable, we haven't set its
   * value...that's because we can't. This hasn't been implemented in the
   * language yet (for associative arrays at least), so we declare it here and
   * we'll define it in the constructor. Should you try to assign the value
   * here, the compiler will generate an error telling you the right hand
   * operand is not contant (spoiler alert: it is). This is acknoledged in the
   * associative array documentation on the D Wiki.
   *
   * This property is marked as immutable because it is not allowed to be
   * modified once the class is instantiated.
   */
  private immutable int[long] resultsDictionary;

  /**
   * This (pun intended) is the constructor. In D, a constructor is always a
   * function taking whatever arguments you want, called this...makes sense
   * honestly. Although, D follows the C#/JavaScript approach that while you're
   * in the class, the this keyword is not requied and actually discouraged.
   */
  this(long n) {
    // Make the bits array a boolean array n members long
    bits = new bool[n];
    
    // Set all the bits to true or 1
    //
    // Note the use of the spread operator here. ES6 developers will be
    // familiar with this operator and its function. For everyone else, the
    // spread operator basically means "and everything in between", so if I
    // were to write 0 .. 99, that would mean 0 to 99 and all the numbers in
    // between 0 and 99, or 0-99. A very handy thing to have in a programming
    // language, saves the developer a for loop.
    bits[0 .. n] = true;

    // Set the sieve size to n
    sieveSize = n;

    // Since we couldn't define the resultsDictionary earler, we'll do it here
    //
    // You may notice the L's at the end of each key, that indicates that value
    // is a long integer. Those familiar with Dave's original implementation
    // may notice it's not LL. That's because in D, long is equivalent to C and
    // C++'s long long.
    //
    // As for those underscores...D has a really nifty feature. In any numeric
    // value, any instance of the underscore character is ignored and removed
    // by the compiler. Why? To improve readability. This allows developers to
    // make demarcations to say mark thousands, millions, etc; however, how
    // this is used is up to the developer. Suffice it to say you'll never need
    // to count 0's again when looking at numbers 100_000_000 will always be 
    // one hunred million and never again a billion.
    resultsDictionary = [
      10L: 4,
      100L: 25,
      1_000L: 168,
      10_000L: 1_229,
      100_000L: 9_592,
      1_000_000L: 78_498,
      10_000_000L: 664_579,
      100_000_000L: 5_761_455,
      1_000_000_000L: 50_847_534,
      10_000_000_000L: 455_052_511
    ];
  }

  /**
   * This method runs the sieve. Again, for more information about this
   * algorithm, check out Dave's original video.
   */
  void runSieve() {
    int factor = 3;
    int q = cast(int) sqrt(to!real(sieveSize));

    while (factor <= q) {
      for (int num = factor; num < sieveSize; num += 2) {
        if (bits[num]) {
          factor = num;
          break;
        }
      }

      for (int num = factor * factor; num < sieveSize; num += factor * 2) {
        bits[num] = false;
      }

      factor += 2;
    }
  }

  /**
   * This method prints the results to the console, with pretty formatting to
   * boot (legal disclaimer, it's not really all that pretty). It takes a
   * boolean value to determine whether or not to show the prime numbers found,
   * a double precision floating point number indicting how many milliseconds it
   * took, and how many passes were completed.
   */
  void printResults(bool showResults, double duration, int passes) {
    if (showResults) {
      fprintf(stderr, "2, ");
    }

    int count = (sieveSize >= 2);
    for (int num = 3; num <= sieveSize; num += 2) {
      if (bits[num]) {
        if (showResults) {
          fprintf(stderr, "%d, ", num);
        }

        count++;
      }
    }

    if (showResults) {
      fprintf(stderr, "\n");
    }

    fprintf(stderr, "Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n",
      passes,
      duration,
      duration / passes,
      sieveSize,
      count,
      countPrimes(),
      // Note printing booleans to the console is interpreted as an integer
      validateResults());

    fprintf(stderr, "\n");       
    printf("eagerestwolf;%d;%f;1;algorithm=base,faithful=yes,bits=8\n", passes, duration);
  }

  /**
   * Count the number of primes in the bit array
   */
  int countPrimes() {
    int count = (sieveSize >= 2);
    for (int i = 3; i < sieveSize; i += 2) {
      if (bits[i]) {
        count++;
      }
    }

    return count;
  }

  /**
   * Check that the number of primes in the bits array is correct
   */
  private bool validateResults() {
    // This line is kinda nifty. We want to get the value corresponding with
    // our sieveSize from the associative array that contains our expected
    // results, so we just call resultsDictionary.get() with our sieveSize.
    // Since our sieveSize and the associative array keys are the same type
    // (long), this just works, and the D compiler knows that. The -1 just
    // tells the D compiler that if it can't find our sieveSize in the array,
    // then just return -1
    auto result = resultsDictionary.get(sieveSize, -1);

    // D allows standard boolean comparisons (shown below "==") and Python-ish
    // style is comparisons. The main difference here is in Python, is means
    // are the left and right operands point to the same object. The Python
    // equivalent would be, ironically enough, "==". Addionally, the reverse
    // works here as well, to check for inequality you can use "!=" or "!is"
    // that is "not equal to" and "not is" respectively.
    if (result is -1) {
      return false;
    }

    // Here is that example of a standard boolean operator. Nothing else fancy
    // here.
    return result == countPrimes();
  }
}

void main()
{
  auto passes = 0;
  auto sw = StopWatch(AutoStart.yes);

  while (true) {
    auto sieve = new PrimeSieve(1_000_000L);
    sieve.runSieve();
    passes++;

    if (sw.peek() >= msecs(5000)) {
      sw.stop();
      // I need to point out something here. Notice that ! in the middle of the
      // function definition. Some properties/methods/etc in D support overload
      // the ! urnary operator to support inline casting to other values. When
      // used in this fashion, it's called the cast operator.
      sieve.printResults(false, sw.peek.total!"usecs" / 1_000_000, passes);
      break;
    }
  }
}
