// Importing dart:io to have access to console printing functions.
// This is a core dart library.
import 'dart:io';

// Importing dart:math to have access to sqrt() and round().
// This is a core dart library.
import 'dart:math';

// Importing dart:math to have access to Uint8List.
// This is a core dart library.
import 'dart:typed_data';

/// This class defines all the funcationality of the Prime Sieve itself, just as
/// Dave's original implementation does.
///
/// Some things to note, before proceeeding. First, you may notice there are
/// multiple types of comments in this file. Double slash comments "//" denote
/// single line comments like most languages. Triple slash comments "///" denote
/// documentation comments, these comments would generate documentation pages if
/// the DartDoc package was included and documentation was requested at build.
/// You may notice there are no traditional multiline comments "/* */" or
/// JavaDoc style comments "/** */", these are also supported, it's honestly
/// down to developer preference.
///
/// A few comments about the structure of Dart itself. In Dart classes contain
/// fields and methods. .NET developers will find this terminology quite
/// familiar, but other developers may not. Fields are simply the properties
/// of the class, such as the _sieveSize (I'll explain the underscore in a
/// second); and methods are the operations the class can perform, such as
/// void runSieve(). In Dart, our constructor is a method, with no return type,
/// whose name is the name of the class (i.e. PrimeSieve()). Finally, Dart
/// doesn't have keywords for member (field and method) visibility, so as a
/// workaround, any member whose name begins with an underscore "_" is a private
/// member and is not accessible outside the class.
class PrimeSieve {
  /// This field contains the size of the sieve.
  ///
  /// A quick note about numbers in Dart. Dart is an interpreted language, much
  /// like Java. As a result, Dart doesn't support specifying the integer size
  /// like a compiled language would. Therefore, Dart uses the system's
  /// architecture for the size of the integer. In other words, Dart will create
  /// a 64-bit integer on 64-bit systems, and a 32-bit integer on 32-bit
  /// systems.
  final int _sieveSize;

  /// This field contains the bits we are tracking
  ///
  /// Dart doesn't have Vectors like C++ does, or arrays like many other
  /// languages. Instead, in Dart we have Lists as our generic data structure.
  ///
  /// Also, as stated previously, Dart doesn't have a way to specify an integer
  /// size, but it does allow you to have lists of specific integer sizes.
  /// Values are truncated when placed into the list.
  final Int64List _bits;

  /// This field contains the results we would expect to find for any given
  /// [sieveSize].
  ///
  /// NOTE: This will probably cause the code to crash on 32-bit systems because
  /// the numbers in this Map are too large to fit in a 32-bit integer.
  ///
  /// Couple of Dart notes here. First, a map allows us store a list of linked
  /// values. In other words, if we request the value 10 from this map, it will
  /// always correlate to the value 4. Second, by declaring the value as static
  /// const we ensure that only one instance of the dictionaly is ever created.
  static const Map<int, int> _resultsDictionary = {
    10: 4,
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455,
    1000000000: 50847534,
    10000000000: 455052511
  };

  /// This method checks that the sieve produced a valid result, by checking the
  /// final number of Primes obtained against the expected value from the
  /// [resultsDictionary]
  bool _validateResults() => _resultsDictionary[_sieveSize] == countPrimes();

  /// This method returns true if [index] corresponds to a 1-bit.
  bool _getBit(int index) {
    assert(index % 2 != 0, "You're getting even bits, which is sub-optimal.");

    /// divide index by 2 and assign it to itself.
    /// this accounts for the fact that we do not store bits for even numbers.
    index >>= 1;

    /// I'll try to break down the following expression so that is clear how
    /// it works.
    ///
    /// "index >> 6" is equivalent to "index / 64". We divide the index by 64
    /// when we call _bits[index >> 6] because we are storing the bits in
    /// groups of 64 (Uint64 values). And this narrows it down to the right
    /// group of 64 bits.
    ///
    /// What "(1 << (index % 64))" does is it puts a 1 bit in the position
    /// within the 64 bits that we are interested in. For example if index
    /// is 133, then 133 % 64 is 5 and 1 << 5 is 0010_0000 in binary.
    ///
    /// When we & the two values together we will get back either the value
    /// 0010_0000 or 0000_0000 depending on weather or not the value returned
    /// by "_bits[index >> 6]" also contained a 1 in the same digit place.
    return (_bits[index >> 6] & (1 << (index % 64))) == 0;
  }

  /// This method sets the bit at [index] to 0.
  void _clearBit(int index) {
    assert(index % 2 != 0, "You're setting even bits, which is sub-optimal.");

    /// divide index by 2 and assign it to itself.
    /// this accounts for the fact that we do not store bits for even numbers.
    index >>= 1;

    /// This works similarly to the _getBit method above.
    /// Lets assume again that index is 133. "(1 << (index % 64))" is then equivalent
    /// to 0010_0000 in binary.
    ///
    /// Now when we | the two values any value in the same digit place as the 1 will
    /// also be set to 1.
    _bits[index >> 6] |= (1 << (index % 64));
  }

  /// This method constructs a new instance of the PrimeSieve object, where the
  /// [sieveSize] is set by the first positional argument, and the [bits] list
  /// will be initialized to 1/128 the [sieveSize] and filled with 0
  PrimeSieve(this._sieveSize) : _bits = Int64List((_sieveSize + 127) >> 7);

  /// This method runs the sieve. For more intormation about the algorithm,
  /// please check back to Dave's original video.
  void runSieve() {
    var factor = 3;
    final q = sqrt(_sieveSize).toInt();

    while (factor <= q) {
      for (var num = factor; num < _sieveSize; num += 2) {
        if (_getBit(num)) {
          factor = num;
          break;
        }
      }

      for (var num = factor * factor; num < _sieveSize; num += factor * 2) {
        _clearBit(num);
      }

      factor += 2;
    }
  }

  /// This method prints the results to the console. If [showResults] is true,
  /// the sieve will print all the primes it locates to the console. It will
  /// always print the [duration], the [passes], and other miscellaneous
  /// information.
  void printResults(bool showResults, double duration, int passes) {
    if (showResults) {
      stderr.write('2, ');
    }

    // Dart doesn't support interpreting booleans as intergers, unlike many
    // other programming languages. Therefore, this line checks if the sieveSize
    // is greater than or equal to 2. If it is, then the initial count is set to
    // 1 because 2 is prime. Otherwise, the count is set to 0 because there are
    // no primes less than 2.
    var count = (_sieveSize >= 2) ? 1 : 0;

    for (var num = 3; num <= _sieveSize; num += 2) {
      if (_getBit(num)) {
        if (showResults) {
          // In Dart, using the dollar sign "$" in the stdout.write method will
          // print a variable of the same name to the console.
          stderr.write('$num, ');
        }

        count++;
      }
    }

    if (showResults) {
      // Print a new line after the results if the results are shown.
      stderr.write('\n');

      // This is just for code readability. Since stdout.write doesn't print a new
      // line at the end of the string, we can split a large string across a few
      // print requests to keep the code to under 80 columns (personal preference).
      stderr.write('Passes: $passes, Time: $duration, ');
      stderr.write('Avg: ${duration / passes}, Limit: $_sieveSize, ');
      stderr.write('Count1: $count, Count2: ${countPrimes()}, ');
      stderr.write('Valid: ${_validateResults()}\n');

      // These 2 lines are for the drag race format
      stderr.write('\n');
    }

    stdout.write(
        'eagerestwolf&mmcdon20_1bit;$passes;$duration;1;algorithm=base,faithful=yes,bits=1\n');
  }

  int countPrimes() {
    var count = (_sieveSize >= 2) ? 1 : 0;

    for (var i = 3; i < _sieveSize; i += 2) {
      if (_getBit(i)) {
        count++;
      }
    }

    return count;
  }
}

/// The main method is where our code will begin execution when called from the
/// command line.
void main() {
  var passes = 0;
  final timer = Stopwatch()..start();

  while (true) {
    final sieve = PrimeSieve(1000000);
    sieve.runSieve();
    passes++;

    if (timer.elapsedMicroseconds >= 5000000) {
      sieve.printResults(false, timer.elapsedMicroseconds / 1000000, passes);
      break;
    }
  }
}
