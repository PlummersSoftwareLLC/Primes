import 'dart:io';
import 'dart:math';

class PrimeSieve {
  // In Dart, underscore dangles are used in place of the private keyword
  // Dart, being an IL-based, interpreted language (similar to C#) uses the
  // system's default architecture for integers (i.e. 64-bit on 64-bit
  // processors and 32-bit on 32-bit processors)
  int _sieveSize = 0;

  // Dart doesn't really have vectors persay (without third-party libraries),
  // so a List will have to do, because I want to eliminate the need to have
  // the user install third-party libraries. Dart also doesn't have bit arrays.
  List<bool> _bits;

  // Because Dart uses the same integer type as the system, this code shouldn't
  // work on 32-bit systems because the numbers are too large.
  final Map<int, int> _resultsDictionary = {
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

  bool _validateResults() {
    MapEntry<int, int> result;

    try {
      result = _resultsDictionary.entries
          .singleWhere((element) => element.key == _sieveSize);
    } catch (e) {
      // This is required because when Dart tries to find a single element in
      // the map, if no result is found, an error is thrown. This is because we
      // don't proivde the optional [OrElse] clause to the singleWhere function.
      // We don't provide that function because it isn't helpful here since we
      // cannot return a value from the main function inside a callback (similar
      // to node.js)
      return false;
    }

    return result.value == countPrimes();
  }

  PrimeSieve(int n) {
    _bits = List.filled(n, true);
    _sieveSize = n;
  }

  void runSieve() {
    var factor = 3;
    var q = sqrt(_sieveSize).round();

    while (factor <= q) {
      for (var num = factor; num < _sieveSize; num += 2) {
        if (_bits[num]) {
          factor = num;
          break;
        }
      }

      for (var num = factor * factor; num < _sieveSize; num += factor * 2) {
        _bits[num] = false;
      }

      factor += 2;
    }
  }

  void printResults(bool showResults, double duration, int passes) {
    if (showResults) {
      stdout.write('2, ');
    }

    // Dart doesn't support interpreting booleans as integers (unlike
    // basically every other language in existence), so a workaround is
    // needed.
    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var num = 3; num <= _sieveSize; num += 2) {
      if (_bits[num]) {
        if (showResults) {
          stdout.write('$num, ');
          count++;
        }
      }
    }

    if (showResults) {
      stdout.write('\n');
    }

    stdout.write(
        'Passes: $passes, Time: $duration, Avg: ${duration / passes}, Limit: $_sieveSize, Count1: $count, Count2: ${countPrimes()}, Valid: ${_validateResults()}\n');
  }

  int countPrimes() {
    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var i = 3; i < _sieveSize; i += 2) {
      if (_bits[i]) {
        count++;
      }
    }

    return count;
  }
}

void main(List<String> arguments) {
  var passes = 0;
  var timer = Stopwatch();

  while (true) {
    timer.start();
    var sieve = PrimeSieve(1000000);
    sieve.runSieve();
    passes++;

    if (timer.elapsed.inSeconds >= 5) {
      sieve.printResults(false, timer.elapsed.inMicroseconds / 1000000, passes);
      break;
    }
  }
}
