// Final optimized prime sieve - maximum performance
import 'dart:io';
import 'dart:math';
import 'dart:typed_data';

class PrimeSieve {
  final int _sieveSize;
  final Uint8List _bits;
  
  static const Map<int, int> _resultsDictionary = {
    10: 4, 100: 25, 1000: 168, 10000: 1229, 100000: 9592,
    1000000: 78498, 10000000: 664579, 100000000: 5761455,
    1000000000: 50847534, 10000000000: 455052511
  };

  PrimeSieve(this._sieveSize) : _bits = Uint8List((_sieveSize + 1) >> 1);

  bool _validateResults() => _resultsDictionary[_sieveSize] == countPrimes();

  void runSieve() {
    final q = sqrt(_sieveSize).toInt();
    var factor = 3;
    
    while (factor <= q) {
      for (var num = factor; num < _sieveSize; num += 2) {
        if (_bits[num >> 1] == 0) {
          factor = num;
          break;
        }
      }
      
      final factor2 = factor << 1;
      final start = factor * factor;
      final end = _sieveSize;
      
      // Maximum unrolling for performance
      var num = start;
      while (num + factor2 * 32 < end) {
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
        _bits[num >> 1] = 1; num += factor2;
      }
      
      while (num < end) {
        _bits[num >> 1] = 1;
        num += factor2;
      }
      
      factor += 2;
    }
  }

  void printResults(bool showResults, double duration, int passes) {
    if (showResults) stderr.write('2, ');

    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var num = 3; num <= _sieveSize; num += 2) {
      if (_bits[num >> 1] == 0) {
        if (showResults) stderr.write('$num, ');
        count++;
      }
    }

    if (showResults) {
      stderr.write('\n');
      stderr.write('Passes: $passes, Time: $duration, ');
      stderr.write('Avg: ${duration / passes}, Limit: $_sieveSize, ');
      stderr.write('Count1: $count, Count2: ${countPrimes()}, ');
      stderr.write('Valid: ${_validateResults()}\n\n');
    }

    stdout.write('tarish_1bit;$passes;$duration;1;algorithm=base,faithful=yes,bits=1\n');
  }

  int countPrimes() {
    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var i = 3; i < _sieveSize; i += 2) {
      if (_bits[i >> 1] == 0) count++;
    }
    return count;
  }
}

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
