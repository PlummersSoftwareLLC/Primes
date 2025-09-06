// Final optimized 8-bit prime sieve - combines all best techniques
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

  PrimeSieve(this._sieveSize)
      : _bits = Uint8List((_sieveSize + 1) >> 1);

  void clear() {
    _bits.fillRange(0, _bits.length, 0);
  }

  bool _validateResults() => _resultsDictionary[_sieveSize] == countPrimes();

  /// Final optimized sieve combining all best techniques:
  /// - Direct iteration (no factor finding overhead)
  /// - Maximum loop unrolling (16x)
  /// - Optimized memory access patterns
  /// - Reduced branching
  void runSieve() {
    final q = sqrt(_sieveSize).toInt();
    final bits = _bits;
    final sieveSize = _sieveSize;
    
    // Direct iteration through all odd numbers up to sqrt(n)
    // This eliminates the overhead of finding the next prime factor
    for (var factor = 3; factor <= q; factor += 2) {
      // Skip composite numbers efficiently
      if (bits[factor >> 1] == 1) continue;
      
      // Pre-calculate values for maximum performance
      final factor2 = factor << 1; // factor * 2
      final start = factor * factor;
      final end = sieveSize;
      
      // Maximum unrolling for marking multiples
      var num = start;
      
      // 16x unrolling for maximum CPU pipeline utilization
      while (num + factor2 * 16 < end) {
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
      }
      
      // 8x unrolling for remaining multiples
      while (num + factor2 * 8 < end) {
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
        bits[num >> 1] = 1; num += factor2;
      }
      
      // Handle final remaining multiples
      while (num < end) {
        bits[num >> 1] = 1;
        num += factor2;
      }
    }
  }

  void printResults(bool showResults, double duration, int passes) {
    if (showResults) {
      stderr.write('2, ');
    }

    var count = (_sieveSize >= 2) ? 1 : 0;

    for (var num = 3; num <= _sieveSize; num += 2) {
      if (_bits[num >> 1] == 0) {
        if (showResults) {
          stderr.write('$num, ');
        }
        count++;
      }
    }

    if (showResults) {
      stderr.write('\n');
      stderr.write('Passes: $passes, Time: $duration, ');
      stderr.write('Avg: ${duration / passes}, Limit: $_sieveSize, ');
      stderr.write('Count1: $count, Count2: ${countPrimes()}, ');
      stderr.write('Valid: ${_validateResults()}\n');
      stderr.write('\n');
    }

    stdout.write(
        'tarish_8bit;$passes;$duration;1;algorithm=base,faithful=yes,bits=8\n');
    stdout.writeln('Passes/s/t: ${passes / (duration * 1)}');
  }

  int countPrimes() {
    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var i = 3; i < _sieveSize; i += 2) {
      if (_bits[i >> 1] == 0) {
        count++;
      }
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
      sieve.printResults(true, timer.elapsedMicroseconds / 1000000, passes);
      break;
    }
  }
}
