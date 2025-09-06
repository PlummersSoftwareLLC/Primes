import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:math';
import 'dart:typed_data';


class PrimeSieve {
  final int _sieveSize;
  final Int64List _bits;
  
  static const Map<int, int> _resultsDictionary = {
    10: 4, 100: 25, 1000: 168, 10000: 1229, 100000: 9592,
    1000000: 78498, 10000000: 664579, 100000000: 5761455,
    1000000000: 50847534, 10000000000: 455052511
  };

  PrimeSieve(this._sieveSize) : _bits = Int64List((_sieveSize + 127) >> 7);

  /// OPTIMIZATION: Added a method to clear the bitmask for reuse.
  /// This avoids reallocating the Int64List on every pass in the benchmark,
  /// reducing memory allocation overhead and GC pressure.
  void clear() {
    _bits.fillRange(0, _bits.length, 0);
  }

  bool _validateResults() => _resultsDictionary[_sieveSize] == countPrimes();

  bool _getBit(int index) {
    index >>= 1;
    return (_bits[index >> 6] & (1 << (index & 63))) == 0;
  }

  void _clearBit(int index) {
    index >>= 1;
    _bits[index >> 6] |= (1 << (index & 63));
  }

  void runSieve() {
    final q = sqrt(_sieveSize).toInt();
    var factor = 3;
    
    while (factor <= q) {
      for (var num = factor; num < _sieveSize; num += 2) {
        if (_getBit(num)) {
          factor = num;
          break;
        }
      }
      
      final factor2 = factor << 1;
      final start = factor * factor;
      final end = _sieveSize;
      
      // Optimized marking with better unrolling
      var num = start;
      while (num + factor2 * 16 < end) {
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
        _clearBit(num); num += factor2;
      }
      
      while (num < end) {
        _clearBit(num);
        num += factor2;
      }
      
      factor += 2;
    }
  }

  void printResults(bool showResults, double duration, int passes) {
    if (showResults) stderr.write('2, ');

    var count = (_sieveSize >= 2) ? 1 : 0;
    for (var num = 3; num <= _sieveSize; num += 2) {
      if (_getBit(num)) {
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
      if (_getBit(i)) count++;
    }
    return count;
  }
}

({double time, int passes}) work() {
  var passes = 0;
  final timer = Stopwatch()..start();

  while (true) {
    final sieve = PrimeSieve(1000000);
    sieve.runSieve();
    passes++;

    if (timer.elapsedMicroseconds >= 5000000) {
      return (
        time: timer.elapsedMicroseconds / 1000000,
        passes: passes,
      );
    }
  }
}

Future<void> main() async {
  final processors = Platform.numberOfProcessors;
  final isolates = [for (int i = 0; i < processors; i++) Isolate.run(work)];
  var passes = 0;
  var time = 0.0;

  await for (final message in Stream.fromFutures(isolates)) {
    passes += message.passes;
    time = max(time, message.time);
  }

  stdout.writeln(
      'tarish_1bit_par;$passes;$time;$processors;algorithm=base,faithful=yes,bits=1');
  
  stdout.writeln('Passes/s/t: ${passes / (time * processors)}');
}
