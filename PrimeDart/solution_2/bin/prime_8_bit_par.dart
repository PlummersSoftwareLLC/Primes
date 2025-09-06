import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:math';
import 'prime_8_bit.dart';

/// OPTIMIZATION: The work function now reuses a single PrimeSieve object.
/// This avoids the overhead of object creation and Uint8List allocation
/// inside the main benchmark loop.
({double time, int passes}) work() {
  var passes = 0;
  final timer = Stopwatch()..start();
  // Create the sieve object once, outside the hot loop.
  final sieve = PrimeSieve(1000000);

  while (true) {
    // Instead of creating a new sieve, just clear the existing one.
    sieve.clear();
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
      'tarish_8bit_par;$passes;$time;$processors;algorithm=base,faithful=yes,bits=8');

  stdout.writeln('Passes/s/t: ${passes / (time * processors)}');
}