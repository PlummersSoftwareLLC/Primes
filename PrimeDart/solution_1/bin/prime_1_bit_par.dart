import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:math';
import 'prime_1_bit.dart';

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
      'eagerestwolf&mmcdon20_1bit_par;$passes;$time;$processors;algorithm=base,faithful=yes,bits=1');
}
