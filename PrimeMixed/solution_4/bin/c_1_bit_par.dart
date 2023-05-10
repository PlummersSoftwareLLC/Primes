import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:ffi';
import 'dart:math';

import 'add_extension.dart';
import 'c_sieve.ffigen.dart';

({double time, int passes}) work() {
  final library = CSieve(DynamicLibrary.open(addExtension('c_sieve')));
  var passes = 0;
  final timer = Stopwatch()..start();

  while (true) {
    final sieve = library.construct_sieve(1000000);
    library.run_sieve(sieve);
    library.destruct_sieve(sieve);
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
      'mmcdon20_dart+c_1_bit_par;$passes;$time;$processors;algorithm=base,faithful=yes,bits=1');
}
