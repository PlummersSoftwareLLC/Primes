import 'dart:async';
import 'dart:io';
import 'dart:isolate';
import 'dart:ffi';
import 'dart:math';

import 'add_extension.dart';
import 'c_sieve.ffigen.dart';

void work(SendPort port) {
  final library = CSieve(DynamicLibrary.open(addExtension('c_sieve')));
  var passes = 0;
  final timer = Stopwatch()..start();

  while (true) {
    final sieve = library.construct_sieve(1000000);
    library.run_sieve(sieve);
    library.destruct_sieve(sieve);
    passes++;

    if (timer.elapsedMicroseconds >= 5000000) {
      Isolate.exit(port, {
        'time': timer.elapsedMicroseconds / 1000000,
        'passes': passes,
      });
    }
  }
}

Future<void> main() async {
  final processors = Platform.numberOfProcessors;
  final receivePort = ReceivePort();
  for (var i = 0; i < processors; i++) {
    unawaited(Isolate.spawn(work, receivePort.sendPort));
  }
  var passes = 0;
  var time = 0.0;
  await for (Map<String, num> message in receivePort.take(processors)) {
    passes += message['passes'] as int;
    time = max(time, message['time'] as double);
  }
  receivePort.close();
  stdout.writeln(
      'mmcdon20_dart+c_1_bit_par;$passes;$time;$processors;algorithm=base,faithful=yes,bits=1');
}
