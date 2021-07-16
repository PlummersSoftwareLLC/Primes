import 'dart:io';
import 'PrimeDartOneBit.dart';
import 'WorkerPool.dart';

void work() {
  final sieve = PrimeSieve(1000000);
  sieve.runSieve();
}

Future<void> main() async {
  final processors = Platform.numberOfProcessors;
  final pool = await WorkerPool.init(numberOfWorkers: processors);

  final timer = Stopwatch()..start();

  pool.broadcast(const Start(work: work));
  await Future.delayed(const Duration(seconds: 5));
  pool.broadcast(const Stop());

  timer.stop();

  final duration = timer.elapsedMicroseconds / 1000000;
  final passes = await pool.passes();
  stdout.writeln(
      'eagerestwolf&mmcdon20_1bit_par;$passes;$duration;$processors;algorithm=base,faithful=yes,bits=1');
}
