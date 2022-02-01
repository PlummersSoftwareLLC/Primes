import 'dart:ffi';

import 'add_extension.dart';
import 'c_sieve.ffigen.dart';

void main() {
  final library = CSieve(DynamicLibrary.open(addExtension('c_sieve')));
  var passes = 0;
  final timer = Stopwatch()..start();

  while (true) {
    final sieve = library.construct_sieve(1000000);
    library.run_sieve(sieve);
    library.destruct_sieve(sieve);
    passes++;

    if (timer.elapsedMicroseconds >= 5000000) {
      break;
    }
  }

  final time = timer.elapsedMicroseconds / 1000000;
  print(
      'mmcdon20_dart+c_1_bit;$passes;$time;1;algorithm=base,faithful=yes,bits=1');
}
