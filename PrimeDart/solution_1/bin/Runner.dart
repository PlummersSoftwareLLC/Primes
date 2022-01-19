import 'PrimeDart.dart' as prime_8_bit;
import 'PrimeDartOneBit.dart' as prime_1_bit;
import 'PrimeDartParallel.dart' as prime_8_bit_par;
import 'PrimeDartParallelOneBit.dart' as prime_1_bit_par;

Future<void> main() async {
  prime_8_bit.main();
  prime_1_bit.main();
  await prime_8_bit_par.main();
  await prime_1_bit_par.main();
}
