import 'prime_8_bit.dart' as prime_8_bit;
import 'prime_1_bit.dart' as prime_1_bit;
import 'prime_8_bit_par.dart' as prime_8_bit_par;
import 'prime_1_bit_par.dart' as prime_1_bit_par;

Future<void> main() async {
  prime_8_bit.main();
  prime_1_bit.main();
  await prime_8_bit_par.main();
  await prime_1_bit_par.main();
}
