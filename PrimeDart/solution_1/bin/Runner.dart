import 'dart:io';

Future<void> main(List<String> args) async {
  for (final program in args) {
    final result = await Process.run(program, []);
    stdout.write(result.stdout);
  }
}
