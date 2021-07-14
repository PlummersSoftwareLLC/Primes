import 'dart:io';

Future<void> main(List<String> args) async {
  for (final program in args) {
    final result = await Process.run(program, []);
    stderr.write(result.stderr);
    stdout.writeln(result.stdout);
  }
}
