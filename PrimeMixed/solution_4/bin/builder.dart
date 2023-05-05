import 'dart:io';
import 'add_extension.dart';

Future<void> main() async {
  final gcc = await Process.run('gcc', [
    '-c',
    '-Ofast',
    'c_library/c_sieve.c',
  ]);
  if (gcc.exitCode != 0) {
    throw Exception(gcc.stderr);
  }

  final dll = await Process.run('gcc', [
    '-shared',
    '-o',
    addExtension('c_sieve'),
    'c_sieve.o',
  ]);
  if (dll.exitCode != 0) {
    throw Exception(dll.stderr);
  }

  final pub = await Process.run('dart', [
    'pub',
    'get',
  ]);
  if (pub.exitCode != 0) {
    throw Exception(pub.stderr);
  }

  final ffigenC = await Process.run('dart', [
    'run',
    'ffigen',
    '--config',
    'ffigen_c_sieve.yaml',
  ]);
  if (ffigenC.exitCode != 0) {
    throw Exception(ffigenC.stderr);
  }
}
