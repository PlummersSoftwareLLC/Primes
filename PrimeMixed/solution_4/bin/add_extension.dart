import 'dart:io';

String addExtension(String name) {
  if (Platform.isWindows) {
    return '$name.dll';
  } else if (Platform.isMacOS) {
    return '$name.dylib';
  } else if (Platform.isLinux) {
    return '$name.so';
  } else {
    throw Exception('Invalid operating system');
  }
}
