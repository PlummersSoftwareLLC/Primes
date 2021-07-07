# Kotlin/Multiplatform Idiomatic

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This version is more focused on a readable, idiomatic way to write Kotlin code,
avoiding compiler workarounds and thus probably more representative of what you
would find in a real-world project.

## Contributors

* [wulkanat](https://github.com/wulkanat)
* [fvbakel](https://github.com/fvbakel)
* [bluefireoly](https://github.com/bluefireoly)

## Further Remarks

- Kotlin's main strength is and has always been JVM support, and I believe it shows.
  JS and Native performance are both nowhere near Kotlin/JVM.
- Kotlin/JS is **incompatible** with Yarn 2.0 and **will** fail
  to build if you have Yarn Berry enabled.
- Kotlin/JS doesn't seem to be built for Number crushing. To remain compliant with
  the Kotlin specification, simple things like addition apparently are implemented as
  functions, and also resulting from that the generated files are huge. I do not
  believe Kotlin/JS is anywhere near ready for use anywhere where performance is
  even somewhat critical. Who knows, maybe that will change if Kotlin adds WASM as a
  target.