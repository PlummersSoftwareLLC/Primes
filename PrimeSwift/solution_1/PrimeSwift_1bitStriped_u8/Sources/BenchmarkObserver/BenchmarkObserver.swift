// Compiled as a separate Swift module without cross-module optimization.
// Its opaque buffer read prevents the benchmark compiler from removing sieve work.
public func observe(_ pointer: UnsafeRawPointer?, at offset: Int) -> UInt64 {
    guard let pointer else { return 0 }
    return UInt64(pointer.load(fromByteOffset: offset, as: UInt8.self))
}
