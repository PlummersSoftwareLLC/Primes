import ArgumentParser

// Keep the range check and UInt64 conversion together, outside the timed pass.
func validatedNanoseconds(for seconds: Double) throws -> UInt64 {
    let nanoseconds = (seconds * 1_000_000_000).rounded(.up)
    guard seconds.isFinite, seconds >= 0,
          nanoseconds < Double(UInt64.max) else {
        throw ValidationError("Time must be finite, nonnegative, and representable in nanoseconds.")
    }
    return UInt64(nanoseconds)
}
