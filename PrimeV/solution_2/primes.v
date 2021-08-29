import time

type Prime = u64

const (
	limit = Prime(1_000_000)
	cpul1cache = 16384
	results = {		
	    Prime(10): 4
	    Prime(100): 25
	    Prime(1000): 168
	    Prime(10000): 1229
	    Prime(100000): 9592
	    Prime(1000000): 78498
	    Prime(10000000): 664579
	    Prime(100000000): 5761455
	    Prime(1000000000): 50847534
	    Prime(10000000000): 455052511
	}
	result = results[limit]
	bitmask = [ u8(1), u8(2), u8(4), u8(8), u8(16), u8(32), u8(64), u8(128) ]
)

enum Technique {
	bit_twiddle
	stride8
	stride8_block
	extreme
	extreme_hybrid
}

struct PrimeSieve {
	sieve_size Prime
    mut: sieve_buffer []u8
}

[direct_array_access]
fn new_prime_sieve(lmt Prime, tec Technique) PrimeSieve {
	bitlmt := int((lmt - 3) >> 1)
	size := ((bitlmt + 64) >> 3) & (-8) // round to next 64-bit
	mut sieve := PrimeSieve {
		sieve_size: lmt
		sieve_buffer: []u8{len: size, cap: size}
	}

	match tec {
		.bit_twiddle {			
			for i := 0; ; i++ {
				mut swi := (i + i) * (i + 3) + 3 // computer mark start index
				if swi > bitlmt { break }
				if (sieve.sieve_buffer[i >> 3] & bitmask[i & 7]) != u8(0) { continue }
				bp := i + i + 3
				for ; swi <= bitlmt; swi += bp {
					sieve.sieve_buffer[swi >> 3] |= bitmask[swi & 7]
				}
			}
		}
		.stride8 {		
			for i := 0; ; i++ {
				mut swi := (i + i) * (i + 3) + 3 // computer mark start index
				if swi > bitlmt { break }
				if (sieve.sieve_buffer[i >> 3] & bitmask[i & 7]) != u8(0) { continue }
				bp := i + i + 3
				tstlmt := swi + (bp << 3) - 1
				slmt := if tstlmt > bitlmt { bitlmt } else { tstlmt }
				for ; swi <= slmt; swi += bp {
					mask := bitmask[swi & 7]
					for cbytendx := swi >> 3; cbytendx < size; cbytendx += bp {
						sieve.sieve_buffer[cbytendx] |= mask
					}
				}
			}
		}
		.stride8_block {
			mut strts := []int{len: 8, cap: 8}
			for i := 0; ; i++ {
				mut swi := (i + i) * (i + 3) + 3 // computer mark start index
				if swi > bitlmt { break }
				if (sieve.sieve_buffer[i >> 3] & bitmask[i & 7]) != u8(0) { continue }
				bp := i + i + 3
				bp2 := bp + bp
				bp3 := bp + bp2
				bp4 := bp + bp3
				pgndx0 := (swi >> 3) & (-8)
				for x := 0; x < 8; x++ {
					strts[swi & 7] = swi >> 3
					swi += bp
				}
				for pgndx := pgndx0; pgndx < size; pgndx += cpul1cache {
					pglmt := if pgndx + cpul1cache > size { size - 1 }
								else { pgndx + cpul1cache - 1 }
					pgstp := pglmt - bp3
					for si := 0; si < 8; si++ {
						mut bytendx := strts[si]
						mask := bitmask[si]
						for ; bytendx < pgstp; bytendx += bp4 {
							sieve.sieve_buffer[bytendx] |= mask
							sieve.sieve_buffer[bytendx + bp] |= mask
							sieve.sieve_buffer[bytendx + bp2] |= mask
							sieve.sieve_buffer[bytendx + bp3] |= mask
						}
						for ; bytendx < pglmt; bytendx += bp {
							sieve.sieve_buffer[bytendx] |= mask
						}
						strts[si] = bytendx
					}
				}
			}
		}
		.extreme, .extreme_hybrid {
			for i := 0; ; i++ {
				mut swi := (i + i) * (i + 3) + 3 // computer mark start index
				if swi > bitlmt { break }
				if (sieve.sieve_buffer[i >> 3] & bitmask[i & 7]) != u8(0) { continue }
				bp := i + i + 3
				if tec == Technique.extreme_hybrid && bp < 20 {
					ilmt := swi | 63
					for ; swi <= ilmt; swi += bp {
						sieve.sieve_buffer[swi >> 3] |= bitmask[swi & 7]
					}
					byteadv := bp << 3
					bytelmt := size - (byteadv - 8)
					mut bytendx := (swi >> 3) & (-8)
					swi &= 63
					match bp {
						3 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000004
									v |= 0x0000000000000020
									v |= 0x0000000000000100
									v |= 0x0000000000000800
									v |= 0x0000000000004000
									v |= 0x0000000000020000
									v |= 0x0000000000100000
									v |= 0x0000000000800000
									v |= 0x0000000004000000
									v |= 0x0000000020000000
									v |= 0x0000000100000000
									v |= 0x0000000800000000
									v |= 0x0000004000000000
									v |= 0x0000020000000000
									v |= 0x0000100000000000
									v |= 0x0000800000000000
									v |= 0x0004000000000000
									v |= 0x0020000000000000
									v |= 0x0100000000000000
									v |= 0x0800000000000000
									wordp[0] = v | 0x4000000000000000
									v = wordp[1] | 0x0000000000000002
									v |= 0x0000000000000010
									v |= 0x0000000000000080
									v |= 0x0000000000000400
									v |= 0x0000000000002000
									v |= 0x0000000000010000
									v |= 0x0000000000080000
									v |= 0x0000000000400000
									v |= 0x0000000002000000
									v |= 0x0000000010000000
									v |= 0x0000000080000000
									v |= 0x0000000400000000
									v |= 0x0000002000000000
									v |= 0x0000010000000000
									v |= 0x0000080000000000
									v |= 0x0000400000000000
									v |= 0x0002000000000000
									v |= 0x0010000000000000
									v |= 0x0080000000000000
									v |= 0x0400000000000000
									wordp[1] = v | 0x2000000000000000
									v = wordp[2] | 0x0000000000000001
									v |= 0x0000000000000008
									v |= 0x0000000000000040
									v |= 0x0000000000000200
									v |= 0x0000000000001000
									v |= 0x0000000000008000
									v |= 0x0000000000040000
									v |= 0x0000000000200000
									v |= 0x0000000001000000
									v |= 0x0000000008000000
									v |= 0x0000000040000000
									v |= 0x0000000200000000
									v |= 0x0000001000000000
									v |= 0x0000008000000000
									v |= 0x0000040000000000
									v |= 0x0000200000000000
									v |= 0x0001000000000000
									v |= 0x0008000000000000
									v |= 0x0040000000000000
									v |= 0x0200000000000000
									v |= 0x1000000000000000
									wordp[2] |= v | 0x8000000000000000
								}
							}
						}
						5 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000004
									v |= 0x0000000000000080
									v |= 0x0000000000001000
									v |= 0x0000000000020000
									v |= 0x0000000000400000
									v |= 0x0000000008000000
									v |= 0x0000000100000000
									v |= 0x0000002000000000
									v |= 0x0000040000000000
									v |= 0x0000800000000000
									v |= 0x0010000000000000
									v |= 0x0200000000000000
									wordp[0] = v | 0x4000000000000000
									v = wordp[1] | 0x0000000000000008
									v |= 0x0000000000000100
									v |= 0x0000000000002000
									v |= 0x0000000000040000
									v |= 0x0000000000800000
									v |= 0x0000000010000000
									v |= 0x0000000200000000
									v |= 0x0000004000000000
									v |= 0x0000080000000000
									v |= 0x0001000000000000
									v |= 0x0020000000000000
									v |= 0x0400000000000000
									wordp[1] = v | 0x8000000000000000
									v = wordp[2] | 0x0000000000000010
									v |= 0x0000000000000200
									v |= 0x0000000000004000
									v |= 0x0000000000080000
									v |= 0x0000000001000000
									v |= 0x0000000020000000
									v |= 0x0000000400000000
									v |= 0x0000008000000000
									v |= 0x0000100000000000
									v |= 0x0002000000000000
									v |= 0x0040000000000000
									wordp[2] = v | 0x0800000000000000
									v = wordp[3] | 0x0000000000000001
									v |= 0x0000000000000020
									v |= 0x0000000000000400
									v |= 0x0000000000008000
									v |= 0x0000000000100000
									v |= 0x0000000002000000
									v |= 0x0000000040000000
									v |= 0x0000000800000000
									v |= 0x0000010000000000
									v |= 0x0000200000000000
									v |= 0x0004000000000000
									v |= 0x0080000000000000
									wordp[3] = v | 0x1000000000000000
									v = wordp[4] | 0x0000000000000002
									v |= 0x0000000000000040
									v |= 0x0000000000000800
									v |= 0x0000000000010000
									v |= 0x0000000000200000
									v |= 0x0000000004000000
									v |= 0x0000000080000000
									v |= 0x0000001000000000
									v |= 0x0000020000000000
									v |= 0x0000400000000000
									v |= 0x0008000000000000
									v |= 0x0100000000000000
									wordp[4] = v | 0x2000000000000000
								}
							}
						}
						7 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000002
									v |= 0x0000000000000100
									v |= 0x0000000000008000
									v |= 0x0000000000400000
									v |= 0x0000000020000000
									v |= 0x0000001000000000
									v |= 0x0000080000000000
									v |= 0x0004000000000000
									wordp[0] = v | 0x0200000000000000
									v = wordp[1] | 0x0000000000000001
									v |= 0x0000000000000080
									v |= 0x0000000000004000
									v |= 0x0000000000200000
									v |= 0x0000000010000000
									v |= 0x0000000800000000
									v |= 0x0000040000000000
									v |= 0x0002000000000000
									v |= 0x0100000000000000
									wordp[1] = v | 0x8000000000000000
									v = wordp[2] | 0x0000000000000040
									v |= 0x0000000000002000
									v |= 0x0000000000100000
									v |= 0x0000000008000000
									v |= 0x0000000400000000
									v |= 0x0000020000000000
									v |= 0x0001000000000000
									v |= 0x0080000000000000
									wordp[2] = v | 0x4000000000000000
									v = wordp[3] | 0x0000000000000020
									v |= 0x0000000000001000
									v |= 0x0000000000080000
									v |= 0x0000000004000000
									v |= 0x0000000200000000
									v |= 0x0000010000000000
									v |= 0x0000800000000000
									v |= 0x0040000000000000
									wordp[3] = v | 0x2000000000000000
									v = wordp[4] | 0x0000000000000010
									v |= 0x0000000000000800
									v |= 0x0000000000040000
									v |= 0x0000000002000000
									v |= 0x0000000100000000
									v |= 0x0000008000000000
									v |= 0x0000400000000000
									v |= 0x0020000000000000
									wordp[4] = v | 0x1000000000000000
									v = wordp[5] | 0x0000000000000008
									v |= 0x0000000000000400
									v |= 0x0000000000020000
									v |= 0x0000000001000000
									v |= 0x0000000080000000
									v |= 0x0000004000000000
									v |= 0x0000200000000000
									v |= 0x0010000000000000
									wordp[5] = v | 0x0800000000000000
									v = wordp[6] | 0x0000000000000004
									v |= 0x0000000000000200
									v |= 0x0000000000010000
									v |= 0x0000000000800000
									v |= 0x0000000040000000
									v |= 0x0000002000000000
									v |= 0x0000100000000000
									v |= 0x0008000000000000
									wordp[6] = v | 0x0400000000000000
								}
							}
						}
						9 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000004
									v |= 0x0000000000000800
									v |= 0x0000000000100000
									v |= 0x0000000020000000
									v |= 0x0000004000000000
									v |= 0x0000800000000000
									wordp[0] = v | 0x0100000000000000
									v = wordp[1] | 0x0000000000000002
									v |= 0x0000000000000400
									v |= 0x0000000000080000
									v |= 0x0000000010000000
									v |= 0x0000002000000000
									v |= 0x0000400000000000
									wordp[1] = v | 0x0080000000000000
									v = wordp[2] | 0x0000000000000001
									v |= 0x0000000000000200
									v |= 0x0000000000040000
									v |= 0x0000000008000000
									v |= 0x0000001000000000
									v |= 0x0000200000000000
									v |= 0x0040000000000000
									wordp[2] = v | 0x8000000000000000
									v = wordp[3] | 0x0000000000000100
									v |= 0x0000000000020000
									v |= 0x0000000004000000
									v |= 0x0000000800000000
									v |= 0x0000100000000000
									v |= 0x0020000000000000
									wordp[3] = v | 0x4000000000000000
									v = wordp[4] | 0x0000000000000080
									v |= 0x0000000000010000
									v |= 0x0000000002000000
									v |= 0x0000000400000000
									v |= 0x0000080000000000
									v |= 0x0010000000000000
									wordp[4] = v | 0x2000000000000000
									v = wordp[5] | 0x0000000000000040
									v |= 0x0000000000008000
									v |= 0x0000000001000000
									v |= 0x0000000200000000
									v |= 0x0000040000000000
									v |= 0x0008000000000000
									wordp[5] = v | 0x1000000000000000
									v = wordp[6] | 0x0000000000000020
									v |= 0x0000000000004000
									v |= 0x0000000000800000
									v |= 0x0000000100000000
									v |= 0x0000020000000000
									v |= 0x0004000000000000
									wordp[6] = v | 0x0800000000000000
									v = wordp[7] | 0x0000000000000010
									v |= 0x0000000000002000
									v |= 0x0000000000400000
									v |= 0x0000000080000000
									v |= 0x0000010000000000
									v |= 0x0002000000000000
									wordp[7] = v | 0x0400000000000000
									v = wordp[8] | 0x0000000000000008
									v |= 0x0000000000001000
									v |= 0x0000000000200000
									v |= 0x0000000040000000
									v |= 0x0000008000000000
									v |= 0x0001000000000000
									wordp[8] = v | 0x0200000000000000
								}
							}
						}
						11 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000040
									v |= 0x0000000000020000
									v |= 0x0000000010000000
									v |= 0x0000008000000000
									v |= 0x0004000000000000
									wordp[0] = v | 0x2000000000000000
									v = wordp[1] | 0x0000000000000100
									v |= 0x0000000000080000
									v |= 0x0000000040000000
									v |= 0x0000020000000000
									v |= 0x0010000000000000
									wordp[1] = v | 0x8000000000000000
									v = wordp[2] | 0x0000000000000400
									v |= 0x0000000000200000
									v |= 0x0000000100000000
									v |= 0x0000080000000000
									wordp[2] = v | 0x0040000000000000
									v = wordp[3] | 0x0000000000000002
									v |= 0x0000000000001000
									v |= 0x0000000000800000
									v |= 0x0000000400000000
									v |= 0x0000200000000000
									wordp[3] = v | 0x0100000000000000
									v = wordp[4] | 0x0000000000000008
									v |= 0x0000000000004000
									v |= 0x0000000002000000
									v |= 0x0000001000000000
									v |= 0x0000800000000000
									wordp[4] = v | 0x0400000000000000
									v = wordp[5] | 0x0000000000000020
									v |= 0x0000000000010000
									v |= 0x0000000008000000
									v |= 0x0000004000000000
									v |= 0x0002000000000000
									wordp[5] = v | 0x1000000000000000
									v = wordp[6] | 0x0000000000000080
									v |= 0x0000000000040000
									v |= 0x0000000020000000
									v |= 0x0000010000000000
									v |= 0x0008000000000000
									wordp[6] = v | 0x4000000000000000
									v = wordp[7] | 0x0000000000000200
									v |= 0x0000000000100000
									v |= 0x0000000080000000
									v |= 0x0000040000000000
									wordp[7] = v | 0x0020000000000000
									v = wordp[8] | 0x0000000000000001
									v |= 0x0000000000000800
									v |= 0x0000000000400000
									v |= 0x0000000200000000
									v |= 0x0000100000000000
									wordp[8] = v | 0x0080000000000000
									v = wordp[9] | 0x0000000000000004
									v |= 0x0000000000002000
									v |= 0x0000000001000000
									v |= 0x0000000800000000
									v |= 0x0000400000000000
									wordp[9] = v | 0x0200000000000000
									v = wordp[10] | 0x0000000000000010
									v |= 0x0000000000008000
									v |= 0x0000000004000000
									v |= 0x0000002000000000
									v |= 0x0001000000000000
									wordp[10] = v | 0x0800000000000000
								}
							}
						}
						13 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000080
									v |= 0x0000000000100000
									v |= 0x0000000200000000
									v |= 0x0000400000000000
									wordp[0] = v | 0x0800000000000000
									v = wordp[1] | 0x0000000000000100
									v |= 0x0000000000200000
									v |= 0x0000000400000000
									v |= 0x0000800000000000
									wordp[1] = v | 0x1000000000000000
									v = wordp[2] | 0x0000000000000200
									v |= 0x0000000000400000
									v |= 0x0000000800000000
									v |= 0x0001000000000000
									wordp[2] = v | 0x2000000000000000
									v = wordp[3] | 0x0000000000000400
									v |= 0x0000000000800000
									v |= 0x0000001000000000
									v |= 0x0002000000000000
									wordp[3] = v | 0x4000000000000000
									v = wordp[4] | 0x0000000000000800
									v |= 0x0000000001000000
									v |= 0x0000002000000000
									v |= 0x0004000000000000
									wordp[4] = v | 0x8000000000000000
									v = wordp[5] | 0x0000000000001000
									v |= 0x0000000002000000
									v |= 0x0000004000000000
									wordp[5] = v | 0x0008000000000000
									v = wordp[6] | 0x0000000000000001
									v |= 0x0000000000002000
									v |= 0x0000000004000000
									v |= 0x0000008000000000
									wordp[6] = v | 0x0010000000000000
									v = wordp[7] | 0x0000000000000002
									v |= 0x0000000000004000
									v |= 0x0000000008000000
									v |= 0x0000010000000000
									wordp[7] = v | 0x0020000000000000
									v = wordp[8] | 0x0000000000000004
									v |= 0x0000000000008000
									v |= 0x0000000010000000
									v |= 0x0000020000000000
									wordp[8] = v | 0x0040000000000000
									v = wordp[9] | 0x0000000000000008
									v |= 0x0000000000010000
									v |= 0x0000000020000000
									v |= 0x0000040000000000
									wordp[9] = v | 0x0080000000000000
									v = wordp[10] | 0x0000000000000010
									v |= 0x0000000000020000
									v |= 0x0000000040000000
									v |= 0x0000080000000000
									wordp[10] = v | 0x0100000000000000
									v = wordp[11] | 0x0000000000000020
									v |= 0x0000000000040000
									v |= 0x0000000080000000
									v |= 0x0000100000000000
									wordp[11] = v | 0x0200000000000000
									v = wordp[12] | 0x0000000000000040
									v |= 0x0000000000080000
									v |= 0x0000000100000000
									v |= 0x0000200000000000
									wordp[12] = v | 0x0400000000000000
								}
							}
						}
						15 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000002000
									v |= 0x0000000010000000
									v |= 0x0000080000000000
									wordp[0] = v | 0x0400000000000000
									v = wordp[1] | 0x0000000000000200
									v |= 0x0000000001000000
									v |= 0x0000008000000000
									wordp[1] = v | 0x0040000000000000
									v = wordp[2] | 0x0000000000000020
									v |= 0x0000000000100000
									v |= 0x0000000800000000
									wordp[2] = v | 0x0004000000000000
									v = wordp[3] | 0x0000000000000002
									v |= 0x0000000000010000
									v |= 0x0000000080000000
									v |= 0x0000400000000000
									wordp[3] = v | 0x2000000000000000
									v = wordp[4] | 0x0000000000001000
									v |= 0x0000000008000000
									v |= 0x0000040000000000
									wordp[4] = v | 0x0200000000000000
									v = wordp[5] | 0x0000000000000100
									v |= 0x0000000000800000
									v |= 0x0000004000000000
									wordp[5] = v | 0x0020000000000000
									v = wordp[6] | 0x0000000000000010
									v |= 0x0000000000080000
									v |= 0x0000000400000000
									wordp[6] = v | 0x0002000000000000
									v = wordp[7] | 0x0000000000000001
									v |= 0x0000000000008000
									v |= 0x0000000040000000
									v |= 0x0000200000000000
									wordp[7] = v | 0x1000000000000000
									v = wordp[8] | 0x0000000000000800
									v |= 0x0000000004000000
									v |= 0x0000020000000000
									wordp[8] = v | 0x0100000000000000
									v = wordp[9] | 0x0000000000000080
									v |= 0x0000000000400000
									v |= 0x0000002000000000
									wordp[9] = v | 0x0010000000000000
									v = wordp[10] | 0x0000000000000008
									v |= 0x0000000000040000
									v |= 0x0000000200000000
									v |= 0x0001000000000000
									wordp[10] = v | 0x8000000000000000
									v = wordp[11] | 0x0000000000004000
									v |= 0x0000000020000000
									v |= 0x0000100000000000
									wordp[11] = v | 0x0800000000000000
									v = wordp[12] | 0x0000000000000400
									v |= 0x0000000002000000
									v |= 0x0000010000000000
									wordp[12] = v | 0x0080000000000000
									v = wordp[13] | 0x0000000000000040
									v |= 0x0000000000200000
									v |= 0x0000001000000000
									wordp[13] = v | 0x0008000000000000
									v = wordp[14] | 0x0000000000000004
									v |= 0x0000000000020000
									v |= 0x0000000100000000
									v |= 0x0000800000000000
									wordp[14] = v | 0x4000000000000000
								}
							}
						}
						17 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000004
									v |= 0x0000000000080000
									v |= 0x0000001000000000
									wordp[0] = v | 0x0020000000000000
									v = wordp[1] | 0x0000000000000040
									v |= 0x0000000000800000
									v |= 0x0000010000000000
									wordp[1] = v | 0x0200000000000000
									v = wordp[2] | 0x0000000000000400
									v |= 0x0000000008000000
									v |= 0x0000100000000000
									wordp[2] = v | 0x2000000000000000
									v = wordp[3] | 0x0000000000004000
									v |= 0x0000000080000000
									wordp[3] = v | 0x0001000000000000
									v = wordp[4] | 0x0000000000000002
									v |= 0x0000000000040000
									v |= 0x0000000800000000
									wordp[4] = v | 0x0010000000000000
									v = wordp[5] | 0x0000000000000020
									v |= 0x0000000000400000
									v |= 0x0000008000000000
									wordp[5] = v | 0x0100000000000000
									v = wordp[6] | 0x0000000000000200
									v |= 0x0000000004000000
									v |= 0x0000080000000000
									wordp[6] = v | 0x1000000000000000
									v = wordp[7] | 0x0000000000002000
									v |= 0x0000000040000000
									wordp[7] = v | 0x0000800000000000
									v = wordp[8] | 0x0000000000000001
									v |= 0x0000000000020000
									v |= 0x0000000400000000
									wordp[8] = v | 0x0008000000000000
									v = wordp[9] | 0x0000000000000010
									v |= 0x0000000000200000
									v |= 0x0000004000000000
									wordp[9] = v | 0x0080000000000000
									v = wordp[10] | 0x0000000000000100
									v |= 0x0000000002000000
									v |= 0x0000040000000000
									wordp[10] = v | 0x0800000000000000
									v = wordp[11] | 0x0000000000001000
									v |= 0x0000000020000000
									v |= 0x0000400000000000
									wordp[11] = v | 0x8000000000000000
									v = wordp[12] | 0x0000000000010000
									v |= 0x0000000200000000
									wordp[12] = v | 0x0004000000000000
									v = wordp[13] | 0x0000000000000008
									v |= 0x0000000000100000
									v |= 0x0000002000000000
									wordp[13] = v | 0x0040000000000000
									v = wordp[14] | 0x0000000000000080
									v |= 0x0000000001000000
									v |= 0x0000020000000000
									wordp[14] = v | 0x0400000000000000
									v = wordp[15] | 0x0000000000000800
									v |= 0x0000000010000000
									v |= 0x0000200000000000
									wordp[15] = v | 0x4000000000000000
									v = wordp[16] | 0x0000000000008000
									v |= 0x0000000100000000
									wordp[16] = v | 0x0002000000000000
								}
							}
						}
						19 {
							for ; bytendx < bytelmt; bytendx += byteadv {
								unsafe {
									wordp := &u64(&sieve.sieve_buffer[bytendx])
									mut v := wordp[0] | 0x0000000000000040
									v |= 0x0000000002000000
									v |= 0x0000100000000000
									wordp[0] = v | 0x8000000000000000
									v = wordp[1] | 0x0000000000040000
									v |= 0x0000002000000000
									wordp[1] = v | 0x0100000000000000
									v = wordp[2] | 0x0000000000000800
									v |= 0x0000000040000000
									wordp[2] = v | 0x0002000000000000
									v = wordp[3] | 0x0000000000000010
									v |= 0x0000000000800000
									v |= 0x0000040000000000
									wordp[3] = v | 0x2000000000000000
									v = wordp[4] | 0x0000000000010000
									v |= 0x0000000800000000
									wordp[4] = v | 0x0040000000000000
									v = wordp[5] | 0x0000000000000200
									v |= 0x0000000010000000
									wordp[5] = v | 0x0000800000000000
									v = wordp[6] | 0x0000000000000004
									v |= 0x0000000000200000
									v |= 0x0000010000000000
									wordp[6] = v | 0x0800000000000000
									v = wordp[7] | 0x0000000000004000
									v |= 0x0000000200000000
									wordp[7] = v | 0x0010000000000000
									v = wordp[8] | 0x0000000000000080
									v |= 0x0000000004000000
									wordp[8] = v | 0x0000200000000000
									v = wordp[9] | 0x0000000000000001
									v |= 0x0000000000080000
									v |= 0x0000004000000000
									wordp[9] = v | 0x0200000000000000
									v = wordp[10] | 0x0000000000001000
									v |= 0x0000000080000000
									wordp[10] = v | 0x0004000000000000
									v = wordp[11] | 0x0000000000000020
									v |= 0x0000000001000000
									v |= 0x0000080000000000
									wordp[11] = v | 0x4000000000000000
									v = wordp[12] | 0x0000000000020000
									v |= 0x0000001000000000
									wordp[12] = v | 0x0080000000000000
									v = wordp[13] | 0x0000000000000400
									v |= 0x0000000020000000
									wordp[13] = v | 0x0001000000000000
									v = wordp[14] | 0x0000000000000008
									v |= 0x0000000000400000
									v |= 0x0000020000000000
									wordp[14] = v | 0x1000000000000000
									v = wordp[15] | 0x0000000000008000
									v |= 0x0000000400000000
									wordp[15] = v | 0x0020000000000000
									v = wordp[16] | 0x0000000000000100
									v |= 0x0000000008000000
									wordp[16] = v | 0x0000400000000000
									v = wordp[17] | 0x0000000000000002
									v |= 0x0000000000100000
									v |= 0x0000008000000000
									wordp[17] = v | 0x0400000000000000
									v = wordp[18] | 0x0000000000002000
									v |= 0x0000000100000000
									wordp[18] = v | 0x0008000000000000
								}
							}
						}
						else {} // never happens
					}
					swi |= bytendx << 3
					for ; swi <= bitlmt; swi += bp {
						sieve.sieve_buffer[swi >> 3] |= bitmask[swi & 7]
					}
				}
				else {
					mut bytendx := swi >> 3
					r1 := ((swi + bp) >> 3) - bytendx
					r2 := ((swi + 2 * bp) >> 3) - bytendx
					r3 := ((swi + 3 * bp) >> 3) - bytendx
					r4 := ((swi + 4 * bp) >> 3) - bytendx
					r5 := ((swi + 5 * bp) >> 3) - bytendx
					r6 := ((swi + 6 * bp) >> 3) - bytendx
					r7 := ((swi + 7 * bp) >> 3) - bytendx
					match (bp >> 1) & 3 {
						0 {
							for ; bytendx < size - r7; bytendx += bp {
								unsafe {
									cullp := &sieve.sieve_buffer[bytendx]
									cullp[0] |= u8(128)
									cullp[r1] |= u8(1)
									cullp[r2] |= u8(2)
									cullp[r3] |= u8(4)
									cullp[r4] |= u8(8)
									cullp[r5] |= u8(16)
									cullp[r6] |= u8(32)
									cullp[r7] |= u8(64)
								}
							}
						}
						1 {						
							for ; bytendx < size - r7; bytendx += bp {
								unsafe {
									cullp := &sieve.sieve_buffer[bytendx]
									cullp[0] |= u8(8)
									cullp[r1] |= u8(64)
									cullp[r2] |= u8(2)
									cullp[r3] |= u8(16)
									cullp[r4] |= u8(128)
									cullp[r5] |= u8(4)
									cullp[r6] |= u8(32)
									cullp[r7] |= u8(1)
								}
							}
						}
						2 {						
							for ; bytendx < size - r7; bytendx += bp {
								unsafe {
									cullp := &sieve.sieve_buffer[bytendx]
									cullp[0] |= u8(8)
									cullp[r1] |= u8(1)
									cullp[r2] |= u8(32)
									cullp[r3] |= u8(4)
									cullp[r4] |= u8(128)
									cullp[r5] |= u8(16)
									cullp[r6] |= u8(2)
									cullp[r7] |= u8(64)
								}
							}
						}
						3 {						
							for ; bytendx < size - r7; bytendx += bp {
								unsafe {
									cullp := &sieve.sieve_buffer[bytendx]
									cullp[0] |= u8(128)
									cullp[r1] |= u8(64)
									cullp[r2] |= u8(32)
									cullp[r3] |= u8(16)
									cullp[r4] |= u8(8)
									cullp[r5] |= u8(4)
									cullp[r6] |= u8(2)
									cullp[r7] |= u8(1)
								}
							}
						}
						else {}
					}
					for ndx := (bytendx << 3) + (swi & 7); ndx < bitlmt; ndx += bp {
						sieve.sieve_buffer[ndx >> 3] |= bitmask[ndx & 7]
					}
				}
			}
		}
	}

	return sieve
}

fn (sieve PrimeSieve) count_primes() int {
	if sieve.sieve_size < 3 {
		if sieve.sieve_size < 2 { return 0 }
		return 1
	}

	mut count := 1
	bitlmt := int((sieve.sieve_size - 3) >> 1)
	for ndx := 0; ndx <= bitlmt; ndx++ {
		if (sieve.sieve_buffer[ndx >> 3] & bitmask[ndx & 7]) == u8(0) {
			count++
		}
	}

	return count
}

fn bench(tec Technique) {
	mut passes := 0
	start_time := time.now()
	for {
		sieve := new_prime_sieve(limit, tec)
		passes++
		duration := (time.now() - start_time).seconds()
		if duration >= 5.0 {
			mut rsltstr := "2 "
			mut count := 1
			bitlmt := int((sieve.sieve_size - 3) >> 1)
			for ndx := 0; ndx <= bitlmt; ndx++ {
				if (sieve.sieve_buffer[ndx >> 3] & bitmask[ndx & 7]) == u8(0) {
					if count < 25 { rsltstr += (ndx + ndx + 3).str() + " " }
					count++
				}
			}

			avg := duration / f64(passes)
			count_primes := sieve.count_primes()
			valid := rsltstr == "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 "
			           && count_primes == result && count == result
			eprintln('Passes: $passes, Time: $duration, Avg: $avg, Limit: $sieve.sieve_size, Count1: $count, Count2: $count_primes, Valid: $valid')
			label := 'GordonBGood_' + match tec {
				.bit_twiddle { 'bittwiddle' }
				.stride8 { 'stride8' }
				.stride8_block { 'stride8-block16K' }
				.extreme { 'extreme' }
				.extreme_hybrid { 'extreme-hybrid' }
			}
			println('$label;$passes;$duration;1;algorithm=base,faithful=yes,bits=1')
			break
		}
	}
}

fn main() {
	bench(Technique.bit_twiddle)
	bench(Technique.stride8)
	bench(Technique.stride8_block)
	bench(Technique.extreme)
	bench(Technique.extreme_hybrid)
}

