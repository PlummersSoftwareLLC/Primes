alias Prime = UInt64

RANGE = 1_000_000_u64.as Prime

FORTO = 5_f64

CPUL1CACHE = 16384_i32

DICT = {
           10_u64 => 4,
          100_u64 => 25,
         1000_u64 => 168,
        10000_u64 => 1229,
       100000_u64 => 9592,
      1000000_u64 => 78498,
     10000000_u64 => 664579,
    100000000_u64 => 5761455,
   1000000000_u64 => 50847534,
  10000000000_u64 => 455052511,
}

EXPECTED = DICT[RANGE]

enum Techniques
  BitTwiddle
  Stride8
  Stride8Block16K
  Extreme
  ExtremeHybrid
end

BITMASKP = Pointer.malloc(8) { |i| 1_u8 << i }

macro unroll_setbits(bitarrp, starti, limiti, stepi)
  ndx: Int32 = {{starti}} & 7
  r0 = {{starti}} >> 3
  r1 = {{starti}} + {{stepi}}
  r2 = r1 + {{stepi}}
  r3 = r2 + {{stepi}}
  r4 = r3 + {{stepi}}
  r5 = r4 + {{stepi}}
  r6 = r5 + {{stepi}}
  r7 = ((r6 + {{stepi}}) >> 3) - r0
  r6 = (r6 >> 3) - r0
  r5 = (r5 >> 3) - r0
  r4 = (r4 >> 3) - r0
  r3 = (r3 >> 3) - r0
  r2 = (r2 >> 3) - r0
  r1 = (r1 >> 3) - r0
  bytep: Pointer(UInt8) = {{bitarrp}} + r0
  looplmtp: Pointer(UInt8) = {{bitarrp}} + (({{limiti}} >> 3) - r7)
  case ((({{stepi}} & 7) << 3) | ({{starti}} & 7)).to_u8
  {% for n in (0_u8..0x3F) %}
    when {{n}}
      while bytep <= looplmtp
        bytep[0] |= {{1_u8 << (n & 7)}}
        bytep[r1] |= {{1_u8 << (((n >> 3) + n) & 7)}}
        bytep[r2] |= {{1_u8 << ((2 * (n >> 3) + n) & 7)}}
        bytep[r3] |= {{1_u8 << ((3 * (n >> 3) + n) & 7)}}
        bytep[r4] |= {{1_u8 << ((4 * (n >> 3) + n) & 7)}}
        bytep[r5] |= {{1_u8 << ((5 * (n >> 3) + n) & 7)}}
        bytep[r6] |= {{1_u8 << ((6 * (n >> 3) + n) & 7)}}
        bytep[r7] |= {{1_u8 << ((7 * (n >> 3) + n) & 7)}}
        bytep += {{stepi}}
      end
  {% end %}
  else
  end
  ndx += (bytep - {{bitarrp}}) << 3
  while ndx <= {{limiti}}
    {{bitarrp}}[ndx >> 3] |= BITMASKP[ndx & 7]; ndx += {{stepi}}
  end
end

# Look Up Table of all start indices for the above step values...
# calculated as follows for each step value in STEPS:
#  swi = (ndx + ndx) * (ndx + 3) + 3
#  r = ((swi | 63) + 1 - swi) % (ndx + ndx + 3)
#  starti = if r == 0 then 0 else ndx + ndx + 3 - r
STARTIS = [ 2, 2, 1, 2, 6, 7, 13, 2, 6, 5, 12, 16, 6, 0, 29, 0,
            6, 16, 30, 25, 6, 32, 45, 32, 6, 48, 30, 16, 6, 0, 62 ]

macro dense_setbits(bitarrp, starti, limiti, stepi)
  dndx = {{starti}}
  dndxlmt = {{starti}} | 63
  while dndx <= dndxlmt # cull to an even 64-bit boundary...
    {{bitarrp}}[dndx >> 3] |= BITMASKP[dndx & 7]; dndx += {{stepi}}
  end
  wordp: Pointer(UInt64) = ({{bitarrp}} + ((dndx >> 3) & (-8))).as(Pointer(UInt64))
  keep = wordp
  wordlmtp: Pointer(UInt64) = ({{bitarrp}} + ((({{limiti}} >> 3) & (-8)) -
                                  (({{stepi}} << 3) - 8))).as(Pointer(UInt64))
  dndx &= 63
  case {{stepi}}.to_u8
    {% for stpvi in (0...STARTIS.size) %} # odd primes STARTIS.size
      when {{stpvi + stpvi + 3}}.to_u8
        while wordp <= wordlmtp
          # for all modulo pattern 64-bit words
          {% for wi in (0 ... (stpvi + stpvi + 3)) %}
            # for all modulo pattern 64-bit words
            {% for bi in (((wi * 64 - 1 - STARTIS[stpvi]) / (stpvi + stpvi + 3) + 1) .. ((wi * 64 + 63 - STARTIS[stpvi]) / (stpvi + stpvi + 3))) %}
              {% if (STARTIS[stpvi] + (bi - 1) * (stpvi + stpvi + 3)) < wi * 64 && (STARTIS[stpvi] + (bi + 1) * (stpvi + stpvi + 3)) >= (wi + 1) * 64  %} # only one bit
                wordp[{{wi}}] |= {{1_u64 << ((STARTIS[stpvi] + bi * (stpvi + stpvi + 3)) & 63)}}
              {% elsif (STARTIS[stpvi] + (bi - 1) * (stpvi + stpvi + 3)) < wi * 64 %} # first bit of many in word
                v = wordp[{{wi}}] | {{1_u64 << ((STARTIS[stpvi] + bi * (stpvi + stpvi + 3)) & 63)}}
              {% elsif (STARTIS[stpvi] + (bi + 1) * (stpvi + stpvi + 3)) >= (wi + 1) * 64 %} # last bit of many in word
                wordp[{{wi}}] = v | {{1_u64 << ((STARTIS[stpvi] + bi * (stpvi + stpvi + 3)) & 63)}}
              {% else %} # not the first nor the last bit in the word
                v |= {{1_u64 << ((STARTIS[stpvi] + bi * (stpvi + stpvi + 3)) & 63)}}
              {% end %} 
            {% end %}
          {% end %}
          wordp += {{stpvi + stpvi + 3}}
        end
    {% end %}
    else
  end
  dndx |= (wordp.as(Pointer(UInt8)) - {{bitarrp}}) << 3
  while dndx <= {{limiti}}
    {{bitarrp}}[dndx >> 3] |= BITMASKP[dndx & 7]; dndx += {{stepi}}
  end
end

class PrimeSieve
  def initialize(range : Prime, tec : Techniques)
    @range = range
    bits = ((range - 3) >> 1).to_i32
    bytesize = ((bits + 64) >> 3) & (-8) # round up to even 64-bit bytes
    @bufp = Pointer.malloc(bytesize, 0_u8)
    bytendxlmtp = @bufp + (bytesize - 1)
    case tec
    in Techniques::BitTwiddle
      bap = @bufp
      (0..).each do |i|
        swi = (i + i) * (i + 3) + 3 # calculate start marking index
        break if swi > bits
        next if (@bufp[i >> 3] & BITMASKP[i & 7]) != 0
        bp = i + i + 3
        while swi <= bits
          bap[swi >> 3] |= BITMASKP[swi & 7]; swi += bp
        end
      end

    in Techniques::Stride8
      (0..).each do |i|
        swi = (i + i) * (i + 3) + 3 # calculate start marking index
        break if swi > bits
        next if (@bufp[i >> 3] & BITMASKP[i & 7]) != 0
        bp = i + i + 3
        swilmt = swi + (bp << 3) - 1
        swilmt = bits if swilmt > bits
        while swi <= swilmt
          mask = BITMASKP[swi & 7]
          bytendxp = @bufp + (swi >> 3)
          while bytendxp <= bytendxlmtp
            bytendxp[0] |= mask; bytendxp += bp
          end
          swi += bp
        end
      end

    in Techniques::Stride8Block16K
      strtsp = Pointer.malloc(8, nil.as Pointer(UInt8))
      (0..).each do |i|
        swi = (i + i) * (i + 3) + 3 # calculate start marking index
        break if swi > bits
        next if (@bufp[i >> 3] & BITMASKP[i & 7]) != 0
        bp = i + i + 3; bp2 = bp + bp; bp3 = bp + bp2; bp4 = bp + bp3
        pagebytendx = (swi >> 3) & (-CPUL1CACHE)
        (0..7).each { |_| strtsp[swi & 7] = @bufp + (swi >> 3); swi += bp }
        while pagebytendx < bytesize
          blocklmtp = @bufp + (pagebytendx + CPUL1CACHE - 1)
          blocklmtp = bytendxlmtp if blocklmtp > bytendxlmtp
          blockstopp = blocklmtp - bp3
          8.times do |si|
            mask = BITMASKP[si]; bytendxp = strtsp[si]
            while bytendxp <= blockstopp
              bytendxp[0] |= mask; bytendxp[bp] |= mask
              bytendxp[bp2] |= mask; bytendxp[bp3] |= mask ; bytendxp += bp4
            end
            while bytendxp <= blocklmtp
              bytendxp[0] |= mask ; bytendxp += bp
            end
            strtsp[si] = bytendxp
          end
          pagebytendx += CPUL1CACHE
        end
      end

    in Techniques::Extreme
      (0..).each do |i|
        swi = (i + i) * (i + 3) + 3 # calculate start marking index
        break if swi > bits
        next if (@bufp[i >> 3] & BITMASKP[i & 7]) != 0
        bp = i + i + 3
        unroll_setbits(@bufp, swi, bits, bp)
      end
    in Techniques::ExtremeHybrid
      (0..).each do |i|
        swi = (i + i) * (i + 3) + 3 # calculate start marking index
        break if swi > bits
        next if (@bufp[i >> 3] & BITMASKP[i & 7]) != 0
        bp = i + i + 3
        if bp <= 63
          dense_setbits(@bufp, swi, bits, bp)
        else
          unroll_setbits(@bufp, swi, bits, bp)
        end
      end
    end
 end

  def count_primes
    if @range < 3
      return 0 if @range < 2
      return 1
    end
    bits = ((@range - 3) >> 1).to_i32
    ((0..bits).each.select do |i|
      (@bufp[i >> 3] & BITMASKP[i & 7]) == 0
    end).size + 1
  end
end

def bench(tec : Techniques)
  passes = 0
  duration = 0_f64
  start_time = Time.monotonic
  loop do
    sieve = PrimeSieve.new(RANGE, tec)
    passes += 1
    duration = (Time.monotonic - start_time).total_seconds
    label = "GordonBGood_"
    case tec
    in Techniques::BitTwiddle
      label += "bittwiddle"
    in Techniques::Stride8
      label += "stride8"
    in Techniques::Stride8Block16K
      label += "stride8-rblock16K"
    in Techniques::Extreme
      label += "extreme"
    in Techniques::ExtremeHybrid
      label += "extreme-hybrid"
    end
    if duration >= FORTO
      prime_count = sieve.count_primes
      count =  sieve.@range < 2 ?  0 : 1
      (0 .. ((sieve.@range - 3) >> 1).to_i32).each do |i|
        count += 1 if (sieve.@bufp[i >> 3] & BITMASKP[i & 7]) == 0
      end
      valid = count == EXPECTED && prime_count == EXPECTED
      if valid
        printf("%s;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", label, passes, duration)
      else
        printf("Invalid result!!!:  ")
      end
      STDERR.printf("Passes: %d Time: %f Avg: %f Limit: %d Count1: %d Count2: %d Valid: %s\n",
                      passes, duration, (duration / passes),
                      sieve.@range, count, prime_count, valid)
      break
    end
  end
end

{% if flag? :expand_macro  %} # only one bit
  bap = Pointer.malloc(16384, 0_u8)
  bp = 3
  swi = (bp * bp - 3) >> 3
  lmti = 131071
  unroll_setbits(bap, swi, lmti, bp)
  dense_setbits(bap, swi, lmti, bp)
{% else %}
  Techniques.each do |t| bench(t) end
{% end %}

