import { IBitArray } from "./IBitArray";

export class Bit32Array implements IBitArray {
  private readonly bits: Uint32Array;
  
  constructor(
    public readonly size: number
  ) {
    this.bits = new Uint32Array((size / 32) + 1);
  }
  public readonly name = '32bit-array';

  public get(i: number) {
    // const ixByte = ~~(i / 8); // Same as math.truc, but fast
    const ixByte = i >> 5; // Integer divide by 32
    const ixBit = i % 32;
    const byte = this.bits[ixByte];
    return (byte & (1 << ixBit)) ? 1 : 0;
  }

  public setTrue(i: number) {
    // const ixByte = ~~(i / 8);
    const ixByte = i >> 5; // Integer divide by 32
    const ixBit = i % 32;
    this.bits[ixByte] |= (1 << ixBit);
  }

  public get bitsPerFlag() {
    return 1;
  }
}