import { IBitArray } from "./IBitArray";

export class BitByteArray implements IBitArray {
  private readonly bits: Uint8Array;
  
  constructor(
    public readonly size: number
  ) {
    this.bits = new Uint8Array(size);
  }

  public readonly name = 'byte-array';

  public get(i: number) {
    return this.bits[i] || 0;
  }

  public setTrue(i: number) {
    this.bits[i] = 1;
  }

  public get bitsPerFlag() {
    return 8;
  }
}