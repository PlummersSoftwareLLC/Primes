import { IBitArray } from "./IBitArray";

/**
 * This is equivalent to Solution_1
 * Solution_1 is classified wrong in there code, since it is 8 bits / flag and not 1 bit / flag
 */
export class Solution1ByteBuffer implements IBitArray {
  private readonly bits: Buffer;
  
  constructor(
    public readonly size: number
  ) {
    this.bits = Buffer.alloc(size);
  }

  public readonly name = 'Solution_1';

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