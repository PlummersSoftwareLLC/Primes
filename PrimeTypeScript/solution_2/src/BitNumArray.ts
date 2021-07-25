import { IBitArray } from "./IBitArray";

export class BitNumArray implements IBitArray {
  private readonly bits: number[] = [];
  
  constructor(
    public readonly size: number
  ) {
    this.bits = [];
  }

  public readonly name = 'number-array';

  public get(i: number) {
    return this.bits[i] || 0;
  }

  public setTrue(i: number) {
    this.bits[i] = 1;
  }

  public get bitsPerFlag() {
    return undefined;
  }
}