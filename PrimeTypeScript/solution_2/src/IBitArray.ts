export interface IBitArray {
  name: string;
  size: number
  get(i: number): number;
  setTrue(i: number): void;
  bitsPerFlag: number | undefined;
}