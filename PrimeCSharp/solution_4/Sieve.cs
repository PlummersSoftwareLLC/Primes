using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

namespace FastSieve
{
	public static unsafe class Sieve
	{
		[MethodImpl(MethodImplOptions.AggressiveOptimization)] //not sure if this does anything for this method or not
		public static nint CalculateSieve(nint max)
		{
			if (max < 2) return 0;
			if (max == 2) return 1;

			bool[] notPrime = new bool[max + 1];

			nint factor = 3;
			nint q = (nint)Math.Sqrt(max);

			while (factor <= q)
			{
				for (nint num = factor; num <= max; num += 2)
				{
					if (!notPrime[num])
					{
						factor = num;
						break;
					}
				}
				nint factor2 = factor << 1;
				for (nint num = factor * factor; num <= max; num += factor2)
				{
					notPrime[num] = true;
				}

				factor += 2;
			}

			nint primeCount = 1;
			for (nint i = 3; i <= max; i += 2)
			{
				if (!notPrime[i]) primeCount++;
			}

			return primeCount;
		}

		[MethodImpl(MethodImplOptions.AggressiveOptimization)] //not sure if this does anything for this method or not
		public static nint CalculateSieveOptimized(nint max, bool vectorize)
		{
			if (max <= 2) return max == 2 ? 1 : 0;

			var halfMax = (max - 1) >> 1;
			bool[] notPrimeEvery2nd = new bool[halfMax]; //index 0 = 3, index 1 = 5, etc.   |    represented as 0 or 1 in memory

			nint factorIndex = 0; //equal to (factor - 3) / 2
			nint q;
			if (max <= 16_777_216)
			{
				q = (nint)MathF.Sqrt(max);
			}
			else //maximum is 9_007_199_254_740_992
			{
				q = (nint)Math.Sqrt(max);
			}
			nint qIndex = (q - 3) >> 1;

			while (factorIndex <= qIndex)
			{
				for (; factorIndex <= qIndex; factorIndex++)
				{
					if (!notPrimeEvery2nd[factorIndex]) break;
				}
				nint addition = (factorIndex << 1) + 3;
				for (nint num = (factorIndex * (factorIndex + 3) << 1) + 3; num < halfMax; num += addition)
				{
					notPrimeEvery2nd[num] = true;
				}

				factorIndex++;
			}

			nint primeCount = 1;
			if (!vectorize) //same as 1 value at a time (fallback)
			{
				for (nint i = 0; i < halfMax; i++)
				{
					if (!notPrimeEvery2nd[i]) primeCount++;
				}
			}
			else
			{
				if (Popcnt.X64.IsSupported) //8 values at a time (x64 + intrinsics)
				{
					fixed (bool* ptr = notPrimeEvery2nd)
					{
						var loops = halfMax >> 3;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 8 - (nint)Popcnt.X64.PopCount(((ulong*)ptr)[i]);
						}
						for (nint i = loops << 3; i < halfMax; i++)
						{
							if (!notPrimeEvery2nd[i]) primeCount++;
						}
					}
				}
				else if (Popcnt.IsSupported) //4 values at a time (x86 + intrinsics)
				{
					fixed (bool* ptr = notPrimeEvery2nd)
					{
						var loops = halfMax >> 2;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 4 - (nint)Popcnt.PopCount(((uint*)ptr)[i]);
						}
						for (nint i = loops << 2; i < halfMax; i++)
						{
							if (!notPrimeEvery2nd[i]) primeCount++;
						}
					}
				}
				else //1 value at a time (fallback)
				{
					for (nint i = 0; i < halfMax; i++)
					{
						if (!notPrimeEvery2nd[i]) primeCount++;
					}
				}
			}
			return primeCount;
		}

		[MethodImpl(MethodImplOptions.AggressiveOptimization)] //not sure if this does anything for this method or not
		public static nint CalculateSieveOptimizedUnmanaged(nint max, bool vectorize)
		{
			if (max <= 2) return max == 2 ? 1 : 0;

			var halfMax = (max - 1) >> 1;
			bool* notPrimeEvery2nd = (bool*)Marshal.AllocHGlobal(halfMax); //index 0 = 3, index 1 = 5, etc.   |    represented as 0 or 1 in memory
			{
				if (vectorize)
				{
					var loops = halfMax >> 5;
					for (nint i = 0; i < loops; i++)
					{
						((Vector256<byte>*)notPrimeEvery2nd)[i] = default;
					}
					Unsafe.InitBlockUnaligned(notPrimeEvery2nd + (loops << 5), 0, (uint)(halfMax - (loops << 5)));
				}
				else
				{
					for (nint i = 0; i < halfMax; i++)
					{
						notPrimeEvery2nd[i] = default;
					}
				}
			}

			nint factorIndex = 0; //equal to (factor - 3) / 2
			nint q;
			if (max <= 16_777_216)
			{
				q = (nint)MathF.Sqrt(max);
			}
			else //maximum is 9_007_199_254_740_992
			{
				q = (nint)Math.Sqrt(max);
			}
			nint qIndex = (q - 3) >> 1;

			while (factorIndex <= qIndex)
			{
				for (; factorIndex <= qIndex; factorIndex++)
				{
					if (!notPrimeEvery2nd[factorIndex]) break;
				}

				nint addition = (factorIndex << 1) + 3;
				for (nint num = (factorIndex * (factorIndex + 3) << 1) + 3; num < halfMax; num += addition)
				{
					notPrimeEvery2nd[num] = true;
				}
				
				factorIndex++;
			}

			nint primeCount = 1;
			if (!vectorize) //same as 1 value at a time (fallback)
			{
				for (nint i = 0; i < halfMax; i++)
				{
					if (!notPrimeEvery2nd[i]) primeCount++;
				}
			}
			else
			{
				if (Popcnt.X64.IsSupported) //8 values at a time (x64 + intrinsics)
				{
					var ptr = notPrimeEvery2nd;
					{
						var loops = halfMax >> 3;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 8 - (nint)Popcnt.X64.PopCount(((ulong*)ptr)[i]);
						}
						for (nint i = loops << 3; i < halfMax; i++)
						{
							if (!notPrimeEvery2nd[i]) primeCount++;
						}
					}
				}
				else if (Popcnt.IsSupported) //4 values at a time (x86 + intrinsics)
				{
					var ptr = notPrimeEvery2nd;
					{
						var loops = halfMax >> 2;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 4 - (nint)Popcnt.PopCount(((uint*)ptr)[i]);
						}
						for (nint i = loops << 2; i < halfMax; i++)
						{
							if (!notPrimeEvery2nd[i]) primeCount++;
						}
					}
				}
				else //1 value at a time (fallback)
				{
					for (nint i = 0; i < halfMax; i++)
					{
						if (!notPrimeEvery2nd[i]) primeCount++;
					}
				}
			}
			Marshal.FreeHGlobal((IntPtr)notPrimeEvery2nd);
			return primeCount;
		}

		[MethodImpl(MethodImplOptions.AggressiveOptimization)] //not sure if this does anything for this method or not
		public static nint CalculateSieveOptimizedUnmanaged1Bit(nint max, bool vectorize)
		{
			if (max <= 2) return max == 2 ? 1 : 0;

			var halfMax = (max - 1) >> 1;
			var bytesMax = (halfMax + 7) >> 3;
			byte* notPrimeEvery2nd = (byte*)Marshal.AllocHGlobal(bytesMax); //packed bits that represent the primes, starting in index 0: (low to high bit is 3, 5, 7, 9, 11, 13, 15, 17), etc.
			{
				if (vectorize)
				{
					var loops = bytesMax >> 5;
					for (nint i = 0; i < loops; i++)
					{
						((Vector256<byte>*)notPrimeEvery2nd)[i] = default;
					}
					Unsafe.InitBlockUnaligned(notPrimeEvery2nd + (loops << 5), 0, (uint)(bytesMax - (loops << 5)));
				}
				else
				{
					for (nint i = 0; i < bytesMax; i++)
					{
						notPrimeEvery2nd[i] = default;
					}
				}
			}

			nint factorIndex = 0; //equal to (factor - 3) / 2
			nint q;
			if (max <= 16_777_216)
			{
				q = (nint)MathF.Sqrt(max);
			}
			else //maximum is 9_007_199_254_740_992
			{
				q = (nint)Math.Sqrt(max);
			}
			nint qIndex = (q - 3) >> 1;
			nint qIndexHi = qIndex >> 3;
			int qIndexLo = (int)(qIndex & 7);

			while (factorIndex <= qIndex)
			{
				nint factorIndexHi = factorIndex >> 3;
				if (factorIndexHi == qIndexHi) goto skipToHighCheck;

				{
					var factorIndexLo = factorIndex & 7;

					var bitfield = notPrimeEvery2nd[factorIndexHi];
					var newFactorIndexLo = NextPrime[bitfield | (factorIndexLo << 8)];

					if (newFactorIndexLo != -1)
					{
						factorIndex = (factorIndexHi << 3) | newFactorIndexLo;
						goto fullBreak;
					}

					factorIndexHi++;
				}
				for (; factorIndexHi < qIndexHi; factorIndexHi++)
				{
					var bitfield = notPrimeEvery2nd[factorIndexHi];
					var newFactorIndexLo = NextPrime[bitfield];

					if (newFactorIndexLo != -1)
					{
						factorIndex = (factorIndexHi << 3) | newFactorIndexLo;
						goto fullBreak;
					}
				}
				factorIndex = factorIndex << 3;
				skipToHighCheck:
				//if (factorIndexHi == qIndexHi)
				{
					var factorIndexLo = factorIndex & 7;

					var bitfield = notPrimeEvery2nd[factorIndexHi];
					var newFactorIndexLo = NextPrime[bitfield | (factorIndexLo << 8)];

					if (newFactorIndexLo != -1 && newFactorIndexLo <= qIndexLo)
					{
						factorIndex = (factorIndexHi << 3) | newFactorIndexLo;
						goto fullBreak;
					}
				}
				break;
				fullBreak:

				nint addition = (factorIndex << 1) + 3;
				for (nint num = (factorIndex * (factorIndex + 3) << 1) + 3; num < halfMax; num += addition)
				{
					var idx = num >> 3;
					var idx2 = num & 7;
					notPrimeEvery2nd[idx] |= (byte)(1u << (int)idx2);
				}

				factorIndex++;
			}

			nint primeCount = 1;
			if (!vectorize) //same as 1 value at a time (fallback)
			{
				for (nint i = 0; i < bytesMax; i++)
				{
					primeCount += PopcountByte[notPrimeEvery2nd[i]];
				}
			}
			else
			{
				if (Popcnt.X64.IsSupported) //8 values at a time (x64 + intrinsics)
				{
					var ptr = notPrimeEvery2nd;
					{
						var loops = bytesMax >> 3;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 64 - (nint)Popcnt.X64.PopCount(((ulong*)ptr)[i]);
						}
						for (nint i = loops << 3; i < bytesMax; i++)
						{
							primeCount += PopcountByte[notPrimeEvery2nd[i]];
						}
					}
				}
				else if (Popcnt.IsSupported) //4 values at a time (x86 + intrinsics)
				{
					var ptr = notPrimeEvery2nd;
					{
						var loops = bytesMax >> 2;
						for (nint i = 0; i < loops; i++)
						{
							primeCount += 32 - (nint)Popcnt.PopCount(((uint*)ptr)[i]);
						}
						for (nint i = loops << 2; i < bytesMax; i++)
						{
							primeCount += PopcountByte[notPrimeEvery2nd[i]];
						}
					}
				}
				else //1 value at a time (fallback)
				{
					for (nint i = 0; i < bytesMax; i++)
					{
						primeCount += PopcountByte[notPrimeEvery2nd[i]];
					}
				}
			}
			Marshal.FreeHGlobal((IntPtr)notPrimeEvery2nd);
			return primeCount - ((bytesMax << 3) - halfMax);
		}

		//returns the popcount
		private static readonly byte* PopcountByte = (byte*)Marshal.AllocHGlobal(256);

		public static void InitializePopcountByte()
		{
			for (int i = 0; i < 256; i++)
			{
				int value = 0;
				if ((i & 1) == 0) value++;
				if ((i & 2) == 0) value++;
				if ((i & 4) == 0) value++;
				if ((i & 8) == 0) value++;
				if ((i & 16) == 0) value++;
				if ((i & 32) == 0) value++;
				if ((i & 64) == 0) value++;
				if ((i & 128) == 0) value++;
				PopcountByte[i] = (byte)value;
			}
		}

		//returns the index of the next prime in bitfield (x) starting at a certain index (y), index = (x | (y << 8))
		private static readonly int* NextPrime = (int*)Marshal.AllocHGlobal(8192);

		public static void InitializeNextPrime()
		{
			for (int start = 7; start >= 0; start--)
			{
				for (int i = 0; i < 256; i++)
				{
					int idx = i | (start << 8);
					if (start <= 0)
					{
						if ((i & 1) == 0)
						{
							NextPrime[idx] = 0;
							continue;
						}
					}
					if (start <= 1)
					{
						if ((i & 2) == 0)
						{
							NextPrime[idx] = 1;
							continue;
						}
					}
					if (start <= 2)
					{
						if ((i & 4) == 0)
						{
							NextPrime[idx] = 2;
							continue;
						}
					}
					if (start <= 3)
					{
						if ((i & 8) == 0)
						{
							NextPrime[idx] = 3;
							continue;
						}
					}
					if (start <= 4)
					{
						if ((i & 16) == 0)
						{
							NextPrime[idx] = 4;
							continue;
						}
					}
					if (start <= 5)
					{
						if ((i & 32) == 0)
						{
							NextPrime[idx] = 5;
							continue;
						}
					}
					if (start <= 6)
					{
						if ((i & 64) == 0)
						{
							NextPrime[idx] = 6;
							continue;
						}
					}
					if (start <= 7)
					{
						if ((i & 128) == 0)
						{
							NextPrime[idx] = 7;
							continue;
						}
					}
					NextPrime[idx] = -1;
				}
			}
		}
	}
}
