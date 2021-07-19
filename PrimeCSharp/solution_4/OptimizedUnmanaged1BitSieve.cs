using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

namespace FastSieve
{
	public unsafe class OptimizedUnmanaged1BitSieve : Sieve
	{
		protected byte* notPrimeEvery2nd;
		protected nint halfMax;
		protected nint bytesMax;

		protected override void InitializeImpl()
		{
			if (max > 2)
			{
				halfMax = (max - 1) >> 1;
				bytesMax = (halfMax + 7) >> 3;
				notPrimeEvery2nd = (byte*)Marshal.AllocHGlobal(bytesMax); //packed bits that represent the primes, starting in index 0: (low to high bit is 3, 5, 7, 9, 11, 13, 15, 17), etc.
				{
					for (nint i = 0; i < bytesMax; i++)
					{
						notPrimeEvery2nd[i] = default;
					}
				}
			}
		}

		protected override object RunSieveImpl()
		{
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

			return (IntPtr)notPrimeEvery2nd;
		}

		protected override nint CountPrimesImpl()
		{
			nint primeCount = 1;

			//same as 1 value at a time (fallback)
			for (nint i = 0; i < bytesMax; i++)
			{
				primeCount += PopcountByte[notPrimeEvery2nd[i]];
			}

			return primeCount - ((bytesMax << 3) - halfMax);
		}

		protected override void DisposeImpl()
		{
			if (notPrimeEvery2nd != null) Marshal.FreeHGlobal((IntPtr)notPrimeEvery2nd);
		}

		public override string CreateMessage(nint passes, double seconds)
		{
			return $"hamarb123_optimized,unmanaged,onebit;{ passes };{ seconds };1;algorithm=base,faithful=yes,bits=1";
		}

		public sealed class Vectorized : OptimizedUnmanaged1BitSieve
		{
			protected override void InitializeImpl()
			{
				if (max > 2)
				{
					halfMax = (max - 1) >> 1;
					bytesMax = (halfMax + 7) >> 3;
					notPrimeEvery2nd = (byte*)Marshal.AllocHGlobal(bytesMax); //packed bits that represent the primes, starting in index 0: (low to high bit is 3, 5, 7, 9, 11, 13, 15, 17), etc.
					{
						var loops = bytesMax >> 5;
						for (nint i = 0; i < loops; i++)
						{
							((Vector256<byte>*)notPrimeEvery2nd)[i] = default;
						}
						Unsafe.InitBlockUnaligned(notPrimeEvery2nd + (loops << 5), 0, (uint)(bytesMax - (loops << 5)));
					}
				}
			}

			protected override nint CountPrimesImpl()
			{
				nint primeCount = 1;

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

				return primeCount - ((bytesMax << 3) - halfMax);
			}

			public override string CreateMessage(nint passes, double seconds)
			{
				return $"hamarb123_optimized,unmanaged,vectorized,onebit;{ passes };{ seconds };1;algorithm=base,faithful=no,bits=1";
			}
		}

		//Static (essentially compile-time) constants:

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
