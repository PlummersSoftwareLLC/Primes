using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

namespace FastSieve
{
	public unsafe class OptimizedUnmanagedSieve : Sieve
	{
		protected bool* notPrimeEvery2nd;
		protected nint halfMax;

		protected override void InitializeImpl()
		{
			if (max > 2)
			{
				halfMax = (max - 1) >> 1;
				notPrimeEvery2nd = (bool*)Marshal.AllocHGlobal(halfMax); //index 0 = 3, index 1 = 5, etc.   |    represented as 0 or 1 in memory
				{
					for (nint i = 0; i < halfMax; i++)
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

			return (IntPtr)notPrimeEvery2nd;
		}

		protected override nint CountPrimesImpl()
		{
			nint primeCount = 1;

			//same as 1 value at a time (fallback)
			for (nint i = 0; i < halfMax; i++)
			{
				if (!notPrimeEvery2nd[i]) primeCount++;
			}

			return primeCount;
		}

		protected override void DisposeImpl()
		{
			if (notPrimeEvery2nd != null) Marshal.FreeHGlobal((IntPtr)notPrimeEvery2nd);
		}

		public override string CreateMessage(nint passes, double seconds)
		{
			return $"hamarb123_optimized,unmanaged;{ passes };{ seconds };1;algorithm=base,faithful=yes,bits=8";
		}

		public sealed class Vectorized : OptimizedUnmanagedSieve
		{
			protected override void InitializeImpl()
			{
				if (max > 2)
				{
					halfMax = (max - 1) >> 1;
					notPrimeEvery2nd = (bool*)Marshal.AllocHGlobal(halfMax); //index 0 = 3, index 1 = 5, etc.   |    represented as 0 or 1 in memory
					{
						var loops = halfMax >> 5;
						for (nint i = 0; i < loops; i++)
						{
							((Vector256<byte>*)notPrimeEvery2nd)[i] = default;
						}
						Unsafe.InitBlockUnaligned(notPrimeEvery2nd + (loops << 5), 0, (uint)(halfMax - (loops << 5)));
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

				return primeCount;
			}

			public override string CreateMessage(nint passes, double seconds)
			{
				return $"hamarb123_optimized,unmanaged,vectorized;{ passes };{ seconds };1;algorithm=base,faithful=no,bits=8";
			}
		}
	}
}
