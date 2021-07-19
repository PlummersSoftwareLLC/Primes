using System;
using System.Runtime.Intrinsics.X86;

namespace FastSieve
{
	public class OptimizedSieve : Sieve
	{
		protected bool[] notPrimeEvery2nd;
		protected nint halfMax;

		protected override void InitializeImpl()
		{
			if (max > 2)
			{
				halfMax = (max - 1) >> 1;
				notPrimeEvery2nd = new bool[halfMax]; //index 0 = 3, index 1 = 5, etc.   |    represented as 0 or 1 in memory
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

			return notPrimeEvery2nd;
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
			notPrimeEvery2nd = null;
		}

		public override string CreateMessage(nint passes, double seconds)
		{
			return $"hamarb123_optimized;{ passes };{ seconds };1;algorithm=base,faithful=yes,bits=8";
		}

		public unsafe sealed class Vectorized : OptimizedSieve
		{
			protected override nint CountPrimesImpl()
			{
				nint primeCount = 1;

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

				return primeCount;
			}

			public override string CreateMessage(nint passes, double seconds)
			{
				return $"hamarb123_optimized,vectorized;{ passes };{ seconds };1;algorithm=base,faithful=no,bits=8";
			}
		}
	}
}
