using System;
namespace FastSieve
{
	public sealed class StandardSieve : Sieve
	{
		private bool[] notPrime;

		protected override void InitializeImpl()
		{
			notPrime = max > 2 ? new bool[max + 1] : null;
		}

		protected override object RunSieveImpl()
		{
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

			return notPrime;
		}

		protected override nint CountPrimesImpl()
		{
			nint primeCount = 1;
			for (nint i = 3; i <= max; i += 2)
			{
				if (!notPrime[i]) primeCount++;
			}

			return primeCount;
		}

		protected override void DisposeImpl()
		{
			notPrime = null;
		}

		public override string CreateMessage(nint passes, double seconds)
		{
			return $"hamarb123_standard;{ passes };{ seconds };1;algorithm=base,faithful=yes,bits=8";
		}
	}
}
