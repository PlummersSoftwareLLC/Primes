using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

namespace FastSieve
{
	public unsafe abstract class Sieve : IDisposable
	{
		private bool run;
		protected nint max;

		protected abstract void InitializeImpl();
		protected abstract object RunSieveImpl();
		protected abstract nint CountPrimesImpl();

		public object RunSieve()
		{
			if (disposedValue) throw new ObjectDisposedException("this", "Sieve is already disposed");
			if (run) throw new Exception("Already run");

			run = true;

			if (max <= 2) return null;
			return RunSieveImpl();
		}

		public nint CountPrimes()
		{
			if (disposedValue) throw new ObjectDisposedException("this", "Sieve is already disposed");
			if (!run) throw new Exception("Not run");

			if (max <= 2) return max == 2 ? 1 : 0;
			return CountPrimesImpl();
		}

		[MethodImpl(MethodImplOptions.NoInlining)]
		public static Sieve Create(int algorithm, nint max)
		{
			Sieve sieve;
			switch (algorithm)
			{
				case 1:
				{
					sieve = new StandardSieve();
					break;
				}
				case 2:
				{
					sieve = new OptimizedSieve();
					break;
				}
				case 3:
				{
					sieve = new OptimizedSieve.Vectorized();
					break;
				}
				case 4:
				{
					sieve = new OptimizedUnmanagedSieve();
					break;
				}
				case 5:
				{
					sieve = new OptimizedUnmanagedSieve.Vectorized();
					break;
				}
				case 6:
				{
					sieve = new OptimizedUnmanaged1BitSieve();
					break;
				}
				case 7:
				{
					sieve = new OptimizedUnmanaged1BitSieve.Vectorized();
					break;
				}
				default:
				{
					throw new Exception("Invalid algorithm");
				}
			}

			sieve.max = max;
			sieve.InitializeImpl();

			return sieve;
		}

		protected abstract void DisposeImpl();

		private bool disposedValue;

		private void Dispose(bool disposing)
		{
			if (!disposedValue)
			{
				if (disposing)
				{
					//Dispose managed state (managed objects)
				}

				//Free unmanaged resources (unmanaged objects) and override finalizer
				//Set large fields to null
				DisposeImpl();

				disposedValue = true;
			}
		}

		~Sieve()
		{
		    //Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
		    Dispose(disposing: false);
		}

		public void Dispose()
		{
			//Do not change this code. Put cleanup code in 'Dispose(bool disposing)' method
			Dispose(disposing: true);
			GC.SuppressFinalize(this);
		}

		public abstract string CreateMessage(nint passes, double seconds);
	}
}
