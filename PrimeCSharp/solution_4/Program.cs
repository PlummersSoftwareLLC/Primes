using System;
using System.Diagnostics;
using System.Globalization;
using System.Threading;

namespace FastSieve
{
	unsafe class Program
	{
		static void Main(string[] args)
		{
			CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;
			Console.WriteLine("hamarb123's C# implementation: Started");

			//this could be an array that could be precalculated and then stored in code, but I feel that this is more readable
			Sieve.InitializePopcountByte();

			//this could be an array that could be precalculated and then stored in code, but I feel that this is more readable
			Sieve.InitializeNextPrime();

			//Jit each method by calling so metrics aren't scewed by first-time run of method:
			{
				{
					using Sieve sieve = new Sieve();
					sieve.Initialize(1, 0);
					sieve.RunSieve();
					sieve.CountPrimes();
				}
				{
					using Sieve sieve = new Sieve();
					sieve.Initialize(2, 0);
					sieve.RunSieve();
					sieve.CountPrimes();
				}
				{
					using Sieve sieve = new Sieve();
					sieve.Initialize(4, 0);
					sieve.RunSieve();
					sieve.CountPrimes();
				}
				{
					using Sieve sieve = new Sieve();
					sieve.Initialize(6, 0);
					sieve.RunSieve();
					sieve.CountPrimes();
				}
			}

			//args = new string[] { "runallchecks" };

			if (args.Length == 0 || (args[0] == "runall" && args.Length == 1))
			{
				//Thread.Sleep allows the cpu to calm down in case of thermal throttle like on my machine, should probably sleep longer
				double time = 5;
				Thread.Sleep(6000);
				Run(7, time);
				Thread.Sleep(3000);
				Run(6, time);
				Thread.Sleep(3000);
				Run(5, time);
				Thread.Sleep(3000);
				Run(4, time);
				Thread.Sleep(3000);
				Run(3, time);
				Thread.Sleep(3000);
				Run(2, time);
				Thread.Sleep(3000);
				Run(1, time);
				return;
			}
			else if (args[0] == "runallchecks" && args.Length == 1)
			{
				RunTest(2, 1);
				RunTest(3, 2);
				RunTest(4, 2);
				RunTest(5, 3);
				RunTest(10, 4);
				RunTest(100, 25);
				RunTest(1000, 168);
				RunTest(10000, 1229);
				RunTest(100000, 9592);
				RunTest(1000000, 78498);
				RunTest(10000000, 664579);
				RunTest(100000000, 5761455);
				RunTest(1000000000, 50847534);
				if (IntPtr.Size >= 8) RunTest(unchecked((nint)10000000000), 455052511); //this test is rather slow, but important for testing 64-bit numbers
				Console.WriteLine("Checks have all run.");
				return;
			}
			Console.WriteLine($"Invalid command line arguments ('{ Environment.CommandLine }'). Options are:");
			Console.WriteLine("\t<program> [runall]");
			Console.WriteLine("\t<program> runallchecks");
		}

		public static void Run(int method, double time)
		{
			const nint max = 1_000_000;
			long timerTicksPerSecond = Stopwatch.Frequency;
			long durationLimit = (long)Math.Ceiling(time * timerTicksPerSecond);

			Stopwatch watch = Stopwatch.StartNew();

			nint passes = 0;

			while (watch.ElapsedTicks < durationLimit)
			{
				using var sieve = new Sieve();
				sieve.Initialize(method, max);
				sieve.RunSieve();
				nint value = sieve.CountPrimes();
				if (value != 78498)
				{
					Console.WriteLine("Invalid result (method " + method.ToString() + ") " + value.ToString() + ", expected 78498");
					return;
				}
				passes++;
			}

			watch.Stop();
			string methodName;
			if (method == 1) methodName = "standard";
			else if (method == 2) methodName = "optimized";
			else if (method == 3) methodName = "optimized,vectorized";
			else if (method == 4 || method == 6) methodName = "optimized,unmanaged";
			else /*if (method == 5 || method == 7)*/ methodName = "optimized,unmanaged,vectorized";
			Console.WriteLine($"hamarb123_{ methodName }{ (method < 6 ? "" : ",onebit") };{ passes };{ watch.Elapsed.TotalSeconds };1;algorithm=base,faithful={ (methodName.Contains("vectorized") ? "no" : "yes") },bits={ (method < 6 ? 8 : 1) }");
		}

		public static void RunTest(nint max, nint expected)
		{
			if (max <= 0X7FFFFFC7L) //array based implementations
			{
				var sieve1 = new Sieve();
				sieve1.Initialize(1, max);
				sieve1.RunSieve();
				nint value1 = sieve1.CountPrimes();
				if (value1 != expected)
				{
					Console.WriteLine("Invalid result (method 1) with max " + max.ToString() + ", got " + value1.ToString() + ", expected " + expected.ToString());
				}
			}
			if (max <= (0X7FFFFFC7L * 2 + 1)) //array based implementations
			{
				var sieve2 = new Sieve();
				sieve2.Initialize(2, max);
				sieve2.RunSieve();
				nint value2 = sieve2.CountPrimes();
				if (value2 != expected)
				{
					Console.WriteLine("Invalid result (method 2) with max " + max.ToString() + ", got " + value2.ToString() + ", expected " + expected.ToString());
				}
				var sieve3 = new Sieve();
				sieve3.Initialize(3, max);
				sieve3.RunSieve();
				nint value3 = sieve3.CountPrimes();
				if (value3 != expected)
				{
					Console.WriteLine("Invalid result (method 3) with max " + max.ToString() + ", got " + value3.ToString() + ", expected " + expected.ToString());
				}
			}
			var sieve4 = new Sieve();
			sieve4.Initialize(4, max);
			sieve4.RunSieve();
			nint value4 = sieve4.CountPrimes();
			if (value4 != expected)
			{
				Console.WriteLine("Invalid result (method 4) with max " + max.ToString() + ", got " + value4.ToString() + ", expected " + expected.ToString());
			}
			var sieve5 = new Sieve();
			sieve5.Initialize(5, max);
			sieve5.RunSieve();
			nint value5 = sieve5.CountPrimes();
			if (value5 != expected)
			{
				Console.WriteLine("Invalid result (method 5) with max " + max.ToString() + ", got " + value5.ToString() + ", expected " + expected.ToString());
			}
			var sieve6 = new Sieve();
			sieve6.Initialize(6, max);
			sieve6.RunSieve();
			nint value6 = sieve6.CountPrimes();
			if (value6 != expected)
			{
				Console.WriteLine("Invalid result (method 6) with max " + max.ToString() + ", got " + value6.ToString() + ", expected " + expected.ToString());
			}
			var sieve7 = new Sieve();
			sieve7.Initialize(7, max);
			sieve7.RunSieve();
			nint value7 = sieve7.CountPrimes();
			if (value7 != expected)
			{
				Console.WriteLine("Invalid result (method 7) with max " + max.ToString() + ", got " + value7.ToString() + ", expected " + expected.ToString());
			}
			Console.WriteLine("Finished checks for max " + max.ToString());
		}
	}
}
