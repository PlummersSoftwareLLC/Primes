using System;
using System.Collections.Generic;
using CommandLine;
using PrimeCSharp.SieveDetails;

namespace PrimeCSharp.Config
{
    public class SettingsV2
    {
        [Option('s', "size",
            Default = 1_000_000, HelpText = "The size of the sieve to run.")]
        public int SieveSize { get; set; }

        [Option('t', "time",
            Default = 5, HelpText = "How many seconds to run the sieve.")]
        public int SecondsToRun { get; set; }

        [Option('v', "verbose",
            Default = false, HelpText = "Show the prime numbers that were found.")]
        public bool Verbose { get; set; }

        [Option('m', "multi",
            Default = false, HelpText = "Run the sieve in a multi-threaded manner.")]
        public bool MultiThreaded { get; set; }

        [Option("threads",
            Default = 0, HelpText = "Set the number of threads to use while multithreading. Setting to 0 will make it use the number of processors on the machine.")]
        public int ThreadCount { get; set; }

        [Option("pthreads",
            Default = 0, HelpText = "Set the number of threads to use while running parallel algorithms. Setting to 0 will make it use the number of processors on the machine.")]
        public int PThreadCount { get; set; }


        [Option("benchmark",
            Default = false, HelpText = "Run benchmarks on the sieve implementations.")]
        public bool Benchmark { get; set; }

        [Option('b', "bench",
            Default = "", HelpText = "A type of benchmark to run. Leaving it blank will run the standard benchmarks. Current options: mod, ref, ofN, par")]
        public string? Bench { get; set; }


        [Option("all",
            Default = false, HelpText = "Run all sieves")]
        public bool All { get; set; }


        [Option("bit2",
            Default = false, HelpText = "Use bitarray storage with the 1 of 2 algorithm")]
        public bool Bit2 { get; set; }

        [Option("bit2while",
            Default = false, HelpText = "Use bitarray storage with the 1 of 2 algorithm and while loop")]
        public bool Bit2While { get; set; }

        [Option("bit6",
            Default = false, HelpText = "Use bitarray storage with the 2 of 6 algorithm")]
        public bool Bit6 { get; set; }

        [Option("bit30",
            Default = false, HelpText = "Use bitarray storage with the 8 of 30 algorithm")]
        public bool Bit30 { get; set; }


        [Option("bool2",
            Default = false, HelpText = "Use bool array storage with the 1 of 2 algorithm")]
        public bool Bool2 { get; set; }

        [Option("bool2while",
            Default = false, HelpText = "Use bool array storage with the 1 of 2 algorithm and while loop")]
        public bool Bool2While { get; set; }

        [Option("bool6",
            Default = false, HelpText = "Use bool array storage with the 2 of 6 algorithm")]
        public bool Bool6 { get; set; }

        [Option("bool30",
            Default = false, HelpText = "Use bool array storage with the 8 of 30 algorithm")]
        public bool Bool30 { get; set; }


        [Option("ibool2",
            Default = false, HelpText = "Use inverted bool array storage with the 1 of 2 algorithm")]
        public bool iBool2 { get; set; }

        [Option("ibool2while",
            Default = false, HelpText = "Use inverted bool array storage with the 1 of 2 algorithm and while loop")]
        public bool iBool2While { get; set; }

        [Option("ibool6",
            Default = false, HelpText = "Use inverted bool array storage with the 2 of 6 algorithm")]
        public bool iBool6 { get; set; }

        [Option("ibool30",
            Default = false, HelpText = "Use inverted bool array storage with the 8 of 30 algorithm")]
        public bool iBool30 { get; set; }


        [Option("poolb2",
            Default = false, HelpText = "Use array pool byte storage with the 1 of 2 algorithm")]
        public bool PoolB2 { get; set; }

        [Option("poolb6",
            Default = false, HelpText = "Use array pool byte storage with the 2 of 6 algorithm")]
        public bool PoolB6 { get; set; }

        [Option("poolb30",
            Default = false, HelpText = "Use array pool byte storage with the 8 of 30 algorithm")]
        public bool PoolB30 { get; set; }

        [Option("poold2",
            Default = false, HelpText = "Use array pool 32-bit storage with the 1 of 2 algorithm")]
        public bool PoolD2 { get; set; }

        [Option("poold6",
            Default = false, HelpText = "Use array pool 32-bit storage with the 2 of 6 algorithm")]
        public bool PoolD6 { get; set; }

        [Option("poold30",
            Default = false, HelpText = "Use array pool 32-bit storage with the 8 of 30 algorithm")]
        public bool PoolD30 { get; set; }

        [Option("poolq2",
            Default = false, HelpText = "Use array pool 64-bit storage with the 1 of 2 algorithm")]
        public bool PoolQ2 { get; set; }

        [Option("poolq6",
            Default = false, HelpText = "Use array pool 64-bit storage with the 2 of 6 algorithm")]
        public bool PoolQ6 { get; set; }

        [Option("poolq30",
            Default = false, HelpText = "Use array pool 64-bit storage with the 8 of 30 algorithm")]
        public bool PoolQ30 { get; set; }

        [Option("poolq30m",
            Default = false, HelpText = "Use array pool 64-bit storage with the 8 of 30 algorithm and bitmasking")]
        public bool PoolQ30M { get; set; }


        [Option("rawb2",
            Default = false, HelpText = "Use manual byte storage with the 1 of 2 algorithm")]
        public bool RawB2 { get; set; }

        [Option("rawb6",
            Default = false, HelpText = "Use manual byte storage with the 2 of 6 algorithm")]
        public bool RawB6 { get; set; }

        [Option("rawb30",
            Default = false, HelpText = "Use manual byte storage with the 8 of 30 algorithm")]
        public bool RawB30 { get; set; }

        [Option("rawd2",
            Default = false, HelpText = "Use manual 32-bit storage with the 1 of 2 algorithm")]
        public bool RawD2 { get; set; }

        [Option("rawd6",
            Default = false, HelpText = "Use manual 32-bit storage with the 2 of 6 algorithm")]
        public bool RawD6 { get; set; }

        [Option("rawd30",
            Default = false, HelpText = "Use manual 32-bit storage with the 8 of 30 algorithm")]
        public bool RawD30 { get; set; }

        [Option("rawq2",
            Default = false, HelpText = "Use manual 64-bit storage with the 1 of 2 algorithm")]
        public bool RawQ2 { get; set; }

        [Option("rawq6",
            Default = false, HelpText = "Use manual 64-bit storage with the 2 of 6 algorithm")]
        public bool RawQ6 { get; set; }

        [Option("rawq30",
            Default = false, HelpText = "Use manual 64-bit storage with the 8 of 30 algorithm")]
        public bool RawQ30 { get; set; }

        [Option("rawq30m",
            Default = false, HelpText = "Use manual 64-bit storage with the 8 of 30 algorithm")]
        public bool RawQ30M { get; set; }


        [Option("bitarray",
            Default = false, HelpText = "Run all sieves that use a bitarray")]
        public bool Bitarray { get; set; }

        [Option("bool",
            Default = false, HelpText = "Run all sieves that use a bool array")]
        public bool Boolarray { get; set; }

        [Option("pool",
            Default = false, HelpText = "Run all sieves that use an array pool")]
        public bool Arraypool { get; set; }

        [Option("raw",
            Default = false, HelpText = "Run all sieves that use manual memory allocation")]
        public bool RawArray { get; set; }

        [Option("invert",
            Default = false, HelpText = "Run all sieves that use inverted storage logic")]
        public bool Invert { get; set; }

        [Option("bytes",
            Default = false, HelpText = "Run all sieves that use byte storage")]
        public bool DataByte { get; set; }

        [Option("32bit",
            Default = false, HelpText = "Run all sieves that use 32 bit storage")]
        public bool DataDword { get; set; }

        [Option("64bit",
            Default = false, HelpText = "Run all sieves that use 64 bit storage")]
        public bool DataQword { get; set; }

        [Option("1of2",
            Default = false, HelpText = "Run all sieves that use the 1 of 2 algorithm")]
        public bool Alg1of2 { get; set; }

        [Option("2of6",
            Default = false, HelpText = "Run all sieves that use the 2 of 6 algorithm")]
        public bool Alg2of6 { get; set; }

        [Option("8of30",
            Default = false, HelpText = "Run all sieves that use the 8 of 30 algorithm")]
        public bool Alg8of30 { get; set; }

        [Option("bitmask",
            Default = false, HelpText = "Run all sieves that use a bitmask wheel")]
        public bool Bitmask { get; set; }

        [Option("parallel",
            Default = false, HelpText = "Run all sieves that use parallelized algorithms")]
        public bool Parallel { get; set; }

        [Option("composed",
            Default = false, HelpText = "")]
        public bool Composed { get; set; }


        internal List<SieveProperty> GetSieves()
        {
            List<SieveProperty> sieves = new();

            if (All)
            {
                sieves.AddRange(SieveDetails.SieveRunners.SieveSets);
                return sieves;
            }

            if (Bit2)
                sieves.Add(SieveDetails.SieveRunners.Bit2);
            if (Bit2While)
                sieves.Add(SieveDetails.SieveRunners.Bit2While);
            if (Bit6)
                sieves.Add(SieveDetails.SieveRunners.Bit6);
            if (Bit30)
                sieves.Add(SieveDetails.SieveRunners.Bit30);
            if (Bool2)
                sieves.Add(SieveDetails.SieveRunners.Bool2);
            if (Bool2While)
                sieves.Add(SieveDetails.SieveRunners.Bool2While);
            if (Bool6)
                sieves.Add(SieveDetails.SieveRunners.Bool6);
            if (Bool30)
                sieves.Add(SieveDetails.SieveRunners.Bool30);
            if (iBool2)
                sieves.Add(SieveDetails.SieveRunners.IBool2);
            if (iBool2While)
                sieves.Add(SieveDetails.SieveRunners.IBool2While);
            if (iBool6)
                sieves.Add(SieveDetails.SieveRunners.IBool6);
            if (iBool30)
                sieves.Add(SieveDetails.SieveRunners.IBool30);
            if (PoolB2)
                sieves.Add(SieveDetails.SieveRunners.PoolB2);
            if (PoolB6)
                sieves.Add(SieveDetails.SieveRunners.PoolB6);
            if (PoolB30)
                sieves.Add(SieveDetails.SieveRunners.PoolB30);
            if (PoolD2)
                sieves.Add(SieveDetails.SieveRunners.PoolD2);
            if (PoolD6)
                sieves.Add(SieveDetails.SieveRunners.PoolD6);
            if (PoolD30)
                sieves.Add(SieveDetails.SieveRunners.PoolD30);
            if (PoolQ2)
                sieves.Add(SieveDetails.SieveRunners.PoolQ2);
            if (PoolQ6)
                sieves.Add(SieveDetails.SieveRunners.PoolQ6);
            if (PoolQ30)
                sieves.Add(SieveDetails.SieveRunners.PoolQ30);
            if (PoolQ30M)
                sieves.Add(SieveDetails.SieveRunners.PoolQ30M);
            if (RawB2)
                sieves.Add(SieveDetails.SieveRunners.RawB2);
            if (RawB6)
                sieves.Add(SieveDetails.SieveRunners.RawB6);
            if (RawB30)
                sieves.Add(SieveDetails.SieveRunners.RawB30);
            if (RawD2)
                sieves.Add(SieveDetails.SieveRunners.RawD2);
            if (RawD6)
                sieves.Add(SieveDetails.SieveRunners.RawD6);
            if (RawD30)
                sieves.Add(SieveDetails.SieveRunners.RawD30);
            if (RawQ2)
                sieves.Add(SieveDetails.SieveRunners.RawQ2);
            if (RawQ6)
                sieves.Add(SieveDetails.SieveRunners.RawQ6);
            if (RawQ30)
                sieves.Add(SieveDetails.SieveRunners.RawQ30);
            if (RawQ30M)
                sieves.Add(SieveDetails.SieveRunners.RawQ30M);


            var sieveProperties = GetSieveProperties();

            foreach (var sieve in SieveDetails.SieveRunners.GetSieves(sieveProperties))
            {
                if (!sieves.Contains(sieve))
                    sieves.Add(sieve);
            }

            return sieves;
        }

        private List<SieveProperty> GetSieveProperties()
        {
            List<SieveProperty> properties = new();

            if (Bitarray)
                properties.Add(SieveProperty.BitarrayStorage);
            if (Boolarray)
            {
                properties.Add(SieveProperty.BoolArrayStorage);

                if (Invert)
                    properties.Add(SieveProperty.InvertStorageValues);
            }
            if (Arraypool)
                properties.Add(SieveProperty.PoolStorage);
            if (RawArray)
                properties.Add(SieveProperty.RawStorage);

            if (DataByte)
                properties.Add(SieveProperty.DataByte);
            if (DataDword)
                properties.Add(SieveProperty.DataDword);
            if (DataQword)
                properties.Add(SieveProperty.DataQword);

            if (Alg1of2)
                properties.Add(SieveProperty.Alg1of2);
            if (Alg2of6)
                properties.Add(SieveProperty.Alg2of6);
            if (Alg8of30)
                properties.Add(SieveProperty.Alg8of30);

            if (Bitmask)
                properties.Add(SieveProperty.Bitmasking);
            if (Parallel)
                properties.Add(SieveProperty.Parallel);
            if (Composed)
                properties.Add(SieveProperty.Composed);


            return properties;
        }
    }
}
