using System;
using System.Collections.Generic;

namespace PrimeCSharp.SieveDetails
{
    public static class SieveRunners
    {
        public const SieveProperty Bit2 = SieveProperty.BitarrayStorage | SieveProperty.Alg1of2;
        public const SieveProperty Bit2While = SieveProperty.BitarrayStorage | SieveProperty.Alg1of2 | SieveProperty.While;
        public const SieveProperty Bit6 = SieveProperty.BitarrayStorage | SieveProperty.Alg2of6;
        public const SieveProperty Bit30 = SieveProperty.BitarrayStorage | SieveProperty.Alg8of30;

        public const SieveProperty Bool2 = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2;
        public const SieveProperty Bool2While = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.While;
        public const SieveProperty Bool6 = SieveProperty.BoolArrayStorage | SieveProperty.Alg2of6;
        public const SieveProperty Bool30 = SieveProperty.BoolArrayStorage | SieveProperty.Alg8of30;

        public const SieveProperty IBool2 = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.InvertStorageValues;
        public const SieveProperty IBool2While = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.InvertStorageValues | SieveProperty.While;
        public const SieveProperty IBool6 = SieveProperty.BoolArrayStorage | SieveProperty.Alg2of6 | SieveProperty.InvertStorageValues;
        public const SieveProperty IBool30 = SieveProperty.BoolArrayStorage | SieveProperty.Alg8of30 | SieveProperty.InvertStorageValues;

        public const SieveProperty PoolB2 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg1of2;
        public const SieveProperty PoolB6 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg2of6;
        public const SieveProperty PoolB30 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg8of30;
        public const SieveProperty PoolD2 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg1of2;
        public const SieveProperty PoolD6 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg2of6;
        public const SieveProperty PoolD30 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg8of30;
        public const SieveProperty PoolQ2 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg1of2;
        public const SieveProperty PoolQ6 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg2of6;
        public const SieveProperty PoolQ30 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg8of30;
        public const SieveProperty PoolQ30M = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg8of30 | SieveProperty.Bitmasking;

        public const SieveProperty RawB2 = SieveProperty.RawStorage | SieveProperty.DataByte | SieveProperty.Alg1of2;
        public const SieveProperty RawB6 = SieveProperty.RawStorage | SieveProperty.DataByte | SieveProperty.Alg2of6;
        public const SieveProperty RawB30 = SieveProperty.RawStorage | SieveProperty.DataByte | SieveProperty.Alg8of30;
        public const SieveProperty RawD2 = SieveProperty.RawStorage | SieveProperty.DataDword | SieveProperty.Alg1of2;
        public const SieveProperty RawD6 = SieveProperty.RawStorage | SieveProperty.DataDword | SieveProperty.Alg2of6;
        public const SieveProperty RawD30 = SieveProperty.RawStorage | SieveProperty.DataDword | SieveProperty.Alg8of30;
        public const SieveProperty RawQ2 = SieveProperty.RawStorage | SieveProperty.DataQword | SieveProperty.Alg1of2;
        public const SieveProperty RawQ6 = SieveProperty.RawStorage | SieveProperty.DataQword | SieveProperty.Alg2of6;
        public const SieveProperty RawQ30 = SieveProperty.RawStorage | SieveProperty.DataQword | SieveProperty.Alg8of30;
        public const SieveProperty RawQ30M = SieveProperty.RawStorage | SieveProperty.DataQword | SieveProperty.Alg8of30 | SieveProperty.Bitmasking;


        public static readonly List<SieveProperty> SieveSets = new()
        {
            Bit2,
            Bit2While,
            Bit6,
            Bit30,
            Bool2,
            Bool2While,
            Bool6,
            Bool30,
            IBool2,
            IBool2While,
            IBool6,
            IBool30,
            PoolB2,
            PoolB6,
            PoolB30,
            PoolD2,
            PoolD6,
            PoolD30,
            PoolQ2,
            PoolQ6,
            PoolQ30,
            PoolQ30M,
            RawB2,
            RawB6,
            RawB30,
            RawD2,
            RawD6,
            RawD30,
            RawQ2,
            RawQ6,
            RawQ30,
            RawQ30M,
        };

        /// <summary>
        /// Get a list of the sieve runners that have any of the provided sieve properties.
        /// </summary>
        /// <param name="properties">A list of basic properties that a sieve runner might implement.</param>
        /// <returns>Returns a list of composed sieve properties that represent a sieve runner.</returns>
        public static List<SieveProperty> GetSieves(IEnumerable<SieveProperty> properties)
        {
            List<SieveProperty> sieves = new();

            foreach (SieveProperty sieve in SieveSets)
            {
                foreach (SieveProperty sieveProperty in properties)
                {
                    if (sieve.HasFlag(sieveProperty))
                    {
                        sieves.Add(sieve);
                        break;
                    }
                }
            }

            return sieves;
        }
    }
}
