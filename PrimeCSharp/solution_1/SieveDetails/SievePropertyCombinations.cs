using System;
using System.Linq;
using System.Collections.Generic;

namespace PrimeCSharp.SieveDetails
{
    public static class SievePropertyCombinations
    {
        public const SieveProperty Bit2 = SieveProperty.BitarrayStorage | SieveProperty.Alg1of2;
        public const SieveProperty Bit2While = SieveProperty.BitarrayStorage | SieveProperty.Alg1of2 | SieveProperty.While;
        public const SieveProperty Bit6 = SieveProperty.BitarrayStorage | SieveProperty.Alg2of6;
        public const SieveProperty Bit30 = SieveProperty.BitarrayStorage | SieveProperty.Alg8of30;

        public const SieveProperty Bool2 = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2;
        public const SieveProperty Bool2While = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.While;
        public const SieveProperty Bool6 = SieveProperty.BoolArrayStorage | SieveProperty.Alg2of6;
        public const SieveProperty Bool30 = SieveProperty.BoolArrayStorage | SieveProperty.Alg8of30;

        public const SieveProperty InvBool2 = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.InvertStorageValues;
        public const SieveProperty InvBool2While = SieveProperty.BoolArrayStorage | SieveProperty.Alg1of2 | SieveProperty.InvertStorageValues | SieveProperty.While;
        public const SieveProperty InvBool6 = SieveProperty.BoolArrayStorage | SieveProperty.Alg2of6 | SieveProperty.InvertStorageValues;
        public const SieveProperty InvBool30 = SieveProperty.BoolArrayStorage | SieveProperty.Alg8of30 | SieveProperty.InvertStorageValues;

        public const SieveProperty PoolB2 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg1of2;
        public const SieveProperty PoolB6 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg2of6;
        public const SieveProperty PoolB30 = SieveProperty.PoolStorage | SieveProperty.DataByte | SieveProperty.Alg8of30;
        public const SieveProperty PoolD2 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg1of2;
        public const SieveProperty PoolD6 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg2of6;
        public const SieveProperty PoolD30 = SieveProperty.PoolStorage | SieveProperty.DataDword | SieveProperty.Alg8of30;
        public const SieveProperty PoolQ2 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg1of2;
        public const SieveProperty PoolQ6 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg2of6;
        public const SieveProperty PoolQ30 = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg8of30;
        public const SieveProperty PoolQ2M = SieveProperty.PoolStorage | SieveProperty.DataQword | SieveProperty.Alg1of2 | SieveProperty.Bitmasking;
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
            InvBool2,
            InvBool2While,
            InvBool6,
            InvBool30,
            PoolB2,
            PoolB6,
            PoolB30,
            PoolD2,
            PoolD6,
            PoolD30,
            PoolQ2,
            PoolQ6,
            PoolQ30,
            PoolQ2M,
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

        private static readonly List<SieveProperty> StorageProperties = new()
        {
            SieveProperty.BitarrayStorage,
            SieveProperty.BoolArrayStorage,
            SieveProperty.InvertStorageValues,
            SieveProperty.PoolStorage,
            SieveProperty.RawStorage
        };

        private static readonly List<SieveProperty> DataProperties = new()
        {
            SieveProperty.DataByte,
            SieveProperty.DataDword,
            SieveProperty.DataQword
        };

        private static readonly List<SieveProperty> AlgorithmProperties = new()
        {
            SieveProperty.Alg1of2,
            SieveProperty.Alg2of6,
            SieveProperty.Alg8of30,
            SieveProperty.Bitmasking,
            SieveProperty.While
        };

        private static readonly List<SieveProperty> OtherProperties = new()
        {
            SieveProperty.Composed,
            SieveProperty.Parallel
        };

        /// <summary>
        /// Get a list of the sieve runners that have any of the provided sieve properties.
        /// </summary>
        /// <param name="properties">A list of basic properties that a sieve runner might implement.</param>
        /// <returns>Returns a list of composed sieve properties that represent a sieve runner.</returns>
        public static List<SieveProperty> GetSieves(IEnumerable<SieveProperty> properties)
        {
            if (!properties.Any())
                    return new List<SieveProperty>();

            IEnumerable<SieveProperty> requestedStorage = StorageProperties.Intersect(properties);
            IEnumerable<SieveProperty> requestedData = DataProperties.Intersect(properties);
            IEnumerable<SieveProperty> requestedAlgorithms = AlgorithmProperties.Intersect(properties);
            IEnumerable<SieveProperty> requestedOther = OtherProperties.Intersect(properties);

            IEnumerable<SieveProperty> s = SieveSets;

            s = s.Where(p => !requestedStorage.Any() || (requestedStorage.Any(a => p.HasFlag(a))));
            s = s.Where(p => !requestedData.Any() || (requestedData.Any(a => p.HasFlag(a))));
            s = s.Where(p => !requestedAlgorithms.Any() || (requestedAlgorithms.Any(a => p.HasFlag(a))));
            s = s.Where(p => !requestedOther.Any() || (requestedOther.Any(a => p.HasFlag(a))));

            return s.ToList();
        }
    }
}
