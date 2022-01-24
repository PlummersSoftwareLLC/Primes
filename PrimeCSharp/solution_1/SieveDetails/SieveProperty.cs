using System;

namespace PrimeCSharp.SieveDetails
{
    [Flags]
    public enum SieveProperty
    {
        None = 0,

        // Storage types
        BitarrayStorage = 1,
        BoolArrayStorage = 2,
        PoolStorage = 4,
        RawStorage = 8,

        // Data size for some arrays
        DataByte = 16,
        DataDword = 32,
        DataQword = 64,

        // Algorithm used
        Alg1of2 = 128,
        Alg2of6 = 256,
        Alg8of30 = 512,

        // Other flags
        InvertStorageValues = 1024,
        Bitmasking = 2048,
        Parallel = 4096,
        Composed = 8192,
        While = 16384,
    }
}
