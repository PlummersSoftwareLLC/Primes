/*
JavaScript implementation of Prime Sieve using worker threads. This is the
parent entry file, and its functionality has been extracted from PrimeJavaScript_cluster.js

Author: Dief Bell
Date:   2022-07-10
*/
"use strict";


import { dirname, join } from "path";

import { PrimeSieve } from "./PrimeSieve.mjs";
import { Worker } from "worker_threads";
import { fileURLToPath } from "url";
import os from "os";



let runtime = "";
let verbose = false;
try
{
    !!Deno;
    runtime = "deno";
    verbose = Deno.args.includes("verbose");
}
catch
{
    const runtimeParts = process.argv[0].split("/");
    runtime = runtimeParts[runtimeParts.length - 1];
    verbose = process.argv.includes("verbose");
}

const NOW_UNITS_PER_SECOND = 1000;

// Historical data for validating our results - the number of primes
// to be found under some limit, such as 168 primes under 1000
const knownPrimeCounts = {
    10: 4,
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455
};


// get a single sieve (for validation and statistics)
const evalSieve = (sieveSize, maxShowPrimes = 100) =>
{
    const sieve = new PrimeSieve(sieveSize);
    sieve.runSieve();
    return {
        "countedPrimes": sieve.countPrimes(),
        "primeArray": sieve.getPrimes(maxShowPrimes)
    };
}

const printResult = (config, numPasses, durationInSec) =>
{
    const { maxShowPrimes, sieveSize, numThreads } = config;

    // validate algorithm - run one final time on the result
    const sieveResult = evalSieve(sieveSize, maxShowPrimes);
    let validResult = false;
    if (sieveSize in knownPrimeCounts)
    {
        const knownPrimeCount = knownPrimeCounts[sieveSize];
        validResult = (knownPrimeCount == sieveResult.countedPrimes);

        if (!validResult) console.log(
            "\nError: invalid result.",
            `Limit for ${sieveSize} should be ${knownPrimeCount}`,
            `but result contains ${sieveResult.countedPrimes} primes.`
        );
    }
    else console.log(
        `Warning: cannot validate result of ${sieveResult.countedPrimes} primes:`,
        `limit ${sieveSize} is not in the known list of number of primes!`
    );

    if (validResult)
    {
        const res = [
            `\ndiefbell-worker-${runtime}`,
            numPasses.toString(),
            durationInSec.toString(),
            numThreads.toString(),
            "algorithm=base,faithful=yes,bits=1"
        ];
        console.log(res.join(";"));
    }

    if (config.verbose)
    {
        console.log(`\nThe first ${maxShowPrimes} found primes are:`, sieveResult.primeArray);

        console.log(
            `Passes: ${numPasses},`,
            `Time: ${(durationInSec).toFixed(2)},`,
            `Avg: ${(durationInSec / numPasses).toFixed(8)} (sec/pass),`,
            ` ${(numPasses / durationInSec).toFixed(2)} (pass/sec),`,
            `Sieve size: ${sieveSize},`,
            `Primes: ${sieveResult.countedPrimes},`,
            `Valid: ${validResult}`
        );
    }

}


const main = async (config) =>
{
    const { numThreads, sieveSize, timeLimitSeconds } = config;
    const workers = [];

    let childWorkerFile = "";
    try
    {
        // Bun
        childWorkerFile = join(__dirname, "PrimeJavaScript_worker_child.mjs");
    }
    catch
    {
        // Node ES6
        const fileName = fileURLToPath(import.meta.url);
        childWorkerFile = join(dirname(fileName), "PrimeJavaScript_worker_child.mjs");
    }

    const timeStart = performance.now();

    for (let i = 0; i < numThreads; i++)
    {
        workers.push(new Promise((resolve, reject) =>
        {
            const workerData = { sieveSize, timeLimitSeconds };
            const worker = new Worker(childWorkerFile, { workerData });

            worker.on("message", resolve);
            worker.on("error", reject);
            worker.on("exit", (code) =>
            {
                if (code !== 0) reject();
            });
        }));
    }

    const workerPasses = await Promise.all(workers);
    const timeEnd = performance.now();

    const totalPasses = workerPasses.reduce((currTotalPasses, workerPasses) => currTotalPasses + workerPasses, 0);
    const durationInSec = (timeEnd - timeStart) / NOW_UNITS_PER_SECOND;

    printResult(config, totalPasses, durationInSec);
}


const config = {
    verbose: verbose,
    timeLimitSeconds: 5,
    sieveSize: 1000000,
    maxShowPrimes: 100,
    // automatically get # of threads from os, if intel assume hyperthreading
    // the || 2 is just because os.cpus() currently returns an empty array in Bun :(
    numThreads: os.cpus().length / (os.cpus()[0]?.model.includes("Intel") ? 2 : 1) || 2
};

await main(config);
