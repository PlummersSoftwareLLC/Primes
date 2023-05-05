/*
Child worker for the PrimeJavaScript_worker JavaScript implementation of Prime Sieve.
Intended to be run using worker_threads from NodeJS.

Functionality extracted from PrimeJavaScript_cluster.js.

Author: Dief Bell
Date:   2022-07-10
*/
"use strict";

import { parentPort, workerData } from "worker_threads";
import { PrimeSieve } from "./PrimeSieve.mjs";



let runtime = "";
try
{
	!!Deno;
	runtime = "deno";
}
catch
{
	const runtimeParts = process.argv[0].split("/");
	runtime = runtimeParts[runtimeParts.length - 1];
}

const NOW_UNITS_PER_SECOND = 1000;

// run the sieve for timeLimitSeconds
const runSieveBatch = (sieveSize, timeLimitSeconds = 5) =>
{
	let nrOfPasses = 0;  // Counter for the number of passes in a from timestart to timefinish

	const timeStart = performance.now();  // Record starting time
	const timeFinish = timeStart + timeLimitSeconds * NOW_UNITS_PER_SECOND;  // Calculate finish time before, so we don't repeat

	do
	{
		const sieve = new PrimeSieve(sieveSize);
		sieve.runSieve();
		nrOfPasses++;
	}
	while (performance.now() < timeFinish);  // keep going for timeLimitSeconds

	return nrOfPasses;
}


const main = ({ sieveSize, timeLimitSeconds }) =>
{
	const numPasses = runSieveBatch(sieveSize, timeLimitSeconds);

	parentPort.postMessage(numPasses);
	process.exit(0);
};

main(workerData);
