import * as fs from "fs";
import * as path from "path";
import glob from "glob";
import * as si from "systeminformation";

import { Command } from "commander";
import Docker from "dockerode";

interface IResult {
  implementation: string;
  solution: string;
  label: string;
  passes: number;
  duration: number;
  threads: number;
}

export const command = new Command("report")
  .requiredOption("-d, --directory <directory>", "Results directory")
  // .requiredOption("-o, --output <output_directory>", "Reports output directory")
  .action(async (args) => {
    const directory = path.resolve(args.directory as string);
    if (!fs.existsSync(directory)) {
      console.error(`Directory ${directory} does not exist!`);
      return;
    }

    const results = glob.sync(path.join(directory, "*.out")).flatMap((file) => {
      const [implementation, solution] = path
        .basename(file, ".out")
        .split("-", 2);

      const localResults = fs
        .readFileSync(file, "utf-8")
        .split(/\r?\n/)
        .map((line) => {
          const match = line
            .trim()
            .match(
              /^(?<label>.+)\s*;\s*(?<passes>\d+)\s*;\s*(?<duration>\d+([.]\d+)?)\s*;\s*(?<threads>\d+)$/
            );

          return match
            ? ({
                implementation: implementation.replace("prime", ""),
                solution: solution.replace("solution_", ""),
                label: match.groups!["label"],
                passes: +match.groups!["passes"],
                duration: +match.groups!["duration"],
                threads: +match.groups!["threads"],
              } as IResult)
            : undefined;
        })
        .filter((result): result is IResult => !!result);

      if (localResults.length === 0) console.warn(`No valid output: ${file}`);

      return localResults;
    });

    if (results.length === 0) {
      console.error("No data was found!");
      return;
    }

    generateReport(
      "Software Drag Race (single-threaded)",
      results.filter((value) => value.threads === 1)
    );

    generateReport(
      "Software Drag Race (multi-threaded)",
      results.filter((result) => result.threads > 1)
    );

    const docker = new Docker();
    const dockerInfo = await docker.info();

    const report = {
      version: "1",
      metadata: {
        date: Math.floor(new Date().getTime() / 1000),
      },
      machine: {
        cpu: await si.cpu(),
        os: await si.osInfo(),
        system: await si.system(),
        docker: {
          kernelVersion: dockerInfo["KernelVersion"],
          operatingSystem: dockerInfo["OperatingSystem"],
          osVersion: dockerInfo["OSVersion"],
          osType: dockerInfo["OSType"],
          architecture: dockerInfo["Architecture"],
          ncpu: dockerInfo["NCPU"],
          memTotal: dockerInfo["MemTotal"],
          serverVersion: dockerInfo["ServerVersion"],
        },
      },
      results,
    };

    console.log(JSON.stringify(report, undefined, 4));
  });

function generateReport(title: string, data: Array<IResult>) {
  console.log(`\n${title}`);
  console.table(
    data
      .map((value) => {
        return {
          ...value,
          passesPerSecond: value.passes / value.duration / value.threads,
        };
      })
      .sort((a, b) => b.passesPerSecond - a.passesPerSecond)
  );
  console.log("");
}
