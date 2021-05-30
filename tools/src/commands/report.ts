import * as fs from "fs";
import * as path from "path";
import glob from "glob";

import { Command } from "commander";
import { Table } from "console-table-printer";

interface IResult {
  implementation: string;
  solution: string;
  label: string;
  passes: number;
  duration: number;
  threads: number;
}

function generateReport(title: string, data: Array<IResult>) {
  const table = new Table({ title });

  table.addRows(
    data
      .map((value) => {
        return {
          ...value,
          passesPerSecond: value.passes / value.duration / value.threads,
        };
      })
      .sort((a, b) => b.passesPerSecond - a.passesPerSecond)
      .map((value, index) => {
        return { index: index + 1, ...value };
      })
  );

  table.printTable();
}

export const command = new Command("report")
  .requiredOption("-d, --directory <directory>", "Results directory")
  .action((args) => {
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
                implementation,
                solution,
                label: match.groups!["label"],
                passes: parseInt(match.groups!["passes"], 10),
                duration: parseFloat(match.groups!["duration"]),
                threads: parseInt(match.groups!["threads"], 10),
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
  });
