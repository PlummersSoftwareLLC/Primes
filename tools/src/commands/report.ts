import fs from "fs";
import path from "path";
import glob from "glob";

import { Command } from "commander";

import { Result } from "../models";
import ReportService from "../services/report";
import ResultService from "../services/result";

const reportService = new ReportService();
const resultService = new ResultService();

export const command = new Command("report")
  .requiredOption("-d, --directory <directory>", "Results directory")
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

      const localResults = new Array<Result>();
      fs.readFileSync(file, "utf-8")
        .split(/\r?\n/)
        .forEach((line) => {
          const match = line
            .trim()
            .match(
              /^(?<label>.+)\s*;\s*(?<passes>\d+)\s*;\s*(?<duration>\d+([.]\d+)?)\s*;\s*(?<threads>\d+)(;(?<extra>.+))?$/
            );

          // Moving to the next record if the line doesn't match
          if (!match) return;

          // Extract tags into an object and add it to the result.
          const tags: { [key: string]: string } = {};
          if (match.groups!["extra"]) {
            const tagsRegex =
              /(?<key>[a-zA-Z0-9\-_]+)=(?<value>[a-zA-Z0-9_\-\+\."':]+)/gim;

            let tagMatch;
            while ((tagMatch = tagsRegex.exec(match.groups!["extra"]))) {
              tags[tagMatch.groups!["key"]] = tagMatch.groups!["value"];
            }
          }

          localResults.push({
            implementation: implementation.replace("prime", ""),
            solution: solution.replace("solution_", ""),
            label: match.groups!["label"],
            passes: +match.groups!["passes"],
            duration: +match.groups!["duration"],
            threads: +match.groups!["threads"],
            tags,
          } as Result);
        });

      if (!localResults.length)
        console.warn(`No valid output for ${file}. Skipping ...`);

      return localResults;
    });

    if (!results.length) {
      console.error("No data was collected!");
      return;
    }

    console.log("");
    resultService.printResults(
      "Single-threaded",
      results.filter((value) => value.threads === 1)
    );

    console.log("");
    resultService.printResults(
      "Multi-threaded",
      results.filter((result) => result.threads > 1)
    );

    const report = await reportService.createReport(results);
    console.log(JSON.stringify(report, undefined, 4));
  });
