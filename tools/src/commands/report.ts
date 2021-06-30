import fs from 'fs';
import path from 'path';
import glob from 'glob';

import { Command } from 'commander';

import { Result } from '../models';
import ReportService from '../services/report';
import ResultService from '../services/result';

const reportService = new ReportService();
const resultService = new ResultService();

export const command = new Command('report')
  .requiredOption('-d, --directory <directory>', 'Results directory')
  .option('--json', 'Output JSON report')
  .action(async (args) => {
    const directory = path.resolve(args.directory as string);

    // Making sure the outputs directory exists
    if (!fs.existsSync(directory)) {
      console.error(`Directory ${directory} does not exist!`);
      return;
    }

    // Iterating over the files and extracting metadata from name
    const results = glob.sync(path.join(directory, '*.out')).flatMap((file) => {
      const [implementation, solution] = path
        .basename(file, '.out')
        .split('-', 2);

      // Parsing lines from output
      const localResults = new Array<Result>();
      fs.readFileSync(file, 'utf-8')
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
          if (match.groups?.['extra']) {
            const tagsRegex =
              /(?<key>[a-zA-Z0-9\-_]{1,32})=(?<value>[a-zA-Z0-9_\-+."':]{1,32})/gim;

            let tagMatch;
            while ((tagMatch = tagsRegex.exec(match.groups?.['extra']))) {
              if (!tagMatch.groups?.['key']) continue;
              tags[tagMatch.groups?.['key']] = tagMatch.groups?.['value'] || '';
            }
          }

          localResults.push({
            implementation: implementation.replace('prime', ''),
            solution: solution.replace('solution_', ''),
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            label: match.groups!['label'],
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            passes: +match.groups!['passes'],
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            duration: +match.groups!['duration'],
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            threads: +match.groups!['threads'],
            tags
          } as Result);
        });

      if (!localResults.length)
        console.warn(`No valid output for ${file}. Skipping ...`);

      return localResults;
    });

    if (!results.length) {
      console.error('No data was collected!');
      return;
    }

    if (args.json) {
      const report = await reportService.createReport(results);
      console.log(JSON.stringify(report, undefined, 4));

      return;
    }

    const singleThreadedResults = results.filter((value) => value.threads === 1);
    if (singleThreadedResults.length > 0) {
      console.log('');
      resultService.printResults('Single-threaded', singleThreadedResults);
    }

    const multiThreadedResults = results.filter((result) => result.threads > 1);
    if (multiThreadedResults.length > 0) {
      console.log('');
      resultService.printResults('Multi-threaded', multiThreadedResults);
    }
  });
