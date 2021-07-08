import glob from 'glob';
import path from 'path';
import fs from 'fs';

import { Command } from 'commander';

import ReportService from '../services/report';
import DockerService from '../services/docker';

import { Result } from '../models';
import FormatterFactory from '../formatter_factory';

const reportService = new ReportService();
const dockerService = new DockerService();

export const command = new Command('benchmark')
  .requiredOption('-d, --directory <directory>', 'Implementation directory')
  .option('-f, --formatter <type>', 'Output formatter', 'table')
  .action(async (args) => {
    const directory = path.resolve(args.directory as string);

    // Making sure the outputs directory exists
    if (!fs.existsSync(directory)) {
      console.error(`Directory ${directory} does not exist!`);
      return;
    }

    // Search for all Dockerfiles within the base directory
    const files = glob.sync(path.join(directory, '**/Dockerfile'));
    if (!files.length) {
      console.error('No implementations have been found!');
      return;
    }

    const results = new Array<Result>();
    files.forEach((file) => {
      const dirname = path.dirname(file);
      const [implementation, solution] = dirname.split(path.sep).slice(-2);
      const imageName = `${implementation}_${solution}`.toLocaleLowerCase();

      // Build the Docker image for the current solution
      try {
        console.log(`[${implementation}][${solution}] Building ...`);
        dockerService.buildImage(dirname, imageName);
      } catch (e) {
        console.error('Failed building solution!');
        return;
      }

      // Run the benchmark container and retrieve output from it
      try {
        console.log(`[${implementation}][${solution}] Running ...`);
        const output = dockerService.runContainer(imageName);

        console.log(`[${implementation}][${solution}] Parsing output ...`);
        const localResults = reportService.parseOutput(
          output,
          implementation,
          solution
        );
        results.push(...localResults);
      } catch (e) {
        console.error('Error running solution!');
        return;
      }
    });

    if (!results.length) {
      console.error('No data was collected!');
      return;
    }

    // Convert report to the correct format and print everything out.
    const report = await reportService.createReport(results);
    const formatter = FormatterFactory.getFormatter(args.formatter);
    console.log(formatter.render(report));
  });
