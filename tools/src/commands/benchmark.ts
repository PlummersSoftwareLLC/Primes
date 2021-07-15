import glob from 'glob';
import path from 'path';
import fs from 'fs';

import { Command } from 'commander';
import { uname } from 'node-uname';
import winston from 'winston';

import ReportService from '../services/report';
import DockerService from '../services/docker';
import { Result } from '../models';
import FormatterFactory from '../formatter_factory';

const reportService = new ReportService();
const dockerService = new DockerService();
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.printf(({ level, message }) => `${level}: ${message}`),
  transports: [new winston.transports.Console()]
});

const ARCHITECTURES: { [arch: string]: string } = {
  x86_64: 'amd64',
  aarch64: 'arm64',
  arm64: 'arm64'
};

export const command = new Command('benchmark')
  .requiredOption('-d, --directory <directory>', 'Implementation directory')
  .option('-f, --formatter <type>', 'Output formatter', 'table')
  .action(async (args) => {
    const directory = path.resolve(args.directory as string);

    // Making sure the outputs directory exists
    if (!fs.existsSync(directory)) {
      logger.error(`Directory ${directory} does not exist!`);
      return;
    }

    // Search for all Dockerfiles within the base directory
    const files = glob.sync(path.join(directory, '**/Dockerfile'));
    if (!files.length) {
      logger.error('No implementations have been found!');
      return;
    }

    // Determine architecture
    const architecture = ARCHITECTURES[uname().machine] || 'amd64';
    logger.info(`Detected architecture: ${architecture}`);

    // Processing files
    const results = new Array<Result>();
    files.forEach((file) => {
      const solutionDirectory = path.dirname(file);
      const [implementation, solution] = solutionDirectory
        .split(path.sep)
        .slice(-2);
      const imageName = `${implementation}_${solution}`.toLocaleLowerCase();

      // NOTE: If any arch-* files are present then check if the current architecture
      // is present among the files, if not we skip this build...
      const archFiles = glob.sync(path.join(solutionDirectory, 'arch-*'));
      if (archFiles.length) {
        const hasArch = archFiles
          .map((file) => path.basename(file).replace('arch-', ''))
          .includes(architecture);

        if (!hasArch) {
          logger.warn(
            `[${implementation}][${solution}] Skipping due to architecture mismatch!`
          );
          return;
        }
      }

      // Build the Docker image for the current solution
      try {
        logger.info(`[${implementation}][${solution}] Building...`);
        dockerService.buildImage(solutionDirectory, imageName);
      } catch (e) {
        logger.error(
          `[${implementation}][${solution}] Failed building solution!`
        );
        return;
      }

      // Run the benchmark container and retrieve output from it
      let output = '';
      try {
        logger.info(`[${implementation}][${solution}] Running...`);
        output = dockerService.runContainer(imageName);
      } catch (e) {
        logger.warn(
          `[${implementation}][${solution}] Exited with abnormal code: ${e.status}. Results might be partial...`
        );
        output = e.output
          .filter((block: Buffer | null) => block !== null)
          .map((block: Buffer) => block.toString('utf8'))
          .join('');
      } finally {
        const localResults = reportService.parseOutput(
          output,
          implementation,
          solution
        );
        results.push(...localResults);
      }
    });

    if (!results.length) {
      logger.error('No data was collected!');
      return;
    }

    // Convert report to the correct format and print everything out.
    const report = await reportService.createReport(results);
    const formatter = FormatterFactory.getFormatter(args.formatter);
    console.log(formatter.render(report));
  });
