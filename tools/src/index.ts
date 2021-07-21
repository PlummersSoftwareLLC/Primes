import { Command } from "commander";

import * as BenchmarkCommand from "./commands/benchmark";

const program = new Command();
program.version(process.env.npm_package_version || "");
program.addCommand(BenchmarkCommand.command);

program.parse(process.argv);
