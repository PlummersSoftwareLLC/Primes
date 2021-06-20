import { Command } from "commander";

import * as ReportCommand from "./commands/report";

const program = new Command();
program.version(process.env.npm_package_version || "");
program.addCommand(ReportCommand.command);

program.parse(process.argv);
