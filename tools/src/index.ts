import { Command } from "commander";

import * as ReportCommand from "./commands/report";
import * as InfoCommand from "./commands/info";

const program = new Command();
program.version(process.env.npm_package_version || "");

program.addCommand(ReportCommand.command);
program.addCommand(InfoCommand.command);

program.parse(process.argv);
