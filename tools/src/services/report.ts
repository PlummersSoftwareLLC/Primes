import * as si from "systeminformation";
import Docker from "dockerode";

import { Report } from "../models";

export default class ReportService {
  public async createReport(results: any[]): Promise<Report> {
    const docker = new Docker();
    const dockerInfo = await docker.info();

    const report: Report = {
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

    return report;
  }

  public saveReport(report: Report, path: string) {
    const content = JSON.stringify(report, undefined, 4);
  }
}
