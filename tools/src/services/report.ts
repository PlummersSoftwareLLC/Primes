import * as si from 'systeminformation';
import Docker from 'dockerode';

import { Report, Result } from '../models';

export default class ReportService {
  public async createReport(results: Result[]): Promise<Report> {
    const docker = new Docker();
    const dockerInfo = await docker.info();

    const cpuInfo = await si.cpu();
    const osInfo = await si.osInfo();
    const systemInfo = await si.system();

    const report: Report = {
      version: '1',
      metadata: {
        date: Math.floor(new Date().getTime() / 1000)
      },
      machine: {
        cpu: {
          manufacturer: cpuInfo.manufacturer,
          brand: cpuInfo.brand,
          vendor: cpuInfo.vendor,
          family: cpuInfo.family,
          model: cpuInfo.model,
          stepping: cpuInfo.stepping,
          revision: cpuInfo.revision,
          voltage: cpuInfo.voltage,
          speed: cpuInfo.speed,
          speedMin: cpuInfo.speedMin,
          speedMax: cpuInfo.speedMax,
          governor: cpuInfo.governor,
          cores: cpuInfo.cores,
          physicalCores: cpuInfo.physicalCores,
          efficiencyCores: cpuInfo.efficiencyCores,
          performanceCores: cpuInfo.performanceCores,
          processors: cpuInfo.processors,
          socket: cpuInfo.socket,
          flags: cpuInfo.flags,
          virtualization: cpuInfo.virtualization,
          cache: cpuInfo.cache
        },
        os: {
          platform: osInfo.platform,
          distro: osInfo.distro,
          release: osInfo.release,
          codename: osInfo.codename,
          kernel: osInfo.kernel,
          arch: osInfo.arch,
          codepage: osInfo.codepage,
          logofile: osInfo.logofile,
          build: osInfo.build,
          servicepack: osInfo.servicepack,
          uefi: osInfo.uefi,
          hypervizor: osInfo.hypervizor,
          remoteSession: osInfo.remoteSession
        },
        system: {
          manufacturer: systemInfo.manufacturer,
          model: systemInfo.model,
          version: systemInfo.version,
          sku: systemInfo.sku,
          virtual: systemInfo.virtual,
          virtualHost: systemInfo.virtualHost,
          raspberry: systemInfo.raspberry
        },
        docker: {
          kernelVersion: dockerInfo['KernelVersion'],
          operatingSystem: dockerInfo['OperatingSystem'],
          osVersion: dockerInfo['OSVersion'],
          osType: dockerInfo['OSType'],
          architecture: dockerInfo['Architecture'],
          ncpu: dockerInfo['NCPU'],
          memTotal: dockerInfo['MemTotal'],
          serverVersion: dockerInfo['ServerVersion']
        }
      },
      results
    };

    return report;
  }

  public parseOutput(
    content: string,
    implementation: string,
    solution: string
  ): Array<Result> {
    return content
      .split(/\r?\n/)
      .map((line) => {
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

        return {
          implementation: implementation.toLowerCase().replace('prime', ''),
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
        } as Result;
      })
      .filter((item): item is Result => !!item);
  }

  public saveReport(report: Report): string {
    const content = JSON.stringify(report, undefined, 4);

    return content;
  }
}
