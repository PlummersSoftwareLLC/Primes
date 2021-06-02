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

  public saveReport(report: Report): string {
    const content = JSON.stringify(report, undefined, 4);

    return content;
  }
}
