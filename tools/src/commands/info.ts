import { Command } from "commander";

import * as si from "systeminformation";

interface CPUInfo {
  manufacturer: string;
  brand: string;
  vendor: string;
  family: string;
  model: string;
  revision: string;
  speed: number;
  cores: number;
  physicalCores: number;
  processors: number;
  socket: string;
  flags: string;
  virtualization: boolean;
  efficiencyCores: number;
  performanceCores: number;
}

interface OSInfo {
  platform: string;
  distro: string;
  release: string;
  codename: string;
  kernel: string;
  arch: string;
  build: string;
  servicepack: string;
  uefi: boolean;
}

interface SystemInfo {
  manufacturer: string;
  model: string;
  version: string;
  virtual: boolean;
}

export const command = new Command("info").action(async () => {
  const cpuInfo = await si.cpu();
  console.log(cpuInfo);

  const osInfo = await si.osInfo();
  console.log(osInfo);

  const systemInfo = await si.system();
  console.log(systemInfo);
});
