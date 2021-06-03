export interface Result {
  implementation: string;
  solution: string;
  label: string;
  passes: number;
  duration: number;
  threads: number;
  tags: { [key: string]: string };
}

export interface Report {
  version: string;
  metadata: { [key: string]: string | number };
  machine: {
    cpu: CpuInfo;
    os: OsInfo;
    system: SystemInfo;
    docker: DockerInfo;
  };
  results: Result[];
}

export interface CpuInfo {
  manufacturer: string;
  brand: string;
  vendor: string;
  family: string;
  model: string;
  stepping: string;
  revision: string;
  voltage: string;
  speed: number;
  speedMin: number;
  speedMax: number;
  governor: string;
  cores: number;
  physicalCores: number;
  efficiencyCores?: number;
  performanceCores?: number;
  processors: number;
  socket: string;
  flags: string;
  virtualization: boolean;
  cache: {
    l1d: number;
    l1i: number;
    l2: number;
    l3: number;
  };
}

export interface OsInfo {
  platform: string;
  distro: string;
  release: string;
  codename: string;
  kernel: string;
  arch: string;
  codepage: string;
  logofile: string;
  build: string;
  servicepack: string;
  uefi: boolean;
  hypervizor?: boolean;
  remoteSession?: boolean;
}

export interface SystemInfo {
  manufacturer: string;
  model: string;
  version: string;
  sku: string;
  virtual: boolean;
  virtualHost?: string;
  raspberry?: {
    manufacturer: string;
    processor: string;
    type: string;
    revision: string;
  };
}

export interface DockerInfo {
  kernelVersion: string;
  operatingSystem: string;
  osVersion: string;
  osType: string;
  architecture: string;
  ncpu: number;
  memTotal: number;
  serverVersion: string;
}
