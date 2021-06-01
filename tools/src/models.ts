import * as si from "systeminformation";

export interface Result {
  implementation: string;
  solution: string;
  label: string;
  passes: number;
  duration: number;
  threads: number;
  tags: { [key: string]: string };
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

export interface Report {
  version: string;
  metadata: { [key: string]: string | number };
  machine: {
    cpu: si.Systeminformation.CpuData;
    os: si.Systeminformation.OsData;
    system: si.Systeminformation.SystemData;
    docker: DockerInfo;
  };
  results: Result[];
}
