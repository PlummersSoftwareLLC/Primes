# Benchmarks

This document describes how to built and run all benchmarks (solutions) for different operating systems.

## Table of contents

1. [General working mechanism](#general-working-mechanism)
2. [Linux](#linux)
3. [Windows](#windows)
4. [macOS](#macos)
5. [Example output](#example-output)
6. [Running a single benchmark](#running-a-single-benchmark)

## What operating system to use?

A Unix-like operating system is the preferred operating system to run this benchmark. Linux and macOS are Unix-like operating systems. This will result in the best performance because the benchmark is based on Unix technologies. Running with Windows or other operating systems is possible but will always require some extra layer of virtualization that impact the performance. Running the benchmark with Windows can have a significant impact on the performance, up to 50%.

## General working mechanism

The main directory contains a file named "`Makefile`". This Makefile implements the following basic flow:

- A temp directory is created to write the output of the benchmarks to
- All solution directories that have a Dockerfile are processed one by one
- If a solution requires a specific hardware architecture that does not match your hardware then this solution is skipped. For example, `PrimeAssembly\solution_1` contains a file named `arch-arm64`. If you are running on different hardware then this solution is skipped
- Each solution is built and run with the Dockerfile that is provided in the solution directory.
- After all solutions have been run a summary report is created in the temp directory that was created in the first step.

## Linux

### Linux run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#linux-installation-and-prerequisites)
2. Open a terminal.
3. Make sure Node.js is added to the path. `export PATH=/path/to/node/bin:$PATH`.
   For example: `export PATH=~/node-v14.17.0-linux-x64/bin:$PATH`
4. `git clone https://github.com/PlummersSoftwareLLC/Primes.git`
5. `cd Primes`
6. `make`
7. All benchmarks will now be build and run one by one. Note that this will take a while, so please wait until this task is complete. The build will only happen the first time. On subsequent runs, only solutions that have changed since the last benchmark run will be rebuilt, which makes those runs a lot faster.
8. Once complete a report is written to the terminal, see [example output](#example-output).

### Linux installation and prerequisites

The following software must be installed:

- /bin/bash
- make
- git
- Node.js
- Docker

The details for each required software are described below.

#### /bin/bash

Most Linux distributions include `/bin/bash` by default. If this is not the case for your distribution then please check your distribution specific documentation on how to install.

#### make

Most Linux distributions include `make` by default. If this is not the case for your distribution then please check your distribution specific documentation on how to install. Below are some examples:

Debian based distributions:

```bash
sudo apt-get install make
```

Fedora based distributions:

```bash
sudo dnf make
```

#### Git for Linux

Most Linux distributions include `git` by default. If this is not the case for your distribution then please check your distribution specific documentation. Below are some examples:

Debian based distributions:

```bash
sudo apt-get install git
```

Fedora based distributions:

```bash
sudo dnf git
```

#### Node.js for Linux

The benchmarks makes use of Node.js to create a report. This requires Node.js version 14.17.0 or higher. The Node.js/npm versions that are included in many OS-provided package managers are too old. This means that it will generally be required to download and install an appropriate version of Node.js from <https://nodejs.org/en/download/>.

Take the following steps to install Node.js on Linux:

1. Navigate to <https://nodejs.org/en/download/>
2. Download the Linux Binaries, depending on your CPU architecture
3. Unzip the downloaded file: `tar -xf /path/to/downloaded/file.xz>`
   For example: `tar -xf ~/Downloads/node-v14.17.0-linux-x64.tar.xz`
4. The Node.js binaries are now extracted to a directory. This directory is referred to as `/path/to/node` in the run instructions.

#### Docker for Linux

For the installation of Docker first follow the instructions as described in <https://docs.docker.com/engine/install/>.

After the installation you need to enable docker as non-root user. Take the following steps:

1. `sudo groupadd docker`
2. `sudo usermod -aG docker $USER`
3. Log out and log back in so that your group membership is re-evaluated.
4. Verify that you can run docker:

   ```bash
   docker run hello-world
   ```

 The docker website describes more [post-installation steps for linux](https://docs.docker.com/engine/install/linux-postinstall/). These additional steps are optional for running the benchmarks.

## Windows

The preferred method to run the benchmark is with native Linux. Running the benchmark with Windows can have a significant impact on the performance, up to 50%.

### Windows run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#windows-installation-and-prerequisites)
2. Make sure Node.js is added to the path. `export PATH=/path/to/node/bin:$PATH`.
   For example: `export PATH=~/node-v14.17.0-linux-x64/bin:$PATH`
3. Open an **Ubuntu** terminal (  Start --> Ubuntu 20.04 LST)
4. `git clone https://github.com/PlummersSoftwareLLC/Primes.git`
5. `cd Primes`
6. `make`
7. All benchmarks will now be build and run one by one. Note that this will take a while, so please wait until this task is complete. The build will only happen the first time. On subsequent runs, only solutions that have changed since the last benchmark run will be rebuilt, which makes those runs a lot faster.
8. Once complete a report is written to the terminal, see [example output](#example-output).

### Windows installation and prerequisites

The following software must be installed:

- Windows Subsystem for Linux 2 (WSL2) with the Ubuntu 20.04 LST distribution
- make
- Node.js
- Docker Desktop for Windows

WSL2 has special [hardware requirements](https://docs.microsoft.com/en-us/windows-server/virtualization/hyper-v/system-requirements-for-hyper-v-on-windows).

#### Install WSL2

Windows Subsystem for Linux (WSL) is a compatibility layer for running Linux binary executables natively on Windows. For the installation of WSL2 follow the instructions as described in <https://docs.microsoft.com/en-us/windows/wsl/install-win10>. Make sure to enable WSL2. Use Ubuntu 20.04 LSTas the default distribution to use. Start the Ubuntu app once to create a user, as described in the above instructions.

#### Install make inside the Ubuntu distribution

Take the following steps to install make inside the Ubuntu distribution:

1. Open an Ubuntu terminal
2. `sudo apt-get install make`

#### Install Node.js inside the Ubuntu distribution

The benchmarks makes use of Node.js to create a report. This requires Node.js version 14.17.0 or higher. The Node.js/npm versions that is included in Ubuntu 20.04 is too old. This means that it will be required to download and install an appropriate version of Node.js from <https://nodejs.org/en/download/>.

Take the following steps to install Node.js inside the Ubuntu distribution:

1. Navigate to <https://nodejs.org/en/download/>
2. Download the Linux Binaries, depending on your CPU architecture
3. Copy the downloaded file inside the Ubuntu distribution home directory. For example:
   `\\wsl$\Ubuntu-20.04\home\user`
4. Open an Ubuntu terminal
5. Unzip the downloaded file: `tar -xf ~/file.xz>`. For example: `tar -xf ~/node-v14.17.0-linux-x64.tar.xz`
6. The Node.js binaries are now extracted to a directory. This directory is referred to as `/path/to/node` in the run instructions.

#### Docker Desktop for Windows

Take the following steps to install Docker Desktop for Windows with the WSL2 backend:

1. Make sure [WSL2 is installed](#install-wsl2) with the Ubuntu 20.04 LST before you start with the installation of Docker.
2. For the installation of Docker follow the instructions as described in <https://docs.docker.com/docker-for-windows/install/#install-docker-desktop-on-windows>. At step 2 select **install required Windows components for WSl2**.
3. Once Docker is installed, start Docker Desktop
4. In Docker Desktop navigate to **Settings --> Resources --> WSL Integration**
5. Check Ubuntu 18.04 and click Apply & Restart
6. Open an **Ubuntu** terminal (  Start --> Ubuntu 20.04 LST)
7. ```sudo groupadd docker```
8. ```sudo usermod -aG docker $USER```
9. Close the Ubuntu window
10. Open an **Ubuntu** terminal (  Start --> Ubuntu 20.04 LST)
11. Verify that you can run docker inside the Ubuntu WSL2 container:

   ```bash
   docker run hello-world
   ```

   This command downloads a test image and runs it in a container. When the container runs, it prints an informational message and exits.

## macOS

### macOS run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#macos-installation-and-prerequisites)
2. Open a terminal.
3. `git clone https://github.com/PlummersSoftwareLLC/Primes.git`
4. `cd Primes`
5. `make`
6. All benchmarks will now be build and run one by one. Note that this will take a while, so please wait until this task is complete. The build will only happen the first time. On subsequent runs, only solutions that have changed since the last benchmark run will be rebuilt, which makes those runs a lot faster.
7. Once complete a report is written to the terminal, see [example output](#example-output).

### macOS installation and prerequisites

The following software must be installed:

- xcode-select (make and git)
- nodejs
- Docker Desktop for macOS

The details for each required software are described below.

#### xcode-select (make and git) for macOS

Take the following steps to install make and git on macOS:

1. Open a terminal
2. `xcode-select --install`
3. A popup window appears, select install
4. In the next window accept the terms
5. The installation now starts, wait until it completes

#### Node.js for macOs

The benchmarks makes use of Node.js to create a report. This requires Node.js version 14.17.0 or higher. The Node.js/npm versions that are included in many OS-provided package managers are too old. This means that it will generally be required to download and install an appropriate version of Node.js from <https://nodejs.org/en/download/>.

Take the following steps to install Node.js on macOS:

1. Navigate to <https://nodejs.org/en/download/>
2. Download the macOS Installer (.pkg)
3. Start the installer
4. Click Continue
5. Click Continue, accept the licence agreement
6. Click Install
7. Provide your password to confirm
8. The installation now starts
9. Close the installer

#### Docker Desktop for macOS

For the installation of Docker follow the instructions as described in <https://docs.docker.com/docker-for-mac/install/>.

## Example output

Below is an example of the benchmark output.

```bash
                                                                   Single-threaded                                                                    
┌───────┬────────────────┬──────────┬───────────────────────────────────┬────────┬───────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                             │ Passes │ Duration  │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────┼────────┼───────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ c              │ 2        │ danielspaangberg_5760of30030_owrb │ 12451  │  5.00006  │    1    │   wheel   │   yes    │ 1    │  2490.17012   │
│   2   │ c              │ 1        │ mckoss-c830                       │  9774  │  5.00000  │    1    │   wheel   │   yes    │ 1    │  1954.80000   │
│   3   │ c              │ 2        │ danielspaangberg_480of2310_owrb   │  9458  │  5.00015  │    1    │   wheel   │   yes    │ 1    │  1891.54325   │

etc

│  72   │ powershell     │ 2        │ crowbar27_ps2                     │   3    │  5.78784  │    1    │   base    │   yes    │ 1    │    0.51833    │
│  73   │ haxe           │ 1        │ TayIorRobinson_Haxe_Python        │   4    │ 12.33397  │    1    │   base    │   yes    │      │    0.32431    │
│  74   │ powershell     │ 1        │ crowbar27_ps1                     │   1    │ 150.03900 │    1    │   base    │   yes    │ 1    │    0.00666    │
└───────┴────────────────┴──────────┴───────────────────────────────────┴────────┴───────────┴─────────┴───────────┴──────────┴──────┴───────────────┘

                                                                   Multi-threaded                                                                    
┌───────┬────────────────┬──────────┬───────────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                             │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼───────────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ c              │ 2        │ danielspaangberg_5760of30030_epar │ 24298  │ 5.00118  │    4    │   wheel   │   yes    │ 1    │  1214.61335   │
│   2   │ c              │ 2        │ danielspaangberg_480of2310_epar   │ 21729  │ 5.00549  │    4    │   wheel   │   yes    │ 1    │  1085.25774   │
│   3   │ c              │ 2        │ danielspaangberg_48of210_epar     │ 17161  │ 5.00735  │    4    │   wheel   │   yes    │ 1    │   856.78966   │

etc

│  13   │ c              │ 2        │ danielspaangberg_1of2_par         │  2943  │ 5.00040  │    4    │   base    │   yes    │ 1    │   147.13829   │
│  14   │ csharp         │ 1        │ kinematics_pool6p                 │  2510  │ 5.00935  │    4    │           │          │      │   125.26575   │
│  15   │ csharp         │ 1        │ kinematics_rawp                   │  1840  │ 5.00896  │    4    │           │          │      │   91.83543    │
└───────┴────────────────┴──────────┴───────────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Running a single benchmark

Running te following command:

```bash
make one SOLUTION=PrimesCrystal/solution1
```

should return something like:

```bash
❯ make one SOLUTION=PrimeCrystal/solution_1
[*] Running primecrystal-solution_1
Passes: 4232 Time: 5.000194 Avg: 0.001182 Limit: 1000000 Count1: 78498 Count2: 78498 Valid: true
marghidanu;4232;5.000194;1;algorithm=base,faithful=yes,bits=1

added 196 packages, and audited 197 packages in 3s

33 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities

> primes@0.1.0 start
> ts-node ./src/index.ts "report" "-d" "/var/folders/cp/szjr187549j7zps8zqzw_n940000gn/T/tmp.cQQFnuZm" "else" "echo" "Not specified!"


                                                       Single-threaded                                                        
┌───────┬────────────────┬──────────┬────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label      │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ crystal        │ 1        │ marghidanu │  4232  │ 5.00019  │    1    │   base    │   yes    │ 1    │   846.36716   │
└───────┴────────────────┴──────────┴────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```
