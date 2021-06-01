# Benchmarks

This document describes how to build and run all benchmarks (solutions) for different operating systems.

## Table of contents

1. [General working mechanism](#general-working-mechanism)
2. [Linux](#linux)
3. [Windows](#windows)
4. [macOS](#macos)
5. [Example output](#example-output)

## What operating system to use?

A Unix-like operating system is the preferred operating system to run this benchmark. Linux and macOS are Unix-like operating systems. This will result in the best performance because the benchmark is based on Unix technologies. Running with Windows or other operating systems is possible but will always require some extra layer of virtualization that impact the performance. Running the benchmark with Windows can have a significant impact on the performance, up to 50%.

## General working mechanism

The main directory contains a file named "```makefile```". This makefile is a linux shell script written in bash. This script has the following basic flow:

- A temp directory is created to write the output of the benchmarks to
- All solution directories that have a Dockerfile are processed one by one
- If a solution requires a spefic hardware architecture that does not match your hardware then this solution is skipped. For example, ```PrimeAssembly\solution_1``` contains a file named ```arch-arm64```. If you are running on different hardware then this solution is skipped
- Each solution is build and run with the Dockerfile that is provided in the solution directory.
- After all solutions have been run a summary report is created in the temp directory that was created in the first step.

## Linux

### Linux run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#linux-installation-and-prerequisites)
2. Open a terminal.
3. ```git clone https://github.com/PlummersSoftwareLLC/Primes.git```
4. ```cd Primes```
5. ```make```
6. The build and run of all benchmarks will now start. Note that this will take a while, so please wait untill this task is complete.
7. Once complete a report is written to the terminal, see [example output](#example-output).

### Linux installation and prerequisites

The following software must be installed:

- /bin/bash
- make
- git
- docker

The details for each required software are described below.

#### **/bin/bash**

Most Linux distributions include ```/bin/bash``` by default. If this is not the case for your distribution then please check your distribution specific documentation on how to install.

#### **make**

Most Linux distributions include ```make``` by default. If this is not the case for your distribution then please check your distribution specific documentation on how to install. Below are some examples:

Debian based distributions:

```bash
sudo apt-get install make
```

Fedora based distributions:

```bash
sudo dnf make
```

#### **Git for Linux**

Most Linux distributions include ```git``` by default. If this is not the case for your distribution then please check your distribution specific documentation. Below are some examples:

Debian based distributions:

```bash
sudo apt-get install git
```

Fedora based distributions:

```bash
sudo dnf git
```

#### **Docker for Linux**

For the installation of Docker follow the instuctions as described in <https://docs.docker.com/engine/install/>.

## Windows

The preferred method to run the benchmark is with native Linux. Running the benchmark with Windows can have a significant impact on the performance, up to 50%.

### Windows run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#windows-installation-and-prerequisites)
2. Open a **bash** terminal.
3. ```git clone https://github.com/PlummersSoftwareLLC/Primes.git```
4. ```cd Primes```
5. ```make```
6. The build and run of all benchmarks will now start. Note that this will take a while, so please wait until this task is complete.
7. Once complete a report is written to the terminal, see [example output](#example-output).

### Windows installation and prerequisites

The following software must be installed:

- Windows Subsystem for Linux 2 (WSL2)
- make
- git
- Docker Desktop

#### **Install WSL2**

Windows Subsystem for Linux (WSL) is a compatibility layer for running Linux binary executables natively on Windows. For the installation of WSL follow the instructions as described in <https://docs.microsoft.com/en-us/windows/wsl/install-win10>. Make sure to enable WSL2. Use Ubuntu 18.04 as the default distribution to use.

#### **Install make and git for Windows**

Take the following steps to install make for Windows inside the Ubuntu distribution:

1. Open a bash terminal
2. ```sudo apt-get install make```
3. ```sudo apt-get install git```

#### **Docker Desktop for Windows**

Make sure WSL2 is installed before you start with the installation of Docker.
For the installation of Docker follow the instuctions as described in <https://docs.docker.com/docker-for-windows/install/>.

## macOS

### macOS run instructions

Take the following steps to run all benchmarks:

1. Make sure you have installed the [required software](#mac-os-installation-and-prerequisites)
2. Open a terminal.
3. ```git clone https://github.com/PlummersSoftwareLLC/Primes.git```
4. ```cd Primes```
5. ```make```
6. The build and run of all benchmarks will now start. Note that this will take a while, so please wait until this task is complete.
7. Once complete a report is written to the terminal, see [example output](#example-output).

### macOS installation and prerequisites

The following software must be installed:

- xcode-select (make and git)
- Docker Desktop

The details for each required software are described below.

#### **xcode-select (make and git) for macOS**

Take the following steps to install make and git on macOS:

1. Open a terminal
2. ```xcode-select --install```
3. A popup window appears, select install
4. In the next window accept the terms
5. The installation now starts, wait until it completes

#### **Docker Desktop for macOS**

For the installation of Docker follow the instuctions as described in <https://docs.docker.com/docker-for-mac/install/>.

## Example output

Below is an example of the benchmark output.

```bash
Generating Software Drag Race (single-threaded)
         implementation    solution                              label  passes    duration  threads  passes_per_second
1                primec  solution_2  danielspaangberg_5760of30030_owrb   13471    5.014557        1        2686.378877
2                primec  solution_2    danielspaangberg_480of2310_owrb   11037    5.016852        1        2199.985170
3                primec  solution_1                        mckoss-c830   10716    5.000000        1        2143.200000
... etc

Generating Software Drag Race (multi-threaded)
   implementation    solution                              label  passes  duration  threads  passes_per_second
1          primec  solution_2  danielspaangberg_5760of30030_epar   30572  5.015792        4        1523.787270
2          primec  solution_2    danielspaangberg_480of2310_epar   28214  5.030535        4        1402.137148
3          primec  solution_2      danielspaangberg_48of210_epar   20572  5.001535        4        1028.284317
... etc

Output files available in /tmp/tmp.kXe95aYI6R
```
