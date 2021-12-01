FROM mcr.microsoft.com/powershell:7.1.3-ubuntu-20.04

WORKDIR /opt/primes

COPY PrimePowerShell.ps1 .

ENTRYPOINT ["pwsh", "-NoLogo", "-NoProfile", "-c ", "& (Join-Path (Get-Location) PrimePowerShell.ps1) | Out-Null"]