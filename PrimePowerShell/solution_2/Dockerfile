FROM mcr.microsoft.com/powershell

WORKDIR /opt/primes

COPY PrimePowerShell.ps1 ./

ENTRYPOINT ["pwsh", "-NoLogo", "-NoProfile", "-c ", "& (Join-Path (Get-Location) PrimePowerShell.ps1) | Out-Null"]