FROM mcr.microsoft.com/dotnet/sdk:10.0-noble

WORKDIR /opt/primes

COPY PrimePowerShell.ps1 .

ENTRYPOINT ["pwsh", "-NoLogo", "-NoProfile", "-c ", "& (Join-Path (Get-Location) PrimePowerShell.ps1) | Out-Null"]