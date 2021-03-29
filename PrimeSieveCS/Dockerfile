FROM mcr.microsoft.com/dotnet/sdk:3.1 AS build

WORKDIR /opt/primes
COPY . .
RUN dotnet build -c Release

FROM mcr.microsoft.com/dotnet/runtime:3.1

WORKDIR /opt/primes
COPY --from=build /opt/primes/bin/Release/netcoreapp3.1/ /opt/primes/
CMD ["/opt/primes/PrimeSieveCS"]
