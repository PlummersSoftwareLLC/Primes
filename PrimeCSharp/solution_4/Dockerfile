#See https://aka.ms/containerfastmode to understand how Visual Studio uses this Dockerfile to build your images for faster debugging.

FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["FastSieve.csproj", "."]
COPY ["Program.cs", "."]
COPY ["Sieve.cs", "."]
RUN dotnet restore "./FastSieve.csproj"
WORKDIR "/src/."
RUN dotnet build "FastSieve.csproj" -c Release -o /app/publish

#Publish step intentionally removed as I got better perf w/o it
FROM base AS final
WORKDIR /app
COPY --from=build /app/publish .
ENTRYPOINT ["dotnet", "FastSieve.dll"]