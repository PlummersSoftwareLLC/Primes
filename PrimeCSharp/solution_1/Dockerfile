#See https://aka.ms/containerfastmode to understand how Visual Studio uses this Dockerfile to build your images for faster debugging.

FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["PrimeCSharp.csproj", "."]
RUN dotnet restore "./PrimeCSharp.csproj"
COPY . .
RUN dotnet build "PrimeCSharp.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "PrimeCSharp.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "PrimeCSharp.dll", "--all"]