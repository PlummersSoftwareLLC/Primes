#See https://aka.ms/containerfastmode to understand how Visual Studio uses this Dockerfile to build your images for faster debugging.

FROM mcr.microsoft.com/dotnet/sdk:6.0-focal AS build
WORKDIR /source

# copy csproj and restore dependencies
COPY *.csproj ./
RUN dotnet restore

# copy and publish app and libraries
COPY . .
RUN dotnet publish -c release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime:6.0-focal AS publish
WORKDIR /app
COPY --from=build /app .

# default entry point and command argument
ENTRYPOINT ["./PrimeCSharp"]
CMD ["--demo"]

