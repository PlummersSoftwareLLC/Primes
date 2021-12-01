FROM mcr.microsoft.com/dotnet/sdk:5.0-focal AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY PrimeSieveFsharp_Unpeeling.fsproj /source
RUN dotnet restore

# copy and publish app and libraries
COPY *.fs /source
RUN dotnet publish -c=Release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime:5.0-focal
WORKDIR /app
COPY --from=build /app .
ENTRYPOINT ["/app/PrimeSieveFsharp_Unpeeling"]
