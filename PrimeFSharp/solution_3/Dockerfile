FROM mcr.microsoft.com/dotnet/sdk:10.0-noble AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY PrimeSieveFsharp_Recursion.fsproj /source
RUN dotnet restore

# copy and publish app and libraries
COPY *.fs /source
RUN dotnet publish -c release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime:10.0-noble
WORKDIR /app
COPY --from=build /app .
ENTRYPOINT ["/app/PrimeSieveFsharp_Recursion"]