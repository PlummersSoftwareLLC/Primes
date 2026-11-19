FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY PrimeSieveCS.csproj ./
RUN dotnet restore

# copy the sources, generate the marking kernels (Markers.Generated.cs), publish
COPY *.cs ./
COPY tools/ ./tools/
RUN dotnet run --file tools/GenMarkers.cs -- Markers.Generated.cs \
    && dotnet publish PrimeSieveCS.csproj -c Release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/runtime:10.0
WORKDIR /app
COPY --from=build /app .
ENTRYPOINT ["/app/PrimeSieveCS"]
