FROM mcr.microsoft.com/dotnet/sdk:5.0-focal AS build
WORKDIR /src

COPY *.fsproj /src
RUN dotnet restore

COPY *.fs /src
RUN dotnet publish -c Release -o /publish --no-restore

FROM mcr.microsoft.com/dotnet/runtime:5.0-focal
WORKDIR /app
COPY --from=build /publish .

ENTRYPOINT ["/app/PrimeSieveFsharp_Recursion"]