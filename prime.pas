program prime;

{$mode objfpc}

uses
    fgl, sysutils,
    fileinfo, winpeimagereader;      //7.1.2021 Added to read exe file info
var
  FileVerInfo: TFileVersionInfo;     //7.1.2021 Added to read exe file info

type
    CheckMap = specialize TFPGMap<Integer, Integer>;
    PackedBoolArray = packed array of Boolean;

    PrimeSieve = class
    private
        SieveSize: Integer;
        NotPrimeArray: PackedBoolArray;

    public
        constructor Create(Size: Integer);
        function RunSieve(): PackedBoolArray;
        function CountPrimes(): Int64;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: Integer);
begin
    SieveSize := Size;
    SetLength(NotPrimeArray, (Size + 1) Div 2);
end;

function PrimeSieve.RunSieve(): PackedBoolArray;
var
    Factor: Integer;
    Number: Integer;
    SieveSqrt: Integer;

begin
    SieveSqrt := Trunc(Sqrt(SieveSize));

    Factor := 3;

    while Factor <= SieveSqrt do
    begin
        Number := Factor;
        while Number <= SieveSqrt do
        begin
            if not NotPrimeArray[Number div 2] then
            begin
                Factor := Number;
                break;
            end;

            Number := Number + 2;
        end;

        if Number > SieveSqrt then
            break;

        Number := Factor * Factor;

        while Number <= SieveSize do
        begin
            NotPrimeArray[Number div 2] := True;
            Number := Number + (Factor * 2);
        end;

        Factor := Factor + 2;
    end;

    RunSieve := NotPrimeArray;
end;

function PrimeSieve.CountPrimes(): Int64;
var
    Count: Int64;
    I: Int64;
begin
    Count := 0;
    for I := Low(NotPrimeArray) to High(NotPrimeArray) do
    begin
        if not NotPrimeArray[I] then
            Count := Count + 1;
    end;

    CountPrimes := Count;
end;

function PrimeSieve.ValidateResults(var ReferenceResults: CheckMap): Boolean;
var
    ReferenceValue: LongInt;

begin
    ReferenceValue := 0;

    if ReferenceResults.TryGetData(SieveSize, ReferenceValue) then
       ValidateResults := ReferenceValue = CountPrimes()
    else
       ValidateResults := False;
end;

var
    ReferenceResults: CheckMap;
    StartTickCount, DurationTickCount: QWord;
    PassCount: Int64;
    Sieve: PrimeSieve;

{$R *.res}

begin
    ReferenceResults := CheckMap.Create();
    ReferenceResults.Add(10, 4);
    ReferenceResults.Add(100, 25);
    ReferenceResults.Add(1000, 168);
    ReferenceResults.Add(10000, 1229);
    ReferenceResults.Add(100000, 9592);
    ReferenceResults.Add(1000000, 78498);
    ReferenceResults.Add(10000000, 664579);
    ReferenceResults.Add(100000000, 5761455);

    PassCount := 0;
    StartTickCount := GetTickCount64();                       //Start the timer
    Sieve := nil;

    while (GetTickCount64() - StartTickCount) <= 5000 do
    begin
        if Sieve <> nil then
            Sieve.Free;

        Sieve := PrimeSieve.Create(1000000);
        Sieve.RunSieve();
        PassCount := PassCount + 1;
    end;

    DurationTickCount := GetTickCount64() - StartTickCount;   //Stop the Timer

    if not Sieve.ValidateResults(ReferenceResults) then
       WriteLn('WARNING: result is incorrect!');

    //7.1.2021 Added to read exe file info
    writeln('Original pascal code created by: Rutger van Bergen [rbergen]');
    writeln('Modified slightly for Lazarus and FPC 3.2.0 by Rex Alfes [ralfes]');
    FileVerInfo:=TFileVersionInfo.Create(nil);
      try
        FileVerInfo.ReadFileInfo;
        writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
        writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
        writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
        writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
        writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
        writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
        writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
        writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);
      finally
        FileVerInfo.Free;
      end;
    //End if File Info stuff

    Writeln('------------------------------------------------------------');
    Write('ralfes;');
    Write(PassCount);
    Write(';');
    Write((DurationTickCount / 1000):4:2);
    WriteLn(';1;algorithm=base,faithful=yes');
    Writeln('------------------------------------------------------------');
end.
