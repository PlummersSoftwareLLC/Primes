program prime;

{$mode objfpc}

uses
    fgl, sysutils;

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
        function CountPrimes(): Integer;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: Integer);
begin
    SieveSize := Size;
    SetLength(NotPrimeArray, (Size + 1) Div 2);
end;

function PrimeSieve.RunSieve(): PackedBoolArray;
var
    Factor: Integer = 3;
    Number: Integer;
    SieveSqrt: Integer;
    Iterations: Integer;
    Step: Integer;
    I: Integer;
begin
    SieveSqrt := Trunc(Sqrt(SieveSize));
    while Factor <= SieveSqrt do
    begin
        Iterations := (SieveSqrt - Factor) div 2;
        for I := 0 to Iterations do
            if not NotPrimeArray[(Factor + (I * 2)) div 2] then
            begin
                Factor := Factor + (I * 2);
                break;
            end;

        Number := Factor * Factor;

        Step := Factor * 2;
        Iterations := (SieveSize - Number) div Step;
        for I := 0 to Iterations do
            NotPrimeArray[(Number + (I * Step)) div 2] := True;

        Factor := Factor + 2;
    end; 

    RunSieve := NotPrimeArray;
end;

function PrimeSieve.CountPrimes(): Integer;
var
    Count: Integer;
    I: Integer;
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
    ReferenceValue: Integer;

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
    PassCount: Integer;
    Sieve: PrimeSieve;

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
    StartTickCount := GetTickCount64();
    Sieve := nil;

    while (GetTickCount64() - StartTickCount) <= 5000 do
    begin
        if Sieve <> nil then 
            Sieve.Free;
        
        Sieve := PrimeSieve.Create(1000000);
        Sieve.RunSieve();
        PassCount := PassCount + 1;
    end;

    DurationTickCount := GetTickCount64() - StartTickCount;

    if not Sieve.ValidateResults(ReferenceResults) then
       WriteLn('WARNING: result is incorrect!');

    Write('rbergen;');
    Write(PassCount);
    Write(';');
    Write((DurationTickCount / 1000):4:2);
    WriteLn(';1;algorithm=base,faithful=yes');
end.
