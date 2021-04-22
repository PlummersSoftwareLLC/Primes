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
        PrimeArray: PackedBoolArray;

    public
        constructor Create(Size: Integer);
        function RunSieve(): PackedBoolArray;
        function CountPrimes(): Integer;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: Integer);
begin
    SieveSize := Size;
    SetLength(PrimeArray, (Size + 1) Div 2);
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
            if ((Number mod 2) <> 0) and (not PrimeArray[Number div 2]) then
            begin
                Factor := Number;
                break;
            end;

            Number := Number + 1;
        end;

        Number := Factor * 3;

        while Number <= SieveSize do
        begin
            PrimeArray[Number div 2] := True;
            Number := Number + (Factor * 2);
        end;

        Factor := Factor + 2;
    end;

    RunSieve := PrimeArray;
end;

function PrimeSieve.CountPrimes(): Integer;
var
    Count: Integer;
    I: Integer;
begin
    Count := 0;
    for I := Low(PrimeArray) to High(PrimeArray) do
    begin
        if not PrimeArray[I] then Count := Count + 1;
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

    while (GetTickCount64() - StartTickCount) <= 5000 do
    begin
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
    WriteLn(';1');
end.
