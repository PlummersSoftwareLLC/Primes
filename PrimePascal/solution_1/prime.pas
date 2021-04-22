program prime;

{$mode objfpc}

uses
    fgl, sysutils;

type
    CheckMap = specialize TFPGMap<Integer, Integer>;

    PrimeSieve = class
    private
        SieveSize: Integer;
        BitArray: packed array of Boolean;

    public
        constructor Create(Size: Integer);
        function CountPrimes(): Integer;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
        procedure RunSieve();
end;

constructor PrimeSieve.Create(Size: Integer);
begin
    SieveSize := Size;
    SetLength(BitArray, (Size + 1) Div 2);
end;

function PrimeSieve.CountPrimes() : Integer;
var
    Count: Integer;
    I: Integer;
begin
    Count := 0;
    for I := Low(BitArray) to High(BitArray) do
    begin
        if not BitArray[I] then Count := Count + 1;
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

procedure PrimeSieve.RunSieve();
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
            if ((Number mod 2) <> 0) and (not BitArray[Number div 2]) then
            begin
                Factor := Number;
                break;
            end;

            Number := Number + 1;
        end;

        Number := Factor * 3;

        while Number <= SieveSize do
        begin
            BitArray[Number div 2] := True;
            Number := Number + (Factor * 2);
        end;

        Factor := Factor + 2;
    end;
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
