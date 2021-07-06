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
        constructor Create(Size: Int64);
        function RunSieve(): PackedBoolArray;
        function CountPrimes(): Integer;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: Int64);
begin
    SieveSize := Size;
    SetLength(NotPrimeArray, (Size + 1) Div 2);
end;

function PrimeSieve.RunSieve(): PackedBoolArray;
var
    Factor   : Integer = Default(Integer);
    Number   : Integer = Default(Integer);
    SieveSqrt: Integer = Default(Integer);

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

function PrimeSieve.CountPrimes(): Integer;
var
    Count: Integer = Default(Integer);
    I    : Integer = Default(Integer);
begin
    for I := Low(NotPrimeArray) to High(NotPrimeArray) do
    begin
        if not NotPrimeArray[I] then 
            Count := Count + 1;
    end;

    CountPrimes := Count;
end;

function PrimeSieve.ValidateResults(var ReferenceResults: CheckMap): Boolean;
var
    ReferenceValue: Integer = Default(Integer);

begin
    if ReferenceResults.TryGetData(SieveSize, ReferenceValue) then
       ValidateResults := ReferenceValue = CountPrimes()
    else
       ValidateResults := False;
end;

var
    ReferenceResults : CheckMap;
    StartTickCount   : QWord = Default(QWord);
    DurationTickCount: QWord = Default(QWord);
    PassCount        : Integer = Default(Integer);
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
