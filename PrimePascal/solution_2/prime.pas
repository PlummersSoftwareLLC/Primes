program prime;

{$mode objfpc}

uses
    fgl, sysutils;

type
    CheckMap = specialize TFPGMap<PtrInt, PtrInt>;
    PPackedBoolArray = ^PackedBoolArray;
    PackedBoolArray = bitpacked array[0..high(PtrInt)-1] of Boolean;

    { PrimeSieve }

    PrimeSieve = class
    private
        SieveSize: PtrInt;
        NotPrimeArray: PPackedBoolArray;

    public
        constructor Create(Size: PtrInt);
        destructor Destroy; override;
        function RunSieve(): PPackedBoolArray;
        function CountPrimes(): PtrInt;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: PtrInt);
var
  NeededArraySize: PtrInt;
begin
    SieveSize := Size;
    NeededArraySize := (Size+1) div 2;
    GetMem(NotPrimeArray, NeededArraySize div 8);
    FillChar(NotPrimeArray^, NeededArraySize div 8, 0);
end;

destructor PrimeSieve.Destroy;
begin
  FreeMem(NotPrimeArray);
  inherited Destroy;
end;

function PrimeSieve.RunSieve(): PPackedBoolArray;
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
            if not NotPrimeArray^[Number div 2] then
            begin
                Factor := Number;
                break;
            end;

            inc(Number, 2);
        end;

        if Number > SieveSqrt then
            break;

        Number := Factor * Factor;

        while Number <= SieveSize do
        begin
            NotPrimeArray^[Number div 2] := True;
            inc(Number, Factor * 2);
        end;

        inc(Factor, 2);
    end;

    RunSieve := NotPrimeArray;
end;

function PrimeSieve.CountPrimes(): PtrInt;
var
    Count: PtrInt;
    I: PtrInt;
begin
    Count := 0;
    for I := 0 to (SieveSize-1) div 2 do
    begin
        if not NotPrimeArray^[I] then
            inc(Count);
    end;

    CountPrimes := Count;
end;

function PrimeSieve.ValidateResults(var ReferenceResults: CheckMap): Boolean;
var
    ReferenceValue: PtrInt;

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
        PassCount += 1;
    end;

    DurationTickCount := GetTickCount64() - StartTickCount;

    if not Sieve.ValidateResults(ReferenceResults) then
       WriteLn('WARNING: result is incorrect!');

    Write('circular17;');
    Write(PassCount);
    Write(';');
    Write((DurationTickCount / 1000):4:2);
    WriteLn(';1;algorithm=base,faithful=yes,bits=1');
end.