program prime;

{$mode objfpc}

uses
    fgl, sysutils;

type
    CheckMap = specialize TFPGMap<NativeUInt, NativeUInt>;
    PackedBoolArray = packed array of Boolean;

    TArrayFor10 = packed array [0..4] of Boolean;
    TArrayFor100 = packed array [0..49] of Boolean;
    TArrayFor1000 = packed array [0..499] of Boolean;
    TArrayFor10000 = packed array [0..4999] of Boolean;
    TArrayFor100000 = packed array [0..49999] of Boolean;
    TArrayFor1000000 = packed array [0..499999] of Boolean;
    TArrayFor10000000 = packed array [0..4999999] of Boolean;
    TArrayFor100000000 = packed array [0..49999999] of Boolean;

    generic PrimeSieve<_T> = class
    private
        SieveSize: NativeUInt;
        NotPrimeArray: _T;

    public
        constructor Create(Size: NativeUInt);
        procedure RunSieve;
        function CountPrimes(): NativeUInt;
        function ValidateResults(var ReferenceResults: CheckMap): Boolean;
end;

constructor PrimeSieve.Create(Size: NativeUInt);
begin
    SieveSize := Size;
end;

procedure PrimeSieve.RunSieve;
var
    Factor: NativeUInt = 3;
    Number: NativeUInt;
    SieveSqrt: NativeUInt;
    Iterations: NativeUInt;
    Step: NativeUInt;
    I: NativeUInt;
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
end;

function PrimeSieve.CountPrimes(): NativeUInt;
var
    Count: NativeUInt;
    I: NativeUInt;
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
    ReferenceValue: NativeUInt;

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
    PassCount: NativeUInt;
    Sieve: specialize PrimeSieve<TArrayFor1000000>;

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
        
        Sieve := specialize PrimeSieve<TArrayFor1000000>.Create(1000000);
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
