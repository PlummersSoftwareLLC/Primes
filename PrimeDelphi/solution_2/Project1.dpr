program Project2;

// Delphi true BIT per prime version by Kim Madsen/C4D

{$APPTYPE CONSOLE}

uses
 System.SysUtils,
 Classes,
 System.Generics.Collections,
 System.Timespan,
 Math,
 Windows;

type
 TKBMBits = record
  FBytes: array of byte;
  FByteSize: integer;
  FIntSize: integer;
  FRemainBytes: integer;
  Size: integer;
  FP: PByte;

  constructor Create(const ASize: integer);

  procedure SetBit(const AIndex: integer; const AValue: boolean); // inline;
  function GetBit(const AIndex: integer): boolean; // inline;
  procedure Initialize;
  procedure Clear; // inline;
  function CountOnes: integer; inline;

  property Bits[const AIndex: integer]: boolean read GetBit write SetBit;
 end;

 TPrimeSieve = class
 private
  FSieveSize: NativeInt;
  FSieveSize2: NativeInt;
  FSieveSizeSqrt: NativeInt;
  FBitArray: TKBMBits;
  FMyDict: TDictionary<NativeInt, NativeInt>;

 public
  constructor Create(Size: integer);
  destructor Destroy; override;

  procedure RunSieve; // Calculate the primes up to the specified limit

  function CountPrimes: integer;
  function ValidateResults: boolean;

  procedure PrintResults(ShowResults: boolean; Duration: Double;
    Passes: integer);
 end;

 { TKBMBits }

constructor TKBMBits.Create(const ASize: integer);
begin
 Size := ASize;
 FIntSize := (ASize + (2 SHL SizeOf(NativeInt)) - 1)
   DIV (SizeOf(NativeInt) SHL 3);
 FByteSize := (ASize + 7) DIV 8;
 FRemainBytes := FByteSize - (FByteSize DIV SizeOf(NativeInt)) *
   SizeOf(NativeInt);
 SetLength(FBytes, FIntSize * SizeOf(NativeInt));
 FP := @FBytes[0];
 Initialize;
end;

procedure TKBMBits.SetBit(const AIndex: integer; const AValue: boolean);
var
 o: integer;
 b: byte;
 p: PByte;
begin
 o := AIndex SHR 3;
 b := 1 SHL (AIndex - (o SHL 3));

 p := FP;
 inc(p, o);
 if AValue then
  p^ := p^ OR b
 else
  p^ := p^ AND (NOT b);
end;

function TKBMBits.GetBit(const AIndex: integer): boolean;
var
 o: integer;
 b: byte;
 p: PByte;
begin
 o := AIndex SHR 3;
 b := 1 SHL (AIndex - (o SHL 3));

 p := FP;
 inc(p, o);
 Result := (p^ AND b) <> 0;
end;

procedure TKBMBits.Clear;
var
 p: PByte;
 i: integer;
begin
 p := FP;
 i := FIntSize;
 while i > 0 do
 begin
  PNativeInt(p)^ := NOT 0;
  inc(p, SizeOf(NativeInt));
  dec(i);
 end;
 i := FRemainBytes;
 if i > 0 then
 begin
  p := FP;
  inc(p, FByteSize);
  repeat
   p^ := $00;
   inc(p);
   dec(i);
  until i = 0;
 end;
end;

procedure TKBMBits.Initialize;
var
 p: PNativeInt;
 i: integer;
begin
 p := PNativeInt(FP);
 i := FIntSize;
 while i > 0 do
 begin
  p^ := $00;
  inc(p);
  dec(i);
 end;
end;

function GetBitCount32(const X: NativeInt): integer; inline;
// {$IFDEF CPUX86}
// asm
// POPCNT    eax, X
// {$ELSE}
// {$IFDEF CPUX64}
// asm
// POPCNT    rax, X
// {$ELSE}
begin
 Result := X - ((X SHR 1) AND $55555555);
 Result := (Result AND $33333333) + ((Result SHR 2) AND $33333333);
 Result := (Result + (Result SHR 4)) AND $0F0F0F0F;
 Result := (Result * $01010101) SHR 24;
 // {$ENDIF}
 // {$ENDIF}
end;

function TKBMBits.CountOnes: integer;
var
 p: PNativeInt;
 i: integer;
begin
 p := PNativeInt(FP);
 Result := 0;
 i := 0;
 while i < FIntSize do
 begin
  Result := Result + GetBitCount32(p^);
  inc(p);
  inc(i);
 end;
end;

{ TPrimeSieve }

constructor TPrimeSieve.Create(Size: integer);
begin
 inherited Create;

 FSieveSize := Size;
 FSieveSize2 := (Size + 1) div 2;
 FSieveSizeSqrt := Floor(Sqrt(FSieveSize));

 // The optimization here is that we only store bits for *odd* numbers.
 // So FBitArray[3] is if 3 is prime
 // and FBitArray[4] is if 5 is prime
 // GetBit and SetBit do the work of "div 2"
 FBitArray := TKBMBits.Create(FSieveSize2);
 FBitArray.Clear;

 FMyDict := TDictionary<NativeInt, NativeInt>.Create;

 // Historical data for validating our results - the number of primes
 // to be found under some limit, such as 168 primes under 1000
 FMyDict.Add(10, 4); // nobody noticed that 1 is wrong? [2, 3, 5, 7]
 FMyDict.Add(100, 25);
 FMyDict.Add(1000, 168);
 FMyDict.Add(10000, 1229);
 FMyDict.Add(100000, 9592);
 FMyDict.Add(1000000, 78498);
 FMyDict.Add(10000000, 664579);
 FMyDict.Add(100000000, 5761455);
end;

destructor TPrimeSieve.Destroy;
begin
 FreeAndNil(FMyDict);
 inherited;
end;

function TPrimeSieve.CountPrimes: integer;
begin
 Result := FBitArray.CountOnes;
end;

function TPrimeSieve.ValidateResults: boolean;
begin
 if FMyDict.ContainsKey(FSieveSize) then
  Result := FMyDict[FSieveSize] = Self.CountPrimes
 else
  Result := False;
end;

procedure TPrimeSieve.RunSieve;
var
 num: NativeInt;
 r: NativeInt;
 factor: NativeInt;
begin
 factor := 3;

 while factor <= FSieveSizeSqrt do
 begin
  r := factor AND $1;
  num := factor SHR 1;
  while num < FSieveSize2 do
  begin
   if FBitArray.Bits[num] then
   begin
    factor := (num SHL 1) OR r;
    Break;
   end;
   inc(num);
  end;

  // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
  num := factor * 3 SHR 1;
  while num <= FSieveSize2 do
  begin
   FBitArray.Bits[num] := False;
   inc(num, factor);
  end;

  inc(factor, 2);
 end;
end;

procedure TPrimeSieve.PrintResults(ShowResults: boolean; Duration: Double;
  Passes: integer);
var
 count: integer;
 num: integer;
const
 SYesNo: array [boolean] of string = ('No', 'Yes');
begin
 if ShowResults then
  Write('2, ');

 count := 1;
 for num := 3 to FSieveSize do
 begin
  if (num and $1 = $1) then
  begin
   if FBitArray.Bits[num div 2] then
   begin
    if ShowResults then
     Write(IntToStr(num) + ', ');
    inc(count);
   end;
  end;
 end;

 if ShowResults then
  WriteLn('');

 WriteLn(Format
   ('Passes: %d, Time: %.3f sec, Avg: %.4f ms, Limit: %d, Count: %d, Valid: %s',
   [Passes, Duration, Duration / Passes * 1000, FSieveSize, count,
   SYesNo[ValidateResults]]));
end;

procedure Main;
var
 sieve: TPrimeSieve;
 dtStart: TDateTime;
 Passes: integer;
 tD: TTimeSpan;
begin
 dtStart := Now;
 Passes := 0;

 sieve := nil;
 while TTimeSpan.Subtract(Now, dtStart).TotalSeconds < 10 do
 begin
  if Assigned(sieve) then
   sieve.Free;

  sieve := TPrimeSieve.Create(1000000);
  sieve.RunSieve;
  inc(Passes);
 end;

 tD := TTimeSpan.Subtract(Now, dtStart);
 if Assigned(sieve) then
  sieve.PrintResults(False, tD.TotalSeconds, Passes);
end;

{
  1 bit per prime, rather than 1 byte per prime, using optimized bit class by Kim Madsen/C4D
  AMD Threadripper 1950X @ 3.40 GHz
  - 32-bit: 4,600+ passes
}
begin
 try
  Main;
  WriteLn('Press enter to close...');
  Readln;
 except
  on E: Exception do
   WriteLn(E.ClassName, ': ', E.Message);
 end;

end.
