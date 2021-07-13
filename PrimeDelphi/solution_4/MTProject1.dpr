program MTProject1;

// ---------------------------------------------
// Multithreaded using all cores including hyperthreading ones
// 1 bit rather than 1 byte per prime candidate,
// using optimized bit class.
// ---------------------------------------------
//
// Optimized by Kim Madsen/C4D with help from
//
// Brian Muegge
//
// Supports both 32 bit and 64 bit compilation.
//
// AMD Threadripper 1950X @ 3.40 GHz 32 cores
// - 32-bit:  85000+ passes
// - 64-bit: 129200+ passes
//

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

      procedure ClearBit(const AIndex: integer); inline;
      function GetBit(const AIndex: integer): boolean; inline;
      procedure Initialize;
      procedure Clear; inline;
      function CountOnes: integer; inline;
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

   end;

   TSieveThread = class(TThread)
   private
      FPasses: integer;
      FSieve: TPrimeSieve;
   protected
      procedure Execute; override;
   public
      destructor Destroy; override;
   end;

// ---------------------------
// TSieveThread
// ---------------------------
destructor TSieveThread.Destroy;
begin
   if Assigned(FSieve) then
      FreeAndNil(FSieve);
   inherited Destroy;
end;

procedure TSieveThread.Execute;
var
   sieve: TPrimeSieve;
begin
   FPasses := 0;
   FSieve := nil;
   sieve := nil;
   while not Terminated do
   begin
      sieve := TPrimeSieve.Create(1000000);
      sieve.RunSieve;
      inc(FPasses);
      if Assigned(FSieve) then
         FreeAndNil(FSieve);
      FSieve := sieve;
   end;
end;

// ---------------------------
// TKBMBits
// ---------------------------

constructor TKBMBits.Create(const ASize: integer);
begin
   Size := ASize;
   FIntSize := (ASize + (2 SHL SizeOf(NativeInt)) - 1)
     DIV (SizeOf(NativeInt) SHL 3);
   FByteSize := (ASize + 7) DIV 8;
   FRemainBytes := FIntSize * SizeOf(NativeInt) - FByteSize;
   SetLength(FBytes, FIntSize * SizeOf(NativeInt));
   FP := @FBytes[0];
   Initialize;
end;

procedure TKBMBits.ClearBit(const AIndex: integer);
var
   o: integer;
   b: byte;
   p: PByte;
begin
   o := AIndex SHR 3;

   b := 1 SHL (AIndex AND $7);
   p := FP;
   inc(p, o);
   p^ := p^ AND (NOT b);
end;

function TKBMBits.GetBit(const AIndex: integer): boolean;
var
   o: integer;
   b: byte;
   p: PByte;
begin
   o := AIndex SHR 3;

   b := 1 SHL (AIndex AND $7);
   p := FP;
   inc(p, o);
   Result := (p^ AND b) = b;
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

function GetBitCount(const X: NativeInt): NativeInt; inline;
// {$IFDEF CPUX86}
// asm
// POPCNT    eax, X
// {$ELSE}
// {$IFDEF CPUX64}
// asm
// POPCNT    rax, X
// {$ELSE}
begin
{$IFDEF CPUX86}
   Result := X - ((X SHR 1) AND $55555555);
   Result := (Result AND $33333333) + ((Result SHR 2) AND $33333333);
   Result := (Result + (Result SHR 4)) AND $0F0F0F0F;
   Result := (Result * $01010101) SHR 24;
{$ELSE}
{$IFDEF CPUX64}
   Result := X - ((X SHR 1) AND $5555555555555555);
   Result := (Result AND $3333333333333333) +
     ((Result SHR 2) AND $3333333333333333);
   Result := (Result + (Result SHR 4)) AND $0F0F0F0F0F0F0F0F;
   Result := (Result * $101010101010101) SHR 56;
{$ENDIF}
{$ENDIF}
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
      Result := Result + GetBitCount(p^);
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
         if FBitArray.GetBit(num) then
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
         FBitArray.ClearBit(num);
         inc(num, factor);
      end;

      inc(factor, 2);
   end;
end;

// ---------------------------
// Main
// ---------------------------

procedure PrintResults(SieveThread: TSieveThread; ShowResults: boolean;
  Duration: Double);
var
   count: integer;
   num: integer;
   sieve:TPrimeSieve;
const
   SYesNo: array [boolean] of string = ('No', 'Yes');
begin
   sieve:=SieveThread.FSieve;
   if ShowResults then
      Write('2, ');

   count := 1;
   for num := 3 to sieve.FSieveSize do
   begin
      if (num and $1 = $1) then
      begin
         if sieve.FBitArray.GetBit(num div 2) then
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
     [SieveThread.FPasses, Duration, Duration / SieveThread.FPasses * 1000,
     sieve.FSieveSize, count,
     SYesNo[sieve.ValidateResults]]));
end;

procedure Main;
var
   dtStart: TDateTime;
   tD: TTimeSpan;
   threads: array of TSieveThread;
   i, sum: integer;
   cores:integer;
begin
   cores:=CPUCount;
   SetLength(threads, cores);
   for i := 0 to cores - 1 do
   begin
      threads[i] := TSieveThread.Create(true);
      threads[i].FreeOnTerminate := False;
      threads[i].Resume;
   end;

   dtStart := Now;

   try
      try
         while TTimeSpan.Subtract(Now, dtStart).TotalSeconds < 10 do
            sleep(10);
         tD := TTimeSpan.Subtract(Now, dtStart);
      finally
         for i := 0 to cores - 1 do
            threads[i].Terminate;
      end;

   finally
      WriteLn(Format('Cores: %d',[cores]));
      sum := 0;
      for i := 0 to cores - 1 do
      begin
         threads[i].WaitFor;
         inc(sum, threads[i].FPasses);
         PrintResults(threads[i], False, tD.TotalSeconds);
         threads[i].Free;
      end;

      WriteLn(Format('TotalPasses: %d, Time: %.3f sec, Avg: %.4f ms',
        [sum, tD.TotalSeconds, tD.TotalSeconds / sum * 1000]));
   end;
end;

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
