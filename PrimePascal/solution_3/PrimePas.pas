program PrimePas;

{$mode objfpc}{$H+}

uses
  {$if defined(linux) or defined(freebsd) or defined(darwin)}
  cthreads,
  {$endif}
  SysUtils, fgl, CPUCountUtil;

type
  TSieveThreadParam = record
    sieveSize: uint64;

    runningThreads,
    totalThreads,
    passes: integer;

    showValidation,
    showResults,
    terminateThreads: boolean;

    startTime: int64;
  end;

  PSieveThreadParam = ^TSieveThreadParam;

  TResultsDictionary = specialize TFPGMap<uint64, uint64>;

  TBitsArr = array [0..high(uint32)] of uint32; // number of uint32 will be SieveSize/64 + 1 extra uint32 if SieveSize/2 (flags for odd numbers only) is not a multiple of 32
  PBitsArr = ^TBitsArr;

  { prime_sieve }
  prime_sieve = class
    private
      sieveSize: uint64;
      bits: PbitsArr;
      mask: array [0..31] of uint32; // masks used for the IsPrime and ClearBits methods

      procedure ClearBits(factor, nMax: uint64); inline;
      function IsPrime(n: uint64): boolean; inline;
      function CountPrimes: uint64;
      procedure PrintResults(showResults, showValidation: boolean; duration: double; totalPasses, threads: integer);
      function ValidateResults: boolean;

    public
      constructor Create(n: uint64);
      destructor Destroy; override;
      procedure RunSieve;
  end;

{ prime_sieve }

constructor prime_sieve.Create(n: uint64);
var
  bitsArrSize: uint32; // 32 bits size array because each value is 32 bits (containing 32 flags for prime numbers)
  i: byte;

begin
  sieveSize:=n;
  //allocating bitsArrSize = sieveSize/64 uint32s on the heap to store the bits for odd numbers
  bitsArrSize:=(n>>6) + ord(((n>>1) and 31)<>0); //and extra uint32 is added in case sieveSize/2 is not a multiple of 32
  bits:=GetMem(bitsArrSize*4); // 32 bits = 4 bytes => allocating sieveSize/2/4 bytes of memory
  fillDWord(bits^[0], bitsArrSize, $FFFFFFFF); // setting all bits to true

  // generating masks for ClearBits and GetBis (IsPrime) methods
  for i:=0 to 31 do mask[i]:=1 << i;
end;

destructor prime_sieve.Destroy;
begin
  FreeMem(bits);
  inherited Destroy;
end;

procedure prime_sieve.ClearBits(factor, nMax: uint64);
var
  n: uint64;

begin
  n:=(factor*factor)>>1;

  while n<=nMax do
  begin
    bits^[n>>5]:=bits^[n>>5] and not mask[n and 31];
    n+=factor;
  end;
end;

function prime_sieve.IsPrime(n: uint64): boolean;
begin
  result:=(bits^[n>>6] and mask[(n>>1) and 31])<>0; // checking whether the bit is set to 1
end;

procedure prime_sieve.RunSieve;
var
  factor,
  q: uint64;

begin
  factor:=3;
  q:=trunc(sqrt(sieveSize));

  while factor<=q do
  begin
    // searching the next prime number to set the factor
    while ((bits^[factor>>6] and mask[(factor>>1) and 31])=0) and (factor<SieveSize) do
      factor+=2;

    ClearBits(factor, sieveSize>>1); // clearing bits for multiples of the factor

    factor+=2;
  end;
end;

function prime_sieve.CountPrimes: uint64;
var
  i,
  count: uint64;

begin
  count:=ord(sieveSize>=2);

  i:=3;

  while i<=sieveSize do
  begin
    if IsPrime(i) then
      count+=1;

    i+=2;
  end;

  result:=count;
end;

function prime_sieve.ValidateResults: boolean;
const
  PRIME_COUNTS: array [0..9, 0..1] of uint64 = (
    (10, 4),
    (100, 25),
    (1000, 168),
    (10000, 1229),
    (100000, 9592),
    (1000000, 78498),
    (10000000, 664579),
    (100000000, 5761455),
    (1000000000, 50847534),
    (10000000000, 455052511)
  );

var
  i: integer;
  resultsDictionary: TResultsDictionary;

begin
  resultsDictionary:=TResultsDictionary.Create;
  resultsDictionary.Sorted:=true;

  for i:=0 to high(PRIME_COUNTS) do
     resultsDictionary.Add(PRIME_COUNTS[i,0], PRIME_COUNTS[i,1]);

  if resultsDictionary.Find(sieveSize, i) then
    result:=resultsDictionary.Data[i]=CountPrimes
  else
    result:=false;

  resultsDictionary.Free;
end;

procedure prime_sieve.PrintResults(showResults, showValidation: boolean; duration: double; totalPasses, threads: integer);
var
  count,
  num: uint64;

begin
    if showResults and (sieveSize>=2) then
      write('2, ');

  count:=ord(sieveSize>=2);

  num:=3;

  while num<=sieveSize do
  begin
    if IsPrime(num) then
    begin
      if showResults then
         write(format('%d, ', [num]));

      count+=1;
    end;

    num+=2;
  end;

  if showResults then
    writeln;

  if showValidation then
  begin
    writeln(format('Passes: %d, Theads: %d, Time: %.6f s, Avg: %.6f ms, Limit: %d, Counts: %d/%d, Valid: %s',[
                   totalPasses,
                   threads,
                   duration,
                   duration*1000/totalPasses, // displaying average in milliseconds
                   sieveSize,
                   count, CountPrimes,
                   BoolToStr(validateResults, 'true', 'false')]));
    writeln;
  end;

  writeln(format('olivierbrun-%d-threads;%d;%.6f;%d;algorithm=base,faithful=yes,bits=1', [threads, totalPasses, duration, threads]));
end;

// retrieve command line options
procedure GetRequested(var lThreadsRequested, lSecondsRequested: longint; var lSizeRequested: uint64; var lShowResults, lShowValidation: boolean);
var
  numParam: longint;
  lCommand: string;

begin
  lThreadsRequested:=GetSystemThreadCount;
  lSecondsRequested:=5;
  lSizeRequested:=1000000;
  lShowResults:=false;
  lShowValidation:=false;

  for numParam:=1 to ParamCount do
  begin
    lCommand:=ParamStr(numParam);

    case LowerCase(LeftStr(lCommand,2)) of
      '-l': lShowResults:=true;
      '-t': lThreadsRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 1);
      '-d': lSecondsRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 5);
      '-s': lSizeRequested:=StrToInt64Def(Trim(RightStr(lCommand, length(lCommand)-2)), 1000000);
      '-v': lShowValidation:=true;
    end;
  end;
end;

// this is the function to spawn a thread
function SieveThread(p: pointer): ptrint;
var
  sieve: prime_sieve;
  lSieveThreadParam: PSieveThreadParam;
  passes: integer;

begin
  lSieveThreadParam:=p;
  InterLockedIncrement(lSieveThreadParam^.runningThreads); // increment the running threads count
  sieve:=nil;
  passes:=0;

  // calculating primes until the main thread sets the terminateThreads to true
  while not lSieveThreadParam^.terminateThreads do
  begin
    // if a sieve object exists destroy it
    if assigned(sieve) then
      FreeAndNil(sieve);

    sieve:=prime_sieve.Create(lSieveThreadParam^.sieveSize); // instantiate a new sieve
    sieve.runSieve; // run it
    passes+=1;
  end;

  InterlockedExchangeAdd(lSieveThreadParam^.passes, passes); // adding passes to the shared variable in a thread safe way

  // displaying results if this is the latest thread to terminate
  if lSieveThreadParam^.runningThreads=1 then
     sieve.PrintResults(lSieveThreadParam^.showResults, lSieveThreadParam^.showValidation, (GetTickCount64-lSieveThreadParam^.startTime)/1000, lSieveThreadParam^.passes, lSieveThreadParam^.totalThreads);

  sieve.Free;
  InterLockedDecrement(lSieveThreadParam^.runningThreads); // decrement the running threads count
  result:=0;
end;

var
  sieveThreadParam: TSieveThreadParam;

  numThread,
  secondsRequested: longint;

begin
  GetRequested(sieveThreadParam.totalThreads, secondsRequested, sieveThreadParam.sieveSize, sieveThreadParam.showResults, sieveThreadParam.showValidation); // retrieving command line options

  sieveThreadParam.passes:=0;
  sieveThreadParam.runningThreads:=0;
  sieveThreadParam.terminateThreads:=false;
  sieveThreadParam.startTime:=GetTickCount64;

  // spawning the requested threads
  for numThread:=1 to sieveThreadParam.totalThreads do
    BeginThread(@sieveThread, @sieveThreadParam);

  while true do
  begin
    if (GetTickCount64-sieveThreadParam.startTime)/1000>=secondsRequested then
    begin
      sieveThreadParam.terminateThreads:=true;

      // waiting for all running threads to terminate
      repeat
      until sieveThreadParam.runningThreads=0;

      break;
    end;
  end;
end.
