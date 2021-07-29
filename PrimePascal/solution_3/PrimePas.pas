program PrimePas;

{$mode objfpc}{$H+}

uses
  {$if defined(linux) or defined(freebsd) or defined(darwin)}
  cthreads,
  {$endif}
  SysUtils, fgl, CPUCountUtil;

type
  TResultsDictionary = specialize TFPGMap<int64, int64>;

  TPackedBits = bitpacked array [0..high(uint32)] of boolean;
  PPackedBits = ^TPackedBits;

  TSieveThreadParam = record
    sieveSize: size_t;

    runningThreads,
    totalThreads,
    passes: integer;

    showValidation,
    showResults,
    terminateThreads: boolean;

    startTime: int64;
  end;

  PSieveThreadParam = ^TSieveThreadParam;

  { prime_sieve }
  prime_sieve = class
    private
      sieveSize: size_t;
      bits: PPackedBits;

      procedure ClearBits(n, skip: size_t);
      function CountPrimes: size_t;
      procedure PrintResults(showResults, showValidation: boolean; duration: double; passes, threads: integer);
      function ValidateResults: boolean;

    public
      constructor Create(n: size_t);
      destructor Destroy; override;

      procedure RunSieve;
  end;
{ prime_sieve }

constructor prime_sieve.Create(n: size_t);
var
  nbBytes: size_t;

begin
  sieveSize:=n;
  //allocating sieveSize/8 bytes on the heap to store the bits
  nbBytes:=n >> 3 + ord((n and $7)>0); // an extra byte is added in the case where n is not a multiple of 8
  bits:=GetMem(nbBytes);
  fillchar(bits[0], nbBytes, $AA); // initializing bits to true for odd numbers and false for even numbers
end;

destructor prime_sieve.Destroy;
begin
  FreeMem(bits);
  inherited Destroy;
end;

procedure prime_sieve.ClearBits(n, skip: size_t);
begin
  while n<=sieveSize do
  begin
    bits^[n]:=false;
    n+=skip;
  end;
end;

procedure prime_sieve.RunSieve;
var
  num,
  factor,
  q: size_t;

begin
  factor:=3;
  q:=trunc(sqrt(sieveSize));

  while factor<=q do
  begin
    num:=factor;

    while (num<=sieveSize) and not bits^[num] do
      num+=2;

    if num>sieveSize then
      break;

    factor:=num;
    ClearBits(factor*factor, factor+factor);

    factor+=2;
  end;
end;

function prime_sieve.CountPrimes: size_t;
var
  i,
  count: size_t;

begin
  count:=ord(sieveSize>=2);

  i:=3;

  while i<=sieveSize do
  begin
    if bits^[i] then
      count+=1;

    i+=2;
  end;

  result:=count;
end;

// validateResults
//
// Checks to see if the number of primes found matches what we should expect.  This data isn't used in the
// sieve processing at all, only to sanity check that the results are right when done.
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

procedure prime_sieve.PrintResults(showResults, showValidation: boolean; duration: double; passes, threads: integer);
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
    if bits^[num] then
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
                   passes,
                   threads,
                   duration,
                   duration*1000/passes,
                   sieveSize,
                   count, CountPrimes,
                   BoolToStr(validateResults, 'true', 'false')]));
    writeln;
  end;

  writeln(format('olivierbrun;%d;%.6f;%d;algorithm=base,faithful=yes,bits=1', [passes, duration, threads]));
end;

// retrieve command line options
procedure GetRequested(var lThreadsRequested, lSecondsRequested: longint; var lSizeRequested: size_t; var lShowResults, lShowValidation: boolean);
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
      '-s': lSizeRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 1000000);
      '-v': lShowValidation:=true;
    end;
  end;
end;

// this is the function to spawn a thread
function SieveThread(p: pointer): ptrint;
var
  sieve: prime_sieve;
  lSieveThreadParam: PSieveThreadParam;

begin
  lSieveThreadParam:=p;
  InterLockedIncrement(lSieveThreadParam^.runningThreads); // increment the running threads count
  sieve:=nil;

  // calculating primes until the main thread sets the terminateThreads to true
  while not lSieveThreadParam^.terminateThreads do
  begin
    // if a sieve object exists destroy it
    if assigned(sieve) then
      FreeAndNil(sieve);

    sieve:=prime_sieve.Create(lSieveThreadParam^.sieveSize); // instantiate a new sieve
    sieve.runSieve; // run it
    InterLockedIncrement(lSieveThreadParam^.passes); // the passes variable is shared among all threads, hence the use of the thread safe InterLockedIncrement instruction instead of directly doing a +=1 operation
  end;

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
