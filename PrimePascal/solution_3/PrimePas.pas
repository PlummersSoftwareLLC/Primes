program PrimePas;

{$mode objfpc}{$H+}

uses
  {$IF defined(linux)}
  cthreads, ctypes,
  {$ELSEIF defined(freebsd) or defined(darwin)}
  uses ctypes, sysctl,
  {$ELSEIF defined(windows)}
  Windows,
    {$IFDEF UseTProcessW}
    pipes,
    {$ENDIF}
  {$ENDIF}
  SysUtils, fgl;

type
  TResultsDictionary = specialize TFPGMap<int64, int64>;

  TPackedBits = bitpacked array [0..high(longint)] of boolean;
  PPackedBits = ^TPackedBits;

  TSieveThreadParam = record
    sieveSize: int64;
    runningThreads,
    passes: longint;
    threadStarted,
    terminateThreads: boolean;
    bits: PPackedBits;
  end;

  PSieveThreadParam = ^TSieveThreadParam;

  { prime_sieve }
  prime_sieve = class
    private
      sieveSize: int64;
      bits: PPackedBits;

    public
      constructor Create(n: int64);
      destructor Destroy; override;

      procedure runSieve;
  end;

{$IFDEF Linux}
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
const _SC_NPROCESSORS_ONLN = 83;
{$ENDIF}

{ prime_sieve }

constructor prime_sieve.Create(n: int64);
var
  nbBytes: int64;

begin
  sieveSize:=n;
  //allocating sieveSize/8 bytes on the heap to store the bits
  nbBytes:=n >> 3 + ord((n and $7)>0); // an extra byte is added in the case where n is not a multiple of 8
  bits:=GetMem(nbBytes);
  fillchar(bits[0], nbBytes, $FF); // initializing all bits to true
end;

destructor prime_sieve.Destroy;
begin
  FreeMem(bits);
  inherited Destroy;
end;

procedure prime_sieve.runSieve;
var
  num,
  step,
  factor,
  q: int64;


begin
  factor:=3;
  q:=trunc(sqrt(sieveSize));

  while factor<=q do
  begin
    num:=factor;

    while num<sieveSize do
    begin
      if bits^[num] then
      begin
        factor:=num;
        break;
      end;

      num+=2;
    end;

    num:=factor*factor;
    step:=factor+factor;

    while num<sieveSize do
    begin
      bits^[num]:=false;
      num+=step;
    end;

    factor+=2;
  end;
end;

function GetSystemThreadCount: integer;
// returns a good default for the number of threads on this system
{$IF defined(windows)}
//returns total number of processors available to system including logical hyperthreaded processors
var
  SystemInfo: SYSTEM_INFO;
  {$IFnDEF WinCE}
  i: Integer;
  ProcessAffinityMask, SystemAffinityMask: DWORD_PTR;
  Mask: DWORD;
  {$ENDIF}
begin
  {$IFnDEF WinCE}
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask{%H-}, SystemAffinityMask{%H-})
  then begin
    Result := 0;
    for i := 0 to 31 do begin
      Mask := DWord(1) shl i;
      if (ProcessAffinityMask and Mask)<>0 then
        inc(Result);
    end;
    exit;
  end;
  {$ENDIF}
  //can't get the affinity mask so we just report the total number of processors
  GetSystemInfo(SystemInfo{%H-});
  Result := SystemInfo.dwNumberOfProcessors;
end;
{$ELSEIF defined(UNTESTEDsolaris)}
  begin
    t = sysconf(_SC_NPROC_ONLN);
  end;
{$ELSEIF defined(freebsd) or defined(darwin)}
var
  mib: array[0..1] of cint;
  len: cint;
  t: cint;
begin
  mib[0] := CTL_HW;
  mib[1] := HW_NCPU;
  len := sizeof(t);
  {$if FPC_FULLVERSION >= 30101}
  fpsysctl(@mib, 2, @t, @len, Nil, 0);
  {$else}
  fpsysctl(pchar(@mib), 2, @t, @len, Nil, 0);
  {$endif}
  Result:=t;
end;
{$ELSEIF defined(linux)}
  begin
    Result:=sysconf(_SC_NPROCESSORS_ONLN);
  end;

{$ELSE}
  begin
    Result:=1;
  end;
{$ENDIF}

procedure GetRequested(var lThreadsRequested, lSecondsRequested: longint; var lSizeRequested: int64);
var
  numParam: longint;
  lCommand: string;

begin
  lThreadsRequested:=GetSystemThreadCount;
  lSecondsRequested:=5;
  lSizeRequested:=1000000;

  for numParam:=1 to ParamCount do
  begin
    lCommand:=ParamStr(numParam);

    case LowerCase(LeftStr(lCommand,2)) of
      '-t': lThreadsRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 1);
      '-d': lSecondsRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 5);
      '-s': lSizeRequested:=StrToIntDef(Trim(RightStr(lCommand, length(lCommand)-2)), 1000000);
    end;
  end;
end;

procedure FillValidateResults(var lResultsDictionary: TResultsDictionary);
const
  PRIME_COUNTS: array [0..9, 0..1] of int64 = (
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
  i: longint;

begin
  lResultsDictionary:=TResultsDictionary.Create;
  lResultsDictionary.Sorted:=true;

  for i:=0 to high(PRIME_COUNTS) do
     lResultsDictionary.Add(PRIME_COUNTS[i,0], PRIME_COUNTS[i,1]);
end;

function SieveThread(p: pointer): ptrint;
var
  sieve: prime_sieve;

  nbBytes,
  sieveSize: int64;

  lSieveThreadParam: PSieveThreadParam;

begin
  lSieveThreadParam:=p;
  InterLockedIncrement(lSieveThreadParam^.runningThreads);
  lSieveThreadParam^.threadStarted:=true;
  sieveSize:=lSieveThreadParam^.sieveSize;
  sieve:=prime_sieve.Create(sieveSize);
  sieve.runSieve;

  if lSieveThreadParam^.terminateThreads and (lSieveThreadParam^.runningThreads=1) then
  begin
    nbBytes:=sieveSize >> 3 + ord((sieveSize and $7)>0);
    lSieveThreadParam^.bits:=GetMem(nbBytes);
    move(sieve.bits[0], lSieveThreadParam^.bits[0], nbBytes);
  end;

  sieve.Free;
  InterLockedIncrement(lSieveThreadParam^.passes);
  InterLockedDecrement(lSieveThreadParam^.runningThreads);
  result:=0;
end;

function CountPrimes(lSieveSize: int64; lBits: PPackedBits): int64;
var
  i,
  count: int64;

begin
  if lSieveSize>=2 then
    count:=1
  else
    count:=0;

  i:=3;

  while i<lSieveSize do
  begin
    if lBits^[i] then
      count+=1;

    i+=2;
  end;

  result:=count;
end;

function ValidateResults(const lResultsDictionary: TResultsDictionary; lSieveSize: int64; lBits: PPackedBits): boolean;
var
  index: longint;

begin
  if lResultsDictionary.Find(lSieveSize, index) then
    result:=lResultsDictionary.Data[index]=CountPrimes(lSieveSize, lBits)
  else
    result:=false;
end;

procedure PrintResults(lShowResults: boolean; lDuration: double; lPasses: longint; lSieveSize: int64; lBits: PPackedBits; const lResultsDictionary: TResultsDictionary; lThreadsRequested: longint);
const
  TrueFalse: array[boolean] of string = ('false', 'true');

var
  num,
  count: int64;

begin
  if lSieveSize>=2 then
  begin
    if lShowResults then
      write('2, ');

    count:=1;
  end
  else
    count:=0;

  num:=3;

  while num<lSieveSize do
  begin
    if lBits^[num] then
    begin
      if lShowResults then
         write(format('%d, ', [num]));

      count+=1;
    end;

    num+=2;
  end;

  if lShowResults then
    writeln;

  writeln(format('passes: %d, Time: %f s, Avg: %.6f ms, Limit: %d, Count1: %d, Count2: %d, Valid: %s',[
                 lPasses,
                 lDuration,
                 lDuration*1000/lPasses,
                 lSieveSize,
                 count,
                 CountPrimes(lSieveSize, lBits),
                 TrueFalse[validateResults(lResultsDictionary, lSieveSize, lBits)]]));
  writeln(format('olivierbrun;%d;%f;%d;algorithm=base,faithful=yes,bits=1', [lPasses, lDuration, lThreadsRequested]));
end;

var
  sieveThreadParam: TSieveThreadParam;

  secondsRequested,
  threadsRequested: longint;

  timeStart: int64;

  resultsDictionary: TResultsDictionary;
  elapsedTime: double;

begin
  GetRequested(threadsRequested, secondsRequested, sieveThreadParam.sieveSize);
  FillValidateResults(resultsDictionary);
  sieveThreadParam.passes:=0;
  sieveThreadParam.runningThreads:=0;
  sieveThreadParam.terminateThreads:=false;
  sieveThreadParam.threadStarted:=false;
  sieveThreadParam.bits:=nil;

  timeStart:=GetTickCount64;

  while true do
  begin
    if sieveThreadParam.runningThreads<threadsRequested then
    begin
      BeginThread(@sieveThread, @sieveThreadParam); // spawn a new thread
      repeat until sieveThreadParam.threadStarted; // waiting until the thread has started to ensure the count of running threads is updated
      sieveThreadParam.threadStarted:=false;
    end;

    elapsedTime:=(GetTickCount64-timeStart)/1000;

    if elapsedTime>=secondsRequested then
    begin
      sieveThreadParam.terminateThreads:=true;

      //waiting for all running threads to complete
      repeat
      until sieveThreadParam.runningThreads=0;

      elapsedTime:=(GetTickCount64-timeStart)/1000;
      break;
    end;
  end;

  PrintResults(false, elapsedTime, sieveThreadParam.passes, sieveThreadParam.sieveSize, sieveThreadParam.bits, resultsDictionary, threadsRequested);
  FreeMem(sieveThreadParam.bits);
  resultsDictionary.Free;
end.
