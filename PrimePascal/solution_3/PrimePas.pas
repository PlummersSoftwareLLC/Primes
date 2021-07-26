program PrimePas;

{$mode objfpc}{$H+}

uses
  SysUtils, fgl; //Classes, fgl;

type
  { prime_sieve }
  TResultsDictionary = specialize TFPGMap<longint, longint>;
  TPackedBits = bitpacked array [0..high(longint)] of boolean;
  PPackedBits = ^TPackedBits;

  prime_sieve = class
    private
      sieveSize: longint;
      bits: PPackedBits;

      resultsDictionary: TResultsDictionary; static;
      passes: longint; static;
      tStart: int64; static;
      elapsedTime: double; static;

      function validateResults: boolean;

    public
      constructor Create(n: longint);
      destructor Destroy; override;

      procedure runSieve;
      procedure printResults(showResults: boolean; duration: double; lPasses: longint);
      function countPrimes: longint;
  end;

{ prime_sieve }

function prime_sieve.validateResults: boolean;
var
  index: longint;

begin
  if resultsDictionary.Find(sieveSize, index) then
    result:=resultsDictionary.Data[index]=countPrimes
  else
    result:=false;
end;

constructor prime_sieve.Create(n: longint);
var
  nbBytes: longint;

begin
  if not assigned(resultsDictionary) then
  begin
    resultsDictionary:=TResultsDictionary.Create;
    resultsDictionary.Sorted:=true;
    resultsDictionary.Add(10, 4);
    resultsDictionary.Add(100, 25);
    resultsDictionary.Add(1000, 168);
    resultsDictionary.Add(10000, 1229);
    resultsDictionary.Add(100000, 9592);
    resultsDictionary.Add(1000000, 78498);
    resultsDictionary.Add(10000000, 664579);
    resultsDictionary.Add(100000000, 5761455);
  end;

  sieveSize:=n;
  nbBytes:=n div 8 + ord(Frac(n/8)>0); // calculating the number of bytes to allocate on the heap for the bits; 1 byte is added in the case where n is not a multiple of 8
  bits:=GetMem(nbBytes); // allocating memory to store the bits
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
  factor,
  q: longint;

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

    while num<sieveSize do
    begin
      bits^[num]:=false;
      num+=factor*2;
    end;

    factor+=2;
  end;
end;

procedure prime_sieve.printResults(showResults: boolean; duration: double; lPasses: longint);
const
  TrueFalse: array[boolean] of string = ('false', 'true');
  
var
  num,
  count: longint;

begin
  if sieveSize>=2 then
  begin
    if showResults then
      write('2, ');

    count:=1;
  end
  else
    count:=0;

  num:=3;

  while num<sieveSize do
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
  begin
    writeln;
    writeln(format('passes: %d, Time: %f, Avg: %.6f, Limit: %d, Count1: %d, Count2: %d, Valid: %s',[
                   lPasses,
                   duration,
                   duration/lPasses,
                   sieveSize,
                   count,
                   countPrimes,
                   TrueFalse[validateResults]]));
    writeln;
  end;
  
  writeln(format('olivierbrun;%d;%f;1;algorithm=base,faithful=yes,bits=1', [lPasses, duration]));
end;

function prime_sieve.countPrimes: longint;
var
  i,
  count: longint;

begin
  if sieveSize>=2 then
    count:=1
  else
    count:=0;

  i:=3;

  while i<sieveSize do
  begin
    if bits^[i] then
      count+=1;

    i+=2;
  end;

  result:=count;
end;

var
  sieve: prime_sieve;

begin
  sieve:=nil;
  prime_sieve.passes:=0;
  prime_sieve.tStart:=GetTickCount64;

  while true do
  begin
    if assigned(sieve) then
       sieve.Free;

    sieve:=prime_sieve.Create(1000000);
    sieve.runSieve;
    prime_sieve.passes+=1;
    prime_sieve.elapsedTime:=(GetTickCount64-prime_sieve.tStart)/1000;

    if prime_sieve.elapsedTime>=5 then
    begin
      sieve.printResults(false, prime_sieve.elapsedTime, prime_sieve.passes);
      break;
    end;
  end;

  sieve.Free;
  prime_sieve.resultsDictionary.Free;
end.

