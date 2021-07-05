///  Modified Jake Tapper's submission:
///  Removed the validation code into a separate static class.
///  Inherited the class from TThread for multi-threaded execution called
///  from a parallel loop in Main procedure.
///  Call with "PrmHelp h" for usage instructions
program PrmDelphi;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Classes,
  System.Generics.Collections,
  System.Timespan,
  Math,
  Windows,
  System.Threading;

type
  TPrimeValidator = class
  private
    class var FMyDict: TDictionary<Integer, Integer>;
  public
    class function Validate(aSieveSize, aPrimeCount: Integer): Boolean;
    class constructor Create;
    class destructor Destroy;
  end;

	TPrimeSieve = class(TThread)
	private
		FSieveSize: Integer;
		FBitArray: array of ByteBool; //ByteBool: 4644. WordBool: 4232. LongBool: 3673
    FReady: Boolean;
		function GetBit(Index: Integer): Boolean;
		procedure ClearBit(Index: Integer);
		procedure InitializeBits;
	public
		constructor Create(Size: Integer; OnTerminateFree: Boolean = True);
		destructor Destroy; override;
		procedure Execute; override; // Calculate the primes up to the specified limit
		function CountPrimes: Integer;
		function ValidateResults: Boolean;
		procedure PrintResults(ShowResults: Boolean; Duration: Double; Passes: Integer);
    property Ready: Boolean read FReady;
	end;

var
	passes: Integer;

  { TPrimeSieve }

constructor TPrimeSieve.Create(Size: Integer; OnTerminateFree: Boolean = True);
begin
	inherited Create(False);
  FreeOnTerminate := OnTerminateFree;
  FReady := False;
	FSieveSize := Size;
	//The optimization here is that we only store bits for *odd* numbers.
	// So FBitArray[3] is if 3 is prime
	// and FBitArray[4] is if 5 is prime
	//GetBit and SetBit do the work of "div 2"
	SetLength(FBitArray, (FSieveSize+1) div 2);
	InitializeBits;
end;

destructor TPrimeSieve.Destroy;
begin
  AtomicIncrement(passes);
  inherited;
end;

function TPrimeSieve.CountPrimes: Integer;
var
	count: Integer;
	i: Integer;
begin
	count := 0;
	for i := 0 to High(FBitArray) do
	begin
		if FBitArray[i] then
			Inc(count);
	end;
	Result := count;
end;

function TPrimeSieve.ValidateResults: Boolean;
begin
  Result := TPrimeValidator.Validate(FSieveSize, Self.CountPrimes);
end;

function TPrimeSieve.GetBit(Index: Integer): Boolean;
begin
	if (Index mod 2 = 0) then
	begin
		Result := False;
		Exit;
	end;
	Result := FBitArray[Index div 2];
end;

procedure TPrimeSieve.InitializeBits;
var
	i: Integer;
	remaining: Integer;
begin
	remaining := Length(FBitArray);
	i := 0;
	while remaining >= 8 do
	begin
		FBitArray[i]   := True;
		FBitArray[i+1] := True;
		FBitArray[i+2] := True;
		FBitArray[i+3] := True;
		FBitArray[i+4] := True;
		FBitArray[i+5] := True;
		FBitArray[i+6] := True;
		FBitArray[i+7] := True;
		Inc(i, 8);
		Dec(remaining, 8);
	end;
	while remaining > 0 do
	begin
		FBitArray[i] := True;
		Inc(i);
		Dec(remaining);
	end;
end;

procedure TPrimeSieve.ClearBit(Index: Integer);
begin
{
	Profiling shows 99% of the execution is spent here in ClearBit.

	Testing index using and 1:        4387 passes
	Testing index using mod 2:        4644 passes
			WriteLine with inline text: 4644 passes
			WriteLine with const  text: 4636 passes
			No WriteLine:               4840 passes
	Omit testing of index:            5280 passes

	Which is surprising, as you'd think the branch predictor would realize this branch is **never** taken.
}
	if (Index mod 2) = 0 then
	begin
//		Writeln('You are setting even bits, which is sub-optimal');
		Exit;
	end;
	//Any compiler worth its salt converts "div 2" into "shr 1".
	//In this case Delphi is worth it's salt; emitting "sar".
	//(But don't forget: Delphi still can't hoist loop variables)
	FBitArray[Index div 2] := False;
end;

procedure TPrimeSieve.Execute;
var
	factor: Integer;
	q: Integer;
	num: Integer;
begin
	factor := 3;
	q := Floor(Sqrt(FSieveSize));
	while (factor <= q) do
	begin
		for num := factor to FSieveSize do
		begin
			if GetBit(num) then
			begin
				factor := num;
				Break;
			end;
		end;
		// If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
		// We can then step by factor * 2 because every second one is going to be even by definition
		num := factor*3;
		while num <= FSieveSize do
		begin
			ClearBit(num);
			Inc(num, factor*2);
		end;
		Inc(factor, 2);
	end;
  FReady := True;
end;

procedure TPrimeSieve.PrintResults(ShowResults: Boolean; Duration: Double; Passes: Integer);
var
	count: Integer;
	num: Integer;
const
	SYesNo: array[Boolean] of string = ('No', 'Yes');
begin
	if ShowResults then
		Write('2, ');
	count := 1;
	for num := 3 to FSieveSize do
	begin
		if GetBit(num) then
		begin
			if ShowResults then
				Write(IntToStr(num) + ', ');
			Inc(count);
		end;
	end;
	if ShowResults then
		WriteLn('');
	WriteLn(Format('Passes: %d, Time: %.3f sec, Avg: %.4f ms, Limit: %d, Count: %d, Valid: %s',
			[Passes, Duration, Duration/Passes*1000, FSieveSize, count, SYesNo[ValidateResults]]));
end;

{ TPrimeValidator }

class constructor TPrimeValidator.Create;
begin
	FMyDict := TDictionary<Integer, Integer>.Create;
	// Historical data for validating our results - the number of primes
	// to be found under some limit, such as 168 primes under 1000
	FMyDict.Add(       10, 4); //nobody noticed that 1 is wrong? [2, 3, 5, 7]
	FMyDict.Add(      100, 25);
	FMyDict.Add(     1000, 168);
	FMyDict.Add(    10000, 1229);
	FMyDict.Add(   100000, 9592);
	FMyDict.Add(  1000000, 78498);
	FMyDict.Add( 10000000, 664579);
	FMyDict.Add(100000000, 5761455);
end;

class destructor TPrimeValidator.Destroy;
begin
  FMyDict.Free;
  inherited;
end;

class function TPrimeValidator.Validate(aSieveSize, aPrimeCount: Integer):
    Boolean;
begin
	if FMyDict.ContainsKey(aSieveSize) then
		Result := FMyDict[aSieveSize] = aPrimeCount
	else
		Result := False;
end;

/// Optimized time measurements by using system tick count reducing function call
/// overhead in the parallel loop.
procedure Main(RunDuration: Integer; Lag: Integer);
var
	sieve: TPrimeSieve;
	dtStart: TDateTime;
	tD: TTimeSpan;
  StartTicks: Cardinal;
  MaxTicks: Cardinal;
const
  TicksPerSecond = 1000;
begin
  if RunDuration <= 0 then
    Exit;
  WriteLn(sLineBreak, 'Starting ', RunDuration,
          ' second run with ', Lag, ' millisecond thread lag ...', sLineBreak);
	dtStart := Now;
  StartTicks := GetTickCount;
	passes := 0;
  MaxTicks := RunDuration * TicksPerSecond - Lag;
  TParallel.For(0, 1000000,
            procedure(i: Integer; state: System.Threading.TParallel.TLoopState)
            begin
              if (GetTickCount - StartTicks >= MaxTicks) then
                state.Stop
              else
                TPrimeSieve.Create(1000000);
            end);
	tD := TTimeSpan.Subtract(Now, dtStart);
	sieve := TPrimeSieve.Create(1000000, False);
  try
    while not sieve.Ready do
      // Wait for completion ...
    ;
    sieve.PrintResults(False, tD.TotalSeconds, passes);
  finally
    sieve.Free;
  end;
end;

{
	Intel Core i9-8950HK @ 2.90 GHz, 10 second run:

	- 32-bit: 13,000 passes
	- 64-bit: 18,000 passes
}

const
  cHelp = sLineBreak +
'Usage:' + sLineBreak +
'       PrmDelphi [runtime in seconds] [thread lag in milliseconds]' + sLineBreak +
'           Runtime defaults to 10 seconds and' + sLineBreak +
'           thread lag defaults to 50 milliseconds.' + sLineBreak +
'           The thread lag is subtracted from the runtime to allow' + sLineBreak +
'           lagging threads to finish within the allocated time.' + sLineBreak;

begin
  var RunDuration := 10;
  var Lag := 50;
	try
    if ParamCount > 0 then
    begin
      if not TryStrToInt((paramstr(1)), RunDuration) then
      begin
        Writeln(cHelp);
        Exit;
      end;
      if ParamCount > 1 then
      begin
        if not TryStrToInt(ParamStr(2), Lag) then
          Lag := 50;
      end;
    end;
    Main(RunDuration, Lag);
    WriteLn(sLineBreak, 'Press enter to close...');
//    Readln;
	except
		on E: Exception do
			Writeln(E.ClassName, ': ', E.Message);
	end;
end.

