program PrimePas;

{$IFDEF FPC}
  {$MODE OBJFPC}  
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
	{$IFDEF FPC}
	SysUtils,
	dateutils,
	{$ELSE}
	System.SysUtils,
	Windows,
	{$ENDIF}
	Classes,
	Math;

type
	TPrimeSieve = class
	private
		FSieveSize: Integer;
		FBitArray: array of ByteBool; {ByteBool: 4644. WordBool: 4232. LongBool: 3673}
		{FMyDict: TDictionary<Integer, Integer>;}
		FMyList: array of Integer;

		function GetBit(Index: Integer): Boolean;
		procedure ClearBit(Index: Integer);
		procedure InitializeBits;

		function ArrayIndex( Size: Integer ): Integer;
	public
		constructor Create(Size: Integer);
		destructor Destroy; override;

		procedure RunSieve; // Calculate the primes up to the specified limit

		function CountPrimes: Integer;
		function ValidateResults: Boolean;

		procedure PrintResults(ShowResults: Boolean; Duration: Double; Passes: Integer);
	end;

{ TPrimeSieve }

constructor TPrimeSieve.Create(Size: Integer);
begin
	inherited Create;

	FSieveSize := Size;

	//The optimization here is that we only store bits for *odd* numbers.
	// So FBitArray[3] is if 3 is prime
	// and FBitArray[4] is if 5 is prime
	//GetBit and SetBit do the work of "div 2"
	SetLength(FBitArray, (FSieveSize+1) div 2);
	InitializeBits;

	//FMyDict := TDictionary<Integer, Integer>.Create;
	SetLength(FMyList, 9);


	// Historical data for validating our results - the number of primes
	// to be found under some limit, such as 168 primes under 1000

	// Instead of using a Dictionary/Map here, we are going to use a simple array, 
	// since we can easily predict the 9 elements and calculate the required index 
	// with Log10/Pow if need be. This is primarly done to avoid needing external 
	// libraries, rather than to speed up the code. (even though it may do so).

	FMyList[0] := 0;
	FMyList[1] := 4;       // 10
	FMyList[2] := 25;      // 100
	FMyList[3] := 168;     // 1000
	FMyList[4] := 1229;    // 10000
	FMyList[5] := 9592;    // 100000
	FMyList[6] := 78498;   // 1000000  
	FMyList[7] := 664579;  // 10000000
	FMyList[8] := 5761455; // 100000000
end;

destructor TPrimeSieve.Destroy;
begin
	inherited;
end;

function TPrimeSieve.ArrayIndex( Size: Integer ): Integer;
begin
	if (Size=0) then
		Result := 0
	else
		Result := trunc(Log10(Size));
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
var
	idx: Integer;
begin
	// if FSieveSize is a whole number below our array size, then it will be in our array
	idx := trunc(Log10(FSieveSize));
	if (idx <= length(FMyList)) then
		Result := FMyList[ idx ] = Self.CountPrimes
	else
		Result := False;
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
		Writeln('You are setting even bits, which is sub-optimal');
		Exit;
	end;

	{Any compiler worth its salt converts "div 2" into "shr 1".
	In this case Delphi is worth it's salt; emitting "sar".
	(But don't forget: Delphi still can't hoist loop variables)}
	FBitArray[Index div 2] := False;
end;


procedure TPrimeSieve.RunSieve;
var
	factor: Integer;
	q: Integer;
	num: Integer;
begin
	factor := 3;
	q := Floor(Sqrt(FSieveSize));

	while (factor <= q) do
	begin
		// Refactored to while loop so we are not limited to incrementing by 1, based on CCP code from @DavePL rather than python
		num := factor;
		while ( num < FSieveSize ) do
		begin
			if GetBit(num) then
			begin
				factor := num;
				Break;
			end;
			Inc(num, 2);
		end;

		// Refactored to while loop so we are not limited to incrementing by 1, based on CCP code from @DavePL rather than python
		num := factor * factor;
		while ( num < FSieveSize ) do 
		begin
			FBitArray[num div 2] := false;
			inc(num, factor*2);
		end;

		Inc(factor, 2);
	end;
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

procedure Main;
var
	sieve: TPrimeSieve;
	dtStart: TDateTime;
	passes: Integer;
	tD: TDateTime;
begin
	dtStart := Now();
	passes := 0;

	sieve := nil;
	sieve := TPrimeSieve.Create(1000000);

	while SecondsBetween( Now(), dtStart ) < 5 do
	begin
		//if Assigned(sieve) then
		//sieve.Free;

		//sieve := TPrimeSieve.Create(1000000);
		sieve.RunSieve;
		Inc(passes);
	end;

	tD := SecondsBetween( Now(), dtStart );
	if Assigned(sieve) then
		sieve.PrintResults(False, tD, passes);
end;

{
	Intel Core i5-9400 @ 2.90 GHz
	- 32-bit: 4,809 passes
	- 64-bit: 2,587 passes

	FPC Intel Core i9-9900K @ 3.6Ghz
	- 64-bit: 2,934 passes 
}
begin
	try
		Main;
	except
		on E: Exception do
			Writeln(E.ClassName, ': ', E.Message);
	end;
end.
