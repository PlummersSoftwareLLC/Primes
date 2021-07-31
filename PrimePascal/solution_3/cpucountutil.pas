// utility to retrieve the number of cores availble on a machine
unit CPUCountUtil;

{$mode objfpc}{$H+}

interface

uses
  {$IF defined(linux)}
  ctypes,
  {$ELSEIF defined(freebsd) or defined(darwin)}
  ctypes, sysctl,
  {$ELSEIF defined(windows)}
  Windows,
    {$IFDEF UseTProcessW}
    pipes,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils;

function GetSystemThreadCount: integer;

{$IFDEF Linux}
function sysconf(i: cint): clong; cdecl; external name 'sysconf';
const _SC_NPROCESSORS_ONLN = 83;
{$ENDIF}

implementation

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
  test: int64;
begin
  QueryPerformanceCounter(test);
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
end.

