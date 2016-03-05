unit HiResStopWatch;

interface

uses
  System.Diagnostics,
  System.SysUtils;

type
  THiResStopWatch = record
  private
    FMsStopWatch: TStopWatch;
    FRepeatCount: integer;
    FRunning: boolean;
    FStart: int64;
    FStop: int64;
  public
    class function StartNew(LowestTimingOfXAttempts: integer = 10): THiResStopWatch; static;
    procedure Stop;
    function GetElapsedTicks: Int64; overload;
    function GetElapsedTicks(divider: int64; SubtractLoop: boolean = true): Int64; overload;

    //Todo, make Sample generic, so it can accept Procs with arguments.
    class function Sample(A: TProc; repeats: integer; EmptyProc: TProc = nil;
                          LowestTimingOfXAttempts: integer = 10): int64; static;
    function GetElapsedMilliseconds: int64;
    property ElapsedTicks: int64 read GetElapsedTicks;
    property ElapsedMilliseconds: int64 read GetElapsedMilliseconds;
  end;

implementation

function Min(a, b: Int64): int64; inline;
begin
  if a > b then Result:= b else Result:= a;
end;

{ THiResStopWatch }

function RDTSC: Int64;
asm
  xor eax,eax
  push rbx
  cpuid      //serialize RDTSC so it does not run out of order.
  pop rbx
  rdtsc
  shl rdx,32
  or rax,rdx
end;


function THiResStopWatch.GetElapsedMilliseconds: int64;
begin
  Assert(not(FRunning),'Do not call while running, you''ll skew the timings');
  Result:= FMsStopWatch.ElapsedMilliseconds;
end;

function THiResStopWatch.GetElapsedTicks: Int64;
begin
  if FRunning then FStop:= RDTSC;
  Result:= FStop - FStart;
end;

function THiResStopWatch.GetElapsedTicks(divider: int64; SubtractLoop: boolean = true): Int64;
var
  Timer: THiResStopWatch;
  EmptyTicks: Int64;
  a,i,d: integer;
  NormalTime, EmptyLoopTime: int64;
begin
  NormalTime:= (FStop - FStart) div divider;
  if SubtractLoop then begin

  EmptyTicks:= MaxInt;
  //Time empty loop.
  for a:= 1 to FRepeatCount do begin   //always lowest of 10 times.
    Timer:= THiResStopwatch.StartNew;
    for i:= 1 to divider do begin
      d:= not(d);
    end;
    Timer.Stop;
    EmptyTicks:= Min(EmptyTicks, Timer.GetElapsedTicks);
  end;
  EmptyLoopTime:= EmptyTicks div divider;
  end else EmptyLoopTime:= 0;
  Result:= NormalTime - EmptyLoopTime;
end;

class function THiResStopWatch.Sample(A: TProc; repeats: integer; EmptyProc: TProc = nil;
  LowestTimingOfXAttempts: integer = 10): int64;
var
  i, j: integer;
  MyStopWatch: THiResStopWatch;
  LowestTime, CurrentTime: int64;
begin
  LowestTime:= UInt64(-1) shr 1;  //Maxint64
  for j:= 1 to LowestTimingOfXAttempts do begin

    MyStopWatch:= THiResStopWatch.StartNew(LowestTimingOfXAttempts);
    for i:= 1 to repeats do A;
    MyStopWatch.Stop;
    CurrentTime:= MyStopWatch.GetElapsedTicks(repeats, not(Assigned(EmptyProc)));
    LowestTime:= Min(LowestTime, CurrentTime);
  end;
  Result:= LowestTime;
  if Assigned(EmptyProc) then begin
    LowestTime:= UInt64(-1) shr 1;
    for j:= 1 to LowestTimingOfXAttempts do begin

      MyStopWatch:= THiResStopWatch.StartNew(LowestTimingOfXAttempts);
      for i:= 1 to repeats do EmptyProc;
      MyStopWatch.Stop;
      CurrentTime:= MyStopWatch.GetElapsedTicks(repeats);
      LowestTime:= Min(LowestTime, CurrentTime);
    end;
    Result:= Result - LowestTime;
  end;
end;

class function THiResStopWatch.StartNew(LowestTimingOfXAttempts: integer = 10): THiResStopWatch;
begin
  Result.FStop:= 0;
  Result.FRunning:= true;
  Result.FRepeatCount:= LowestTimingOfXAttempts;
  Result.FMsStopWatch:= TStopWatch.StartNew;
  Result.FStart:= RDTSC;
end;

procedure THiResStopWatch.Stop;
begin
  FStop:= RDTSC;
  FMsStopWatch.Stop;
  FRunning:= false;
end;

end.
