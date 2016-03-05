unit Sample;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm37 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure EmptyProc;
    procedure SampleProcA;
    procedure SampleProcB;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form37: TForm37;

implementation

{$R *.dfm}

uses
  HiResStopwatch;

procedure TForm37.EmptyProc;
begin
  Random(255);
end;

procedure TForm37.SampleProcA;
var
  a: integer;
begin
  //Force jump mispredictions;
  if Random(255) > 128 then Exit
  else a:= 1;
end;

procedure TForm37.SampleProcB;
var
  a: integer;
begin
  //Jumps are always correctly predicted.
  if Random(255) > 0 then Exit
  else a:= 1;
end;

procedure TForm37.Button1Click(Sender: TObject);
const
  Repeats = 100;
var
  Timer: THiResStopWatch;
  Time1, Time2: int64;
begin
  //Anonymous procs work
  Time1:= Timer.Sample(procedure begin Sleep(1); end, Repeats, nil ,1);
  Time2:= Timer.Sample(procedure begin Sleep(2); end, Repeats, nil ,1);
  //But timing lengty things are better done in Ms.
  Memo1.Lines.Add(Format('1 millisec takes %.0n cycles',[time1 / 1]));
  Memo1.Lines.Add(Format('2 millisecs take %.0n cycles',[time2 / 1]));
  Memo1.Lines.Add('');
  Memo1.Lines.Add(Format('Your CPU runs at %0.2n Ghz',[time1 / (1000*1000)]));
  Memo1.Lines.Add('');
  //Timing small procs with large repeat counts
  Time1:= Timer.Sample(SampleProcA, Repeats*10000, EmptyProc);
  Time2:= Timer.Sample(SampleProcB, Repeats*10000, EmptyProc);
  //Gives very accurate timings.
  Memo1.Lines.Add(Format('Misprediction takes %d cycles',[time1]));
  Memo1.Lines.Add(Format('Correct prediction takes %d cycles',[time2]));
end;

end.
