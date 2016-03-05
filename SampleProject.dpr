program SampleProject;

uses
  Vcl.Forms,
  Sample in 'Sample.pas' {Form37};

{$R *.res}

{$optimization on}
{$stackframes off}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm37, Form37);
  Application.Run;
end.
