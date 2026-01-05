program TestAction;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  Sub in 'Sub.pas' {FormSub};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
