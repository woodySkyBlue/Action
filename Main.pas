unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetMainText: string;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses Sub;

function TFormMain.GetMainText: string;
begin
  Result := Edit1.Text;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  var FormSub := TFormSub.Create(Self);
  try
    FormSub.Edit1.Text := '111';
    FormSub.OnGetMainText := GetMainText;  // メソッドを直接割り当て
    FormSub.ShowModal;
  finally
    FormSub.Free;
  end;
end;

end.
