unit Sub;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TGetMainTextEvent = function: string of object;

  TFormSub = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FOnGetMainText: TGetMainTextEvent;
    procedure DoGetMainText;
  public
    property OnGetMainText: TGetMainTextEvent read FOnGetMainText write FOnGetMainText;
  end;

var
  FormSub: TFormSub;

implementation

{$R *.dfm}

procedure TFormSub.DoGetMainText;
begin
  if Assigned(FOnGetMainText) then
    Edit1.Text := FOnGetMainText();
end;

procedure TFormSub.Button1Click(Sender: TObject);
begin
  DoGetMainText;
end;

end.
