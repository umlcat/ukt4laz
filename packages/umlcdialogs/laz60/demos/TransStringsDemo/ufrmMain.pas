unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  vclsdvTransDlgs;

type
  TfrmMain = class(TForm)
    edMessage: TEdit;
    lbMessage: TLabel;
    btnInputStr: TButton;
    btnExit: TBitBtn;
    btnInputStrings: TButton;
    procedure btnInputStrClick(Sender: TObject);
    procedure btnInputStringsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnInputStrClick(Sender: TObject);
var Answer: string;
begin
  if (TranslateString
  ('Informacion Personal', 'Nombre anterior:', 'Nuevo Nombre:', Answer))
    then edMessage.Text := Answer;
end;

procedure TfrmMain.btnInputStringsClick(Sender: TObject);
var Answer: TStrings;
begin
  Answer := TStringList.Create;
  Answer.Text := edMessage.Text;

  if (TranslateStrings
  ('Informacion Personal', 'Nombre anterior:', 'Nuevo Nombre:', Answer))
    then edMessage.Text := Answer.Text;
  Answer.Free;
end;

end.
