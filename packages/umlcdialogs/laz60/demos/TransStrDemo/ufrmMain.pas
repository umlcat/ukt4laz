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
    procedure btnInputStrClick(Sender: TObject);
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
  Answer := edMessage.Text;
  if TranslateString
    ('Informacion Personal', 'Nombre Actual', 'Nuevo Nombre:', Answer)
    then edMessage.Text := Answer;
end;

end.
