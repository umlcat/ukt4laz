unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  vclsdvInputDlgs, ComCtrls;

type
  TfrmMain = class(TForm)
    lbMessage: TLabel;
    btnExit: TBitBtn;
    btnInputMemo: TButton;
    mmMessage: TMemo;
    sbStatusBar: TStatusBar;
    procedure btnInputMemoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnInputMemoClick(Sender: TObject);
var Answer: TStrings;
begin
  Answer := TStringList.Create;

  if (InputMemo('Informacion Personal', 'Dame tu nombre:', Answer))
    then mmMessage.Text := Answer.Text;

  Answer.Free;
end;

end.
