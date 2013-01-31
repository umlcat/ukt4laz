unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, CheckLst, ComCtrls,
  vclsdvInputDlgs;

type
  TfrmMain = class(TForm)
    lblMessage: TLabel;
    btnExit: TBitBtn;
    btnInputStrings: TButton;
    sbStatusBar: TStatusBar;
    lbMessage: TListBox;
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

procedure TfrmMain.btnInputStringsClick(Sender: TObject);
var Answer: TStrings;
begin
  Answer := TStringList.Create;
  Answer.Assign(lbMessage.Items);

  if (InputStrings('Contact List', 'Select:', Answer))
    then lbMessage.Items.Assign(Answer);

  Answer.Free;
end;

end.
