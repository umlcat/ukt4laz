unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Buttons,
  vclsdvOptionDlgs;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnDemo: TButton;
    procedure btnDemoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnDemoClick(Sender: TObject);
var Answer: Integer;
begin
  Answer := 0;
  if (vclsdvOptionDlgs.InputOption
    ('Hello World', 'Select an option:',
     ['PBuilder', 'CBuilder', 'JBuilder', 'C#Builder'], Answer)) then
  begin
    ShowMessage(IntToStr(Answer));
  end;
end;

end.
