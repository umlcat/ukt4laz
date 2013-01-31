unit ufrmSearchDlgDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, sdvComponents,
  vclsdvDlgCtrls, vclsdvSrchDlgs, vclsdvSrchTxtDlgs;

type
  TfrmSearchDlgDemo = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    SearchDlg: TSDVSearchTextDialog;
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSearchDlgDemo: TfrmSearchDlgDemo;

implementation

{$R *.dfm}

procedure TfrmSearchDlgDemo.btnTestClick(Sender: TObject);
begin
  SearchDlg.Execute;
end;

end.
