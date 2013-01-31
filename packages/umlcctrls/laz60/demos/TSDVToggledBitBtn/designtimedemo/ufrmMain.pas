unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  vclsdvToggleBitBtns;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnDemo: TSDVToggledBitBtn;
    procedure FormActivate(Sender: TObject);
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

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  btnDemo.Activated;
end;

procedure TfrmMain.btnDemoClick(Sender: TObject);
begin
  if (btnDemo.Toggled) then
  begin
     ShowMessage('Toggled: true')
  end else
  begin
    ShowMessage('Toggled: false');
  end;  
end;

end.
