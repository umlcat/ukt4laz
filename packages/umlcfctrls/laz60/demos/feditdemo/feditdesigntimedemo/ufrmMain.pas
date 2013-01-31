unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  //sdvMsgCtrls,
  sdvcomponents,
  sdvedits, sdvfmngrs, sdvfctrls, sdvmemos;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    Manager: TSDVFocusMngr;
    SDVFocusEdit1: TSDVFocusEdit;
    SDVFocusEdit2: TSDVFocusEdit;
    SDVFocusEdit3: TSDVFocusEdit;
    SDVFocusEdit4: TSDVFocusEdit;
    SDVFocusMemo1: TSDVFocusMemo;
    SDVFocusMemo2: TSDVFocusMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    NeverActivated: Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  NeverActivated := true;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if (NeverActivated) then
  begin
    NeverActivated := false;

    Manager.Refresh;
  end;
end;

end.
