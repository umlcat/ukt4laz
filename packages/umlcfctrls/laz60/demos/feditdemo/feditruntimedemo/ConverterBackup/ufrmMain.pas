unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  sdvcomponents,
  //sdvMsgCtrls,
  sdvfmngrs, sdvfctrls;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnConnect: TButton;
    Manager: TSDVFocusMngr;
    btnDisconnect: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Control: TSDVFocusEdit;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  Control := TSDVFocusEdit.Create(Self);
  Control.Top  := 100;
  Control.Left := 100;
  Control.Manager := Manager;

  Self.InsertControl(Control);
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  Control.Manager := nil;
  Control.Free;
end;

end.
