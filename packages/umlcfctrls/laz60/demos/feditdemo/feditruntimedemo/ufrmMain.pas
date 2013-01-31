unit ufrmmain;

interface

uses
  {$IFDEF FPC}
  LCLIntf, LCLType, LMessages,
  {$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons,
  sdvcomponents,
  //sdvMsgCtrls,
  sdvdoscolors,
  sdvfmngrs, sdvfctrls, sdvfedits;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnConnect: TButton;
    Manager: TSDVDOSFocusMngr;
    btnDisconnect: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }

    Control1: TSDVFocusEdit;
    Control2: TSDVFocusEdit;
    Control3: TSDVFocusEdit;
    Control4: TSDVFocusEdit;

    procedure RemoveFocusedControls();
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  RemoveFocusedControls();

  Manager := TSDVDOSFocusMngr.Create(Self);
    //Manager.DefaultFont.Charset := DEFAULT_CHARSET;
    //Manager.DefaultFont.Height := -11;
    //Manager.DefaultFont.Name := 'MS Sans Serif';
    //Manager.DefaultFont.Style := [];
    Manager.DefaultFontColor := dosclBlack;
    Manager.DefaultColor := dosclWhite;

    //Manager.FocusedFont.Charset := DEFAULT_CHARSET;
    //Manager.FocusedFont.Height := -11;
    //Manager.FocusedFont.Name := 'MS Sans Serif';
    //Manager.FocusedFont.Style := [];
    Manager.FocusedFontColor := dosclYellow;
    Manager.FocusedColor := dosclBlue;

    //Manager.DisabledFont.Charset := DEFAULT_CHARSET;
    //Manager.DisabledFont.Height := -11;
    //Manager.DisabledFont.Name := 'MS Sans Serif';
    //Manager.DisabledFont.Style := [];
    Manager.DisabledFontColor := dosclDarkGray;
    Manager.DisabledColor := dosclDarkGray;

    //Manager.ReadOnlyFont.Charset := DEFAULT_CHARSET;
    //Manager.ReadOnlyFont.Height := -11;
    //Manager.ReadOnlyFont.Name := 'MS Sans Serif';
    //Manager.ReadOnlyFont.Style := [];
    Manager.ReadOnlyFontColor := dosclDarkGray;
    Manager.ReadOnlyColor := dosclLightGray;

  Self.InsertComponent(Manager);
  Application.ProcessMessages();

  (*
  Control1 := TSDVFocusEdit.Create(Self);
    Control1.Top   := 100;
    Control1.Left  := 100;
    Control1.Width := 125;
    Control1.Manager := Manager;
    Control1.Text := 'Hello';
  Self.InsertControl(Control1);
  Application.ProcessMessages();

  Control2 := TSDVFocusEdit.Create(Self);
    Control2.Top   := 125;
    Control2.Left  := 100;
    Control2.Width := 125;
    Control2.Manager := Manager;
    Control2.Text := 'World';
  Self.InsertControl(Control2);
  Application.ProcessMessages();

  Control3 := TSDVFocusEdit.Create(Self);
    Control3.Top   := 150;
    Control3.Left  := 100;
    Control3.Width := 125;
    Control3.Manager := Manager;
    Control3.ReadOnly := true;
    Control3.Text := 'Ooops, not here';
  Self.InsertControl(Control3);
  Application.ProcessMessages();

  Control4 := TSDVFocusEdit.Create(Self);
    Control4.Top   := 175;
    Control4.Left  := 100;
    Control4.Width := 125;
    Control4.Manager := Manager;
    Control4.Text := 'Bye';
  Self.InsertControl(Control4);
  Application.ProcessMessages();

  Control1.SetFocus();
  Application.ProcessMessages();
  *)
end;

procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  RemoveFocusedControls();
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  RemoveFocusedControls();
end;

procedure TfrmMain.RemoveFocusedControls();
begin
  if (Control4 <> nil) then
  begin
    Control4.Manager := nil;
    Application.ProcessMessages();
    Self.RemoveControl(Control4);
    Application.ProcessMessages();
    Control4.Free();
    Control4 := nil;
  end;

  if (Control3 <> nil) then
  begin
    Control3.Manager := nil;
    Application.ProcessMessages();
    Self.RemoveControl(Control3);
    Application.ProcessMessages();
    Control3.Free();
    Control3 := nil;
  end;

  if (Control2 <> nil) then
  begin
    Control2.Manager := nil;
    Application.ProcessMessages();
    Self.RemoveControl(Control2);
    Application.ProcessMessages();
    Control2.Free();
    Control2 := nil;
  end;

  if (Control1 <> nil) then
  begin
    Control1.Manager := nil;
    Application.ProcessMessages();
    Self.RemoveControl(Control1);
    Application.ProcessMessages();
    Control1.Free();
    Control1 := nil;
  end;

  if (Manager <> nil) then
  begin
    Self.RemoveComponent(Manager);
    Application.ProcessMessages();
    Manager.Free();
    Manager := nil;
  end;
end;

end.
