unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  sdvpaneltabctrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TBitBtn;
    sbStatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Activated: Boolean;

    procedure OnTabEnterControlClick
      (TabControl: tcustomsdvtabcontrol; TabIndex: Integer);
    procedure OnTabExitControlClick
      (TabControl: tcustomsdvtabcontrol; TabIndex: Integer);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Activated := FALSE;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var control: tsdvtabcontrol;
begin
  if (not Activated) then
  begin
    Activated := TRUE;

    control := tsdvtabcontrol.Create(Self);
      control.Align := alClient;
      control.OnTabEnter := {@}OnTabEnterControlClick;
      control.OnTabExit  := {@}OnTabExitControlClick;
    Self.InsertControl(control);

    control.Tabs.Add('Hello.txt');
    control.Tabs.Add('World.txt');
    control.Tabs.Add('Readme.txt');    
  end;
end;

procedure TfrmMain.OnTabEnterControlClick
  (TabControl: tcustomsdvtabcontrol; TabIndex: Integer);
begin
  ShowMessage('Enter:' + TabControl.Tabs[TabIndex]);
end;

procedure TfrmMain.OnTabExitControlClick
  (TabControl: tcustomsdvtabcontrol; TabIndex: Integer);
begin
  ShowMessage('Exit:' + TabControl.Tabs[TabIndex]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//
end;

end.
