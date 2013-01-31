unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  sdvtabctrls;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TBitBtn;
    sbStatusBar: TStatusBar;
    pcMain: TPageControl;
    tbshOne: TTabSheet;
    tbshTwo: TTabSheet;
    tbshThree: TTabSheet;
    tbshFour: TTabSheet;
    tbshFive: TTabSheet;
    mmOne: TMemo;
    mmTwo: TMemo;
    mmThree: TMemo;
    mmFour: TMemo;
    mmFive: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tabcontrolEnterTab(TabControl: tcustomsdvtabcontrol;
      TabIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }

    Activated: Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Activated := FALSE;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if (not Activated) then
  begin
    Activated := TRUE;
    pcMain.Activate;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  pcMain.Deactivate;
end;

procedure TfrmMain.tabcontrolEnterTab(TabControl: tcustomsdvtabcontrol;
  TabIndex: Integer);
begin
  pcMain.ActivePageIndex := TabIndex;
end;

end.
