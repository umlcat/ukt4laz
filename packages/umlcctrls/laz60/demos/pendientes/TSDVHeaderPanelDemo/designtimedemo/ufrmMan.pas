unit ufrmMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  vclsdvHeaderPanels, vclsdvPanels;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    Control: TSDVHeaderPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    IsActivated: Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IsActivated := false;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if (not IsActivated) then
  begin
    IsActivated := true;
    Control.ActivateFirst;
  end;
end;

end.
