unit ufrmMan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls,
  vclsdvHeaderPanels;

type
  TfrmMain = class(TForm)
    sbStatusBar: TStatusBar;
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnToogle: TButton;
    procedure btnToogleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    IsActivated: Boolean;

    Control: TCustomSDVHeaderPanel;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnToogleClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IsActivated := false;
  Control := TCustomSDVHeaderPanel.Create(Self);
  Control.Top  := 50;
  Control.Left := 50;
  Control.Text := 'Hello World';

  Self.InsertControl(Control);
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
