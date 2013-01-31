unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, vclsdvNavs, vclsdvPanels;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TButton;
    sbStatusBar: TStatusBar;
    SDVNavigator1: TSDVNavigator;
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
//var Control: TSDVNavigator;
begin
//  Control := TSDVNavigator.Create(Self);
//  Control.Align := alBottom;

//  Self.InsertControl(Control);
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Self.Close;
end;

end.
