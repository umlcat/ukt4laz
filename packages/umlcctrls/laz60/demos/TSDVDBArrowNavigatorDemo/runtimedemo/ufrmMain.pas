unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, vclsdvNavs, DB, DBTables,
  vclsdvPanels, vclsdvDBNavs, Grids, DBGrids;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TButton;
    sbStatusBar: TStatusBar;
    DBGrid1: TDBGrid;
    Navigator: TSDVDBNavigator;
    dsAnimals: TDataSource;
    tbAnimals: TTable;
    btnOpen: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure CloseTable;
    procedure RefreshCtrls;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
//var Control: TSDVNavigator;
begin
(*
  Control := TSDVNavigator.Create(Self);
  Control.Align := alBottom;

  Self.InsertControl(Control);
  *)
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.CloseTable;
begin
  tbAnimals.Close;
  RefreshCtrls;  
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  CloseTable;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  tbAnimals.Open;
  RefreshCtrls;  
end;

procedure TfrmMain.RefreshCtrls;
var IsActive: Boolean;
begin
  IsActive := tbAnimals.Active;
  btnOpen.Enabled := not IsActive;
  btnClose.Enabled := IsActive;
  btnExit.Enabled := not IsActive;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  RefreshCtrls;
end;

end.
