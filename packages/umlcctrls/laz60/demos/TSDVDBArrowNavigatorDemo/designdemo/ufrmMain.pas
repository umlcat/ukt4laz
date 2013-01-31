unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  vclsdvPanels, vclsdvNavs, vclsdvDBNavs, DB, DBTables, Grids, DBGrids;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TButton;
    sbStatusBar: TStatusBar;
    dsAnimals: TDataSource;
    dbgrMain: TDBGrid;
    tbAnimals: TTable;
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
//var Control: TSDVDBNavigator;
begin
(*
  Control := TSDVDBNavigator.Create(Self);
  Control.Align := alBottom;
  Control.DataSource := dsAnimals;
  Control.ButtonsVisible :=
    [btnopFirst, btnopFastPrior, btnopPrior,
    btnopNext, btnopFastNext, btnopLast];

  Self.InsertControl(Control);
*)
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Self.Close;
end;

end.
