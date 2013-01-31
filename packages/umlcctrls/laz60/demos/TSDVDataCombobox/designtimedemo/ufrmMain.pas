unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Math, vclsdvItemCombos,
  vclsdvCombos;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    Control: TSDVIntegerCombobox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }

    NeverActivated: Boolean;
  public
    { Public declarations }
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
    NeverActivated := true;

    Control.AddInteger('one', 1);
    Control.AddInteger('two', 2);
    Control.AddInteger('three', 3);
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var Value: Integer; ItemIndex: Integer;
begin
  ItemIndex := Math.Max(0, Control.ItemIndex);
  // avoid non selection

  Value := Control.Value[ItemIndex];
  ShowMessage(IntToStr(Value));
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
//var I, L: Integer; AData: PInteger;
begin
(*
  L := Pred(Control.Items.Count);
  for I := 0 to L do
  begin
    AData := Control.Data[ItemIndex];

    Control.Data[ItemIndex] := nil;
    System.Dispose(AData);
  end;
*)
end;

end.
