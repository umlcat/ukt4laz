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
    Control: TSDVDataCombobox;
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
var I: PInteger;
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    I  := System.New(PInteger);
    I^ := 1;
    Control.AddData('one', I);

    I  := System.New(PInteger);
    I^ := 2;
    Control.AddData('two', I);

    I  := System.New(PInteger);
    I^ := 3;
    Control.AddData('three', I);
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var AData: PInteger; ItemIndex: Integer;
begin
  ItemIndex := Math.Max(0, Control.ItemIndex);
  // avoid non selection

  AData := Control.Data[ItemIndex];
  ShowMessage(IntToStr(AData^));
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
