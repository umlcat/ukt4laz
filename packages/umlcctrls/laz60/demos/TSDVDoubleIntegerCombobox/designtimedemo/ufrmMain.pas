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
    Control: TSDVDoubleIntegerCombobox;
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
var Value: TSDVDoubleIntegerRecord;
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    Value.Lo := 1;
    Value.Hi := 2;
    Control.AddDoubleInteger('one', Value);

    Value.Lo := 3;
    Value.Hi := 4;
    Control.AddDoubleInteger('two', Value);

    Value.Lo := 5;
    Value.Hi := 6;
    Control.AddDoubleInteger('three', Value);
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var Value: TSDVDoubleIntegerRecord; ItemIndex: Integer; Msg: string;
begin
  ItemIndex := Math.Max(0, Control.ItemIndex);
  // avoid non selection

  Value := Control.Value[ItemIndex];

  Msg := SysUtils.Format('Lo:%d, Hi:%d', [Value.Lo, Value.Hi]);
  ShowMessage(Msg);
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
