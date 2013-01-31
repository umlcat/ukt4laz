unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Math, vclsdvItemCombos;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    NeverActivated: Boolean;
  public
    { Public declarations }

    Control: TSDVDoubleIntegerCombobox;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  NeverActivated := true;
  Control := nil;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var Value: TSDVDoubleIntegerRecord;
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    Control := TSDVDoubleIntegerCombobox.Create(Self);
    Control.Top  := 50;
    Control.Left := 50;
    Self.InsertControl(Control);
    Control.Style := csDropDownList;

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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//
end;

end.
