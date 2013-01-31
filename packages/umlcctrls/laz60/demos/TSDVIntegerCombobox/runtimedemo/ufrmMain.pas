unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Math, sdvItemCombos;

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

    Control: TSDVDataCombobox;
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
var I: PInteger;
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    Control := TSDVDataCombobox.Create(Self);
    Control.Top  := 50;
    Control.Left := 50;
    Self.InsertControl(Control);
    Control.Style := csDropDownList;

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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//
end;

end.
