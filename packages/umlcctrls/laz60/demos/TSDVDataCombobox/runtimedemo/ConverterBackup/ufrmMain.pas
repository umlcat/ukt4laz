unit ufrmMain;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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

    Control: TSDVIntegerCombobox;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  NeverActivated := true;
  Control := nil;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    Control := TSDVIntegerCombobox.Create(Self);
    Control.Top  := 50;
    Control.Left := 50;
    Self.InsertControl(Control);
    Control.Style := csDropDownList;

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

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//
end;

end.
