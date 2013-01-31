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
  private
    { Private declarations }

    NeverActivated: Boolean;
  public
    { Public declarations }

    Control: TSDVObjectCombobox;
  end;

  TIntegerObject = class(TObject)
  public
    { Public declarations }

    Value: Integer;
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
var AObject: TIntegerObject;
begin
  if (NeverActivated) then
  begin
    NeverActivated := true;

    Control := TSDVObjectCombobox.Create(Self);
    Control.Top  := 50;
    Control.Left := 50;
    Self.InsertControl(Control);
    Control.Style := csDropDownList;

    AObject := TIntegerObject.Create;
    AObject.Value := 1;
    Control.AddObject('one', AObject);

    AObject := TIntegerObject.Create;
    AObject.Value := 2;
    Control.AddObject('two', AObject);

    AObject := TIntegerObject.Create;
    AObject.Value := 3;
    Control.AddObject('three', AObject);
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
var AObject: TIntegerObject; ItemIndex: Integer;
begin
  ItemIndex := Math.Max(0, Control.ItemIndex);
  // avoid non selection

  AObject := (Control.Objects[ItemIndex] as TIntegerObject);
  ShowMessage(IntToStr(AObject.Value));
end;

end.
