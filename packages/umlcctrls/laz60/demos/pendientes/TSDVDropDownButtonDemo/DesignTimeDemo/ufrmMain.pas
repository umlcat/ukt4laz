unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, ActnList, ImgList,
  vclsdvPanels, vclsdvComboCtrls, vclsdvDropBtns;

const
  riCut   = 0;
  riCopy  = 1;
  riPaste = 2;

  ControlHeight = 52;
  ControlWidth  = 100;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    DropDownButton1: TSDVDropDownButton;
    DropDownButton2: TSDVDropDownButton;
    procedure btnTestClick(Sender: TObject);
    procedure DropDownButton1DropDown(Sender: TObject);
    procedure DropDownButton1Click(Sender: TObject);
    procedure DropDownButton2Click(Sender: TObject);
    procedure DropDownButton2DropDown(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function DropDownButton2PopUpControlHeight(Sender: TObject): Integer;
    function DropDownButton2PopUpControlWidth(Sender: TObject): Integer;
  private
    { Private declarations }
  protected
    { Protected declarations }

    IsActivated: Boolean;
  public
    { Public declarations }

    AOption: Integer;

    procedure RefreshMainButtonGlyph;
  end;

var
  frmMain: TfrmMain;

implementation

uses ufrmPopUpDialog, udmDataModule;

{$R *.dfm}

function NextOrientation(const Value: TSDVOrientation): TSDVOrientation;
begin
  case (Value) of
    doBottom: Result := doLeft;
    doLeft:   Result := doRight;
    doRight:  Result := doTop;
//    doCustom:
//    doTop:
    else      Result := doBottom
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
begin
  DropDownButton1.Orientation :=
     NextOrientation(DropDownButton1.Orientation);

  DropDownButton2.Orientation :=
     NextOrientation(DropDownButton2.Orientation);
end;

procedure TfrmMain.DropDownButton1DropDown(Sender: TObject);
begin
  ShowMessage('Drop Down !!!');
end;

procedure TfrmMain.DropDownButton1Click(Sender: TObject);
begin
  ShowMessage('Click !!!');
end;

procedure TfrmMain.DropDownButton2Click(Sender: TObject);
begin
  case AOption of
    riCut:   ShowMessage('Cut !!!');
    riCopy:  ShowMessage('Copy !!!');
    riPaste: ShowMessage('Paste !!!');
  end;
end;

procedure TfrmMain.DropDownButton2DropDown(Sender: TObject);
var P: TPoint;
begin
  P := DropDownButton2.DropDownLocation;
  ufrmPopUpDialog.Execute(P.X, P.Y, ControlWidth, ControlHeight, AOption);
  RefreshMainButtonGlyph;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if (not IsActivated) then
  begin
    IsActivated := true;
    RefreshMainButtonGlyph;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IsActivated := false;
  AOption := riCopy;
end;

procedure TfrmMain.RefreshMainButtonGlyph;
var Glyph: TBitmap;
begin
  Glyph := TBitmap.Create;

  dmDataModule.imlsItems.GetBitmap(AOption + 2, Glyph);
  DropDownButton2.Glyph := Glyph;
  // first 2 images are reserved
  // las primeras 2 imagenes estan reservadas

  Glyph.Free;
end;

function TfrmMain.DropDownButton2PopUpControlHeight(
  Sender: TObject): Integer;
begin
  Result := ControlHeight;
end;

function TfrmMain.DropDownButton2PopUpControlWidth(
  Sender: TObject): Integer;
begin
  Result := ControlWidth;
end;

end.
