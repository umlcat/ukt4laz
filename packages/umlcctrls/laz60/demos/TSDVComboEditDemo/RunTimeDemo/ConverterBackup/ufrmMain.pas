unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ImgList,
  sdvcomboedits;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    btnExit: TBitBtn;
    btnTest: TButton;
    imlsImages: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Control: TSDVComboEdit;

    procedure ControlOnClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var Bitmap: TBitmap;
begin
  Control := TSDVComboEdit.Create(Self);
  Control.Top  := 100;
  Control.Left := 100;
  Control.OnClick := {@}ControlOnClick;

  Bitmap := TBitmap.Create;
    imlsImages.GetBitmap(1, Bitmap);
    Control.Glyph := Bitmap;
  Bitmap.Free;

  Self.InsertControl(Control);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
//
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
begin
//
end;

procedure TfrmMain.ControlOnClick(Sender: TObject);
begin
  ShowMessage('Hello World !!!');
end;

end.
