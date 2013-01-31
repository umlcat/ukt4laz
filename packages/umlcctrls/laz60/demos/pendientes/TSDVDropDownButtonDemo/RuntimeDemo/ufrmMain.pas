unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, ActnList, ImgList,
  vclsdvComboCtrls, vclsdvDropBtns;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Control: TSDVDropDownButton;

    procedure ControlOnDropDown(Sender: TObject);
    procedure ControlOnClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

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
  Control.Orientation := NextOrientation(Control.Orientation);
end;

procedure TfrmMain.ControlOnDropDown(Sender: TObject);
begin
  ShowMessage('Drop Down !!!');
end;

procedure TfrmMain.ControlOnClick(Sender: TObject);
begin
  ShowMessage('Click !!!');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Control := TSDVDropDownButton.Create(Self);
  Control.Top  := 100;
  Control.Left := 100;
  Control.Color := clBtnFace;  
  Control.OnDropDown := {@}ControlOnDropDown;
  Control.OnClick := {@}ControlOnClick;

  Self.InsertControl(Control);
end;

end.
