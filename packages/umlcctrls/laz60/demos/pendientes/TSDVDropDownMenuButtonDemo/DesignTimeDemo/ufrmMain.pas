unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, ActnList, ImgList,
  vclsdvPanels, vclsdvComboCtrls,
  vclsdvDropBtns, vclsdvDropMenuBtns;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    pmHello: TPopupMenu;
    imlsImages: TImageList;
    aclsActions: TActionList;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditPaste: TAction;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    Control: TSDVDropDownMenuButton;
    procedure btnTestClick(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

function NextOr(const Value: TSDVOrientation): TSDVOrientation;
begin
  case (Value) of
    doBottom: Result := doLeft;
    doLeft:   Result := doRight;
    doRight:  Result := doCustom;
//    doCustom:
//    doTop:
    else      Result := doBottom
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
begin
  Control.Orientation := NextOr(Control.Orientation);
end;

procedure TfrmMain.acEditCutExecute(Sender: TObject);
begin
  ShowMessage('Cut');
end;

procedure TfrmMain.acEditCopyExecute(Sender: TObject);
begin
  ShowMessage('Copy');
end;

procedure TfrmMain.acEditPasteExecute(Sender: TObject);
begin
  ShowMessage('Paste');
end;

end.
