unit ufrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, Menus, ActnList, ImgList,
  Mask, ToolEdit, RXCtrls,
  sdvAcColls, vclsdvCtrls,
  vclsdvComboCtrls,
  vclsdvDropBtns, vclsdvDropMenuBtns, vclsdvPanels;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    sbStatusBar: TStatusBar;
    btnExit: TBitBtn;
    btnTest: TButton;
    imlsActions: TImageList;
    aclsActions: TActionList;
    acEditCut: TAction;
    acEditCopy: TAction;
    acEditPaste: TAction;
    pmPopupMenu: TPopupMenu;
    procedure btnTestClick(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    Control: TCustomSDVDropDownMenuButton;
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
    doRight:  Result := doTop;
//    doCustom:
//    doTop:
    else      Result := doBottom
  end;
end;

procedure TfrmMain.btnTestClick(Sender: TObject);
//var Form: TCustomForm;
begin
  Control.Orientation := NextOr(Control.Orientation);
//  Form := FormByControl(Control);
//  ShowMessage(Form.Name);
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

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Control := TCustomSDVDropDownMenuButton.Create(Self);
  Control.Top  := 100;
  Control.Left := 100;
  Control.Images := {@}imlsActions;
  Control.Name := 'Control';
//  Control.ShowCaption := TRUE;
  Self.InsertControl(Control);

  with Control.Actions do
  begin
    (Add as TSDVActionItem).Action := acEditCut;
    (Add as TSDVActionItem).Action := acEditCopy;
    (Add as TSDVActionItem).Action := nil;    
    (Add as TSDVActionItem).Action := acEditPaste;
  end;
end;

end.
