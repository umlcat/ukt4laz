(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the UMLCat's Component Library.                  *
 *                                                                        *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution, *
 *  for details about the copyright.                                      *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 *                                                                        *
 **************************************************************************
**)

unit lazuktfrmreplacetextdialog;

interface
uses
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktSrchTypes,
  uktresSrchTxtDlgs,
  uktSrchDlgs,
  dummy;
  
type

  { TuktfrmReplaceTextDialog }

  TuktfrmReplaceTextDialog = class(TForm)
    btnALL: TButton;
    btnCancel: TButton;
    btnCopy: TSpeedButton;
    btnHelp: TButton;
    btnOK: TButton;
    chbReplaceCaseSensitive: TCheckBox;
    chbReplaceDeleteOnReplace: TCheckBox;
    chbReplaceKeepCapitalCase: TCheckBox;
    chbReplacePromptOnReplace: TCheckBox;
    chbReplaceRegularExpressions: TCheckBox;
    chbReplaceWholeWordsOnly: TCheckBox;
    edReplaceReplaceWith: TComboBox;
    edReplaceTextToSearch: TComboBox;
    gbReplaceOptions: TGroupBox;
    lbReplaceReplaceWith: TLabel;
    lbReplaceTextToSearch: TLabel;
    MainPanel: TPanel;
    rgReplaceDirection: TRadioGroup;
    rgReplaceOrigin: TRadioGroup;
    rgReplaceScope: TRadioGroup;
    procedure btnOKClick(Sender: TObject);
    procedure btnALLClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure chbReplaceDeleteOnReplaceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }

    FActivated: Boolean;
    FContainer: TCustomSDVReplaceContainer;
  public
    { Public declarations }

    property Container: TCustomSDVReplaceContainer
        read FContainer write FContainer default NIL;
  end;

  function Execute(const Container: TCustomSDVReplaceContainer): Boolean;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
{$R *.lfm}
{$ENDIF}

function Execute(const Container: TCustomSDVReplaceContainer): Boolean;
var Form: TuktfrmReplaceTextDialog;
begin
  Application.CreateForm(TuktfrmReplaceTextDialog, Form);
    Form.Container := Container;
    Result := (Form.ShowModal <> mrCancel);
  Form.Release; Form := nil;
end;

procedure TuktfrmReplaceTextDialog.btnOKClick(Sender: TObject);
begin
  Container.SearchResult := srrSearch;
  ModalResult := mrYes;
end;

procedure TuktfrmReplaceTextDialog.btnALLClick(Sender: TObject);
begin
  Container.SearchResult := srrReplaceALL;
  ModalResult := mrOK;
  // no se puede utilizar "mrYesToALL", porque no esta disponible en Delphi 4
  // cannot use "mrYesToALL", because isn*t available in Delphi 4
end;

procedure TuktfrmReplaceTextDialog.btnCancelClick(Sender: TObject);
begin
  Container.SearchResult := srrCancel;
  ModalResult := mrCancel;
end;

procedure TuktfrmReplaceTextDialog.btnHelpClick(Sender: TObject);
begin
  ShowMessage('Under Construction');
end;

procedure TuktfrmReplaceTextDialog.chbReplaceDeleteOnReplaceClick(
  Sender: TObject);
begin
  edReplaceReplaceWith.Enabled := TRUE;
  if (chbReplaceDeleteOnReplace.Checked) then
  begin
    edReplaceReplaceWith.Text := resDeleteText;
    edReplaceReplaceWith.Enabled := FALSE;
    edReplaceReplaceWith.Color := clBtnFace;
  end else
  begin
    edReplaceReplaceWith.Text := resReplaceText;
    edReplaceReplaceWith.Enabled := TRUE;
    edReplaceReplaceWith.Color := clWindow;
  end;
end;

procedure TuktfrmReplaceTextDialog.FormCreate(Sender: TObject);
begin
  FActivated := FALSE;
end;

procedure TuktfrmReplaceTextDialog.FormActivate(Sender: TObject);
begin
  if ((not FActivated) and Assigned(Container)) then
  with Container do
  begin
    rgReplaceDirection.ItemIndex := Integer(Direction);
    rgReplaceScope.ItemIndex     := Integer(Scope);
    rgReplaceOrigin.ItemIndex    := Integer(Origin);

    chbReplaceCaseSensitive.Checked   := WantCaseSensitive;
    chbReplaceWholeWordsOnly.Checked  := WantWholeWordsOnly;
    chbReplaceRegularExpressions.Checked := WantRegularExpressions;

    chbReplacePromptOnReplace.Checked  := WantPromptOnReplace;
    chbReplaceDeleteOnReplace.Checked  := WantDeleteOnReplace;
    chbReplaceKeepCapitalCase.Checked  := WantKeepCapitalCase;

    edReplaceTextToSearch.Text := SearchText;
    edReplaceReplaceWith.Text  := ReplaceText;

    lbReplaceTextToSearch.Caption := reslblReplaceTextToSearch_Caption;
    lbReplaceReplaceWith.Caption  := reslblReplaceReplaceWith_Caption;

    gbReplaceOptions.Caption :=   resgbSearchOptions_Caption;
    chbReplaceCaseSensitive.Caption := reschbSearchCaseSensitive_Caption;
    chbReplaceWholeWordsOnly.Caption := reschbSearchWholeWordsOnly_Caption;
    chbReplaceRegularExpressions.Caption := reschbSearchRegularExpressions_Caption;

    chbReplacePromptOnReplace.Caption := reschbReplacePromptOnReplace_Caption;
    chbReplaceDeleteOnReplace.Caption := reschbReplaceDeleteOnReplace_Caption;
    chbReplaceKeepCapitalCase.Caption := reschbReplaceKeepCapitalCase_Caption;
    
    rgReplaceDirection.Caption := resrgSearchDirection_Caption;
    rgReplaceDirection.Items[riSearchDirectionForward] := resriSearchDirectionForward_Caption;
    rgReplaceDirection.Items[riSearchDirectionBackward] := resriSearchDirectionBackward_Caption;

    rgReplaceScope.Caption := resrgSearchScope_Caption;
    rgReplaceScope.Items[riSearchScopeGlobal] := resriSearchScopeGlobal_Caption;
    rgReplaceScope.Items[riSearchScopeSelected] := resriSearchScopeSelected_Caption;

    rgReplaceOrigin.Caption := resrgSearchOrigin_Caption;
    rgReplaceOrigin.Items[riSearchOriginCursor] := resriSearchOriginCursor_Caption;
    rgReplaceOrigin.Items[riSearchOriginEntire] := resriSearchOriginEntire_Caption;

    edReplaceTextToSearch.Items := SearchHistory;
    edReplaceReplaceWith.Items  := ReplaceHistory;

    btnOK.Caption := resbtnOK_Caption;
    btnALL.Caption := resbtnALL_Caption;    
    btnCancel.Caption := resbtnCancel_Caption;
    btnHelp.Caption := resbtnHelp_Caption;

    chbReplaceCaseSensitive.Enabled  := ShowCaseSensitive;
    chbReplaceWholeWordsOnly.Enabled := ShowWholeWordsOnly;
    chbReplaceRegularExpressions.Enabled := ShowRegularExpressions;
    chbReplacePromptOnReplace.Enabled := ShowPromptOnReplace;
    chbReplaceDeleteOnReplace.Enabled := ShowDeleteOnReplace;

    btnHelp.Enabled := ShowHelp;
    btnALL.Enabled  := ShowReplaceALL;

    if (Title = '')
      then Caption := resTuktfrmReplaceDialog_Caption
      else Caption := Title;
  end else Caption := resTuktfrmReplaceDialog_Caption;

  edReplaceTextToSearch.SetFocus;
  FActivated := TRUE;
  { Goal: Load parameters from manager .}
  { Objetivo: Cargar parametros del administrador .}
end;

procedure TuktfrmReplaceTextDialog.FormDeactivate(Sender: TObject);
begin
  if Assigned(Container) then
  with Container do
  begin
    SearchText := edReplaceTextToSearch.Text;
    Direction  := TSDVSearchDirection(rgReplaceDirection.ItemIndex);
    Scope  := TSDVSearchScope(rgReplaceScope.ItemIndex);
    Origin := TSDVSearchOrigin(rgReplaceOrigin.ItemIndex);

    WantCaseSensitive := chbReplaceCaseSensitive.Checked;
    WantWholeWordsOnly := chbReplaceWholeWordsOnly.Checked;
    WantRegularExpressions := chbReplaceRegularExpressions.Checked;

    WantPromptOnReplace := chbReplacePromptOnReplace.Checked;
    WantDeleteOnReplace := chbReplaceDeleteOnReplace.Checked;
    WantKeepCapitalCase := chbReplaceKeepCapitalCase.Checked;    

    if (WantDeleteOnReplace)
      then ReplaceText := ''
      else ReplaceText := edReplaceReplaceWith.Text;
  end;
  { Goal: Save parameters into manager .}
  { Objetivo: Guardar parametros al administrador .}
end;

procedure TuktfrmReplaceTextDialog.btnCopyClick(Sender: TObject);
begin
  edReplaceReplaceWith.Text := edReplaceTextToSearch.Text;
  edReplaceReplaceWith.SetFocus;  
end;

end.
