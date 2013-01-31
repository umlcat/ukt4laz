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

unit lazuktfrmsearchfielddialog;

interface

uses
  Windows, Messages,
  SysUtils, Types, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktresSrchTxtDlgs,
  uktSrchTypes,
  uktSrchDlgs,
  dummy;

type
  TuktfrmSearchFieldDialog = class(TForm)
    lbSearchTextToSearch: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    gbSearchOptions: TGroupBox;
    chbSearchCaseSensitive: TCheckBox;
    chbSearchWholeWordsOnly: TCheckBox;
    chbSearchRegularExpressions: TCheckBox;
    rgSearchDirection: TRadioGroup;
    rgSearchScope: TRadioGroup;
    rgSearchOrigin: TRadioGroup;
    edSearchTextToSearch: TComboBox;
    lbFieldName: TLabel;
    cbFieldName: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }

    FActivated:  Boolean;
    FContainer:  TCustomSDVSearchContainer;
    FFieldNames: TStrings;
    FFieldIndex: Integer;
  public
    { Public declarations }

    property Container: TCustomSDVSearchContainer
        read FContainer write FContainer default NIL;
    property FieldNames: TStrings
        read FFieldNames write FFieldNames default NIL;
    property FieldIndex: Integer
        read FFieldIndex write FFieldIndex default -1;
  end;

  function Execute(const Container: TCustomSDVSearchContainer): Boolean;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

function Execute(const Container: TCustomSDVSearchContainer): Boolean;
var Form: TuktfrmSearchFieldDialog;
begin
  Application.CreateForm(TuktfrmSearchFieldDialog, Form);
     Form.Container := Container;
     Result := (Form.ShowModal = mrOK);
  Form.Release; Form := nil;
end;

procedure TuktfrmSearchFieldDialog.btnOKClick(Sender: TObject);
begin
  Container.SearchResult := srrSearch;
end;

procedure TuktfrmSearchFieldDialog.btnCancelClick(Sender: TObject);
begin
  Container.SearchResult := srrCancel;
end;

procedure TuktfrmSearchFieldDialog.btnHelpClick(Sender: TObject);
begin
  ShowMessage('Under Construction');
end;

procedure TuktfrmSearchFieldDialog.FormCreate(Sender: TObject);
begin
  FActivated  := FALSE;
  FContainer  := nil;
  FFieldNames := nil;
  FFieldIndex := -1;  
end;

procedure TuktfrmSearchFieldDialog.FormActivate(Sender: TObject);
begin
  if ((not FActivated) and Assigned(Container)) then
  with Container do
  begin
    rgSearchDirection.ItemIndex := Integer(Direction);
    rgSearchScope.ItemIndex     := Integer(Scope);
    rgSearchOrigin.ItemIndex    := Integer(Origin);

    chbSearchCaseSensitive.Checked  := WantCaseSensitive;
    chbSearchWholeWordsOnly.Checked  := WantWholeWordsOnly;
    chbSearchRegularExpressions.Checked  := WantRegularExpressions;

    edSearchTextToSearch.Text  := SearchText;

    lbSearchTextToSearch.Caption := reslbSearchTextToSearch_Caption;
    lbFieldName.Caption := reslbFieldName_Caption;

    gbSearchOptions.Caption :=   resgbSearchOptions_Caption;
    chbSearchCaseSensitive.Caption := reschbSearchCaseSensitive_Caption;
    chbSearchWholeWordsOnly.Caption := reschbSearchWholeWordsOnly_Caption;
    chbSearchRegularExpressions.Caption := reschbSearchRegularExpressions_Caption;

    rgSearchDirection.Caption := resrgSearchDirection_Caption;
    rgSearchDirection.Items[riSearchDirectionForward] := resriSearchDirectionForward_Caption;
    rgSearchDirection.Items[riSearchDirectionBackward] := resriSearchDirectionBackward_Caption;

    rgSearchScope.Caption := resrgSearchScope_Caption;
    rgSearchScope.Items[riSearchScopeGlobal] := resriSearchScopeGlobal_Caption;
    rgSearchScope.Items[riSearchScopeSelected] := resriSearchScopeSelected_Caption;

    rgSearchOrigin.Caption := resrgSearchOrigin_Caption;
    rgSearchOrigin.Items[riSearchOriginCursor] := resriSearchOriginCursor_Caption;
    rgSearchOrigin.Items[riSearchOriginEntire] := resriSearchOriginEntire_Caption;

    edSearchTextToSearch.Items := SearchHistory;
    cbFieldName.Items := FFieldNames;

    btnOK.Caption := resbtnOK_Caption;
    btnCancel.Caption := resbtnCancel_Caption;
    btnHelp.Caption := resbtnHelp_Caption;

    chbSearchCaseSensitive.Enabled  := ShowCaseSensitive;
    chbSearchWholeWordsOnly.Enabled := ShowWholeWordsOnly;
    chbSearchRegularExpressions.Enabled := ShowRegularExpressions;

    btnHelp.Enabled := ShowHelp;

    if (Title = '')
      then Caption := resTuktfrmSearchDialog_Caption
      else Caption := Title;

    cbFieldName.ItemIndex := FieldIndex;  
    // seleccionar primer elemento de cajacombo
    // select first item from combobox
  end else Caption := resTuktfrmSearchDialog_Caption;

  edSearchTextToSearch.SetFocus;
  FActivated := TRUE;
  { Goal: Load parameters from manager .}
  { Objetivo: Cargar parametros del administrador .}
end;

procedure TuktfrmSearchFieldDialog.FormDeactivate(Sender: TObject);
begin
  if Assigned(Container) then
  with Container do
  begin
    SearchText := edSearchTextToSearch.Text;
    Direction  := TSDVSearchDirection(rgSearchDirection.ItemIndex);
    Scope  := TSDVSearchScope(rgSearchScope.ItemIndex);
    Origin := TSDVSearchOrigin(rgSearchOrigin.ItemIndex);

    WantCaseSensitive := chbSearchCaseSensitive.Checked;
    WantWholeWordsOnly := chbSearchWholeWordsOnly.Checked;
    WantRegularExpressions := chbSearchRegularExpressions.Checked;

    FieldIndex := cbFieldName.ItemIndex;
  end;
  { Goal: Save parameters into manager .}
  { Objetivo: Guardar parametros al administrador .}
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrmsearchfielddialog.lrs'}
  {$ENDIF}

end.
