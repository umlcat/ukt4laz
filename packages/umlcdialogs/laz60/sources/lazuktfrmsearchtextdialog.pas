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

unit lazuktfrmsearchtextdialog;

interface

uses
  Windows, Messages,
  SysUtils, Types, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktressrchtxtdlgs,
  uktsrchtypes,
  uktSrchDlgs,
  dummy;

type

  { TuktfrmSearchTextDialog }

  TuktfrmSearchTextDialog = class(TForm)
    btnCancel: TButton;
    btnHelp: TButton;
    btnOK: TButton;
    chbSearchCaseSensitive: TCheckBox;
    chbSearchRegularExpressions: TCheckBox;
    chbSearchWholeWordsOnly: TCheckBox;
    edSearchTextToSearch: TComboBox;
    gbSearchOptions: TGroupBox;
    lbSearchTextToSearch: TLabel;
    MainPanel: TPanel;
    rgSearchDirection: TRadioGroup;
    rgSearchOrigin: TRadioGroup;
    rgSearchScope: TRadioGroup;
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

    FActivated: Boolean;
    FContainer: TCustomSDVSearchContainer;
  public
    { Public declarations }

    property Container: TCustomSDVSearchContainer
        read FContainer write FContainer default NIL;
  end;

  function Execute(const Container: TCustomSDVSearchContainer): Boolean;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
{$R *.lfm}
{$ENDIF}

function Execute(const Container: TCustomSDVSearchContainer): Boolean;
var Form: TuktfrmSearchTextDialog;
begin
  Application.CreateForm(TuktfrmSearchTextDialog, Form);
     Form.Container := Container;
     Result := (Form.ShowModal = mrOK);
  Form.Release; Form := nil;
end;

procedure TuktfrmSearchTextDialog.btnOKClick(Sender: TObject);
begin
  Container.SearchResult := srrSearch;
end;

procedure TuktfrmSearchTextDialog.btnCancelClick(Sender: TObject);
begin
  Container.SearchResult := srrCancel;
end;

procedure TuktfrmSearchTextDialog.btnHelpClick(Sender: TObject);
begin
  ShowMessage('Under Construction');
end;

procedure TuktfrmSearchTextDialog.FormCreate(Sender: TObject);
begin
  FActivated := FALSE;
end;

procedure TuktfrmSearchTextDialog.FormActivate(Sender: TObject);
begin
  if ((not FActivated) and Assigned(Container)) then
  with Container do
  begin
    rgSearchDirection.ItemIndex  := Integer(Direction);
    rgSearchScope.ItemIndex  := Integer(Scope);
    rgSearchOrigin.ItemIndex  := Integer(Origin);

    chbSearchCaseSensitive.Checked  := WantCaseSensitive;
    chbSearchWholeWordsOnly.Checked  := WantWholeWordsOnly;
    chbSearchRegularExpressions.Checked  := WantRegularExpressions;

    edSearchTextToSearch.Text  := SearchText;

    lbSearchTextToSearch.Caption := reslbSearchTextToSearch_Caption;

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
  end else Caption := resTuktfrmSearchDialog_Caption;

  edSearchTextToSearch.SetFocus;
  FActivated := TRUE;
  { Goal: Load parameters from manager .}
  { Objetivo: Cargar parametros del administrador .}
end;

procedure TuktfrmSearchTextDialog.FormDeactivate(Sender: TObject);
begin
  if (Container <> nil) then
  with Container do
  begin
    SearchText := edSearchTextToSearch.Text;
    Direction  := TSDVSearchDirection(rgSearchDirection.ItemIndex);
    Scope := TSDVSearchScope(rgSearchScope.ItemIndex);
    Origin := TSDVSearchOrigin(rgSearchOrigin.ItemIndex);

    WantCaseSensitive := chbSearchCaseSensitive.Checked;
    WantWholeWordsOnly := chbSearchWholeWordsOnly.Checked;
    WantRegularExpressions := chbSearchRegularExpressions.Checked;
  end;
  { Goal: Save parameters into manager .}
  { Objetivo: Guardar parametros al administrador .}
end;

end.
