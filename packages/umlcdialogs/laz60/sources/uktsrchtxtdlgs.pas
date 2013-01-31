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

unit uktsrchtxtdlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Graphics, Controls, Forms, Dialogs,
{$ENDIF}
  uktsrchtypes,
  uktdlgctrls,
  uktsrchdlgs,
  dummy;

(**
 ** Description:
 ** This unit declares collections of treenodes,
 ** that store Simple X.M.L. parsed tokens.
 **)

type

(* TCustomSDVSearchTextDialog *)

  TCustomSDVSearchTextDialog = class(TCustomSDVSearchContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    function CreateDialog(): TCustomForm; override;

    (* Never Published declarations *)

    (* UnPublished declarations *)
  end;

(* TCustomSDVReplaceTextDialog *)

  TCustomSDVReplaceTextDialog = class(TCustomSDVReplaceContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    function CreateDialog(): TCustomForm; override;

    (* Never Published declarations *)

    (* UnPublished declarations *)
  end;

{ TSDVSearchTextDialog }

  TSDVSearchTextDialog = class(TCustomSDVSearchTextDialog)
  published
    (* Published declarations *)

    (* TDialogComponent: *)

    property HelpContext;
    property Title;

    property BeforeExecute;
    property AfterExecute;

    { TCustomSDVSearchContainer: }

    property SearchText;

    property Direction;
    property Scope;
    property Origin;

    property ShowHelp;
    property ShowCaseSensitive;
    property ShowWholeWordsOnly;
    property ShowRegularExpressions;

    property WantCaseSensitive;
    property WantWholeWordsOnly;
    property WantRegularExpressions;

    property OnHelpClick;
  end;

{ TSDVReplaceTextDialog }

  TSDVReplaceTextDialog = class(TCustomSDVReplaceTextDialog)
  published
    (* Published declarations *)

    (* TDialogComponent: *)

    property HelpContext;
    property Title;

    property BeforeExecute;
    property AfterExecute;

    { TCustomSDVSearchContainer: }

    property SearchHistory;    
    property SearchText;

    property Direction;
    property Scope;
    property Origin;

    property ShowHelp;
    property ShowCaseSensitive;
    property ShowWholeWordsOnly;
    property ShowRegularExpressions;

    property WantCaseSensitive;
    property WantWholeWordsOnly;
    property WantRegularExpressions;

    { TCustomSDVReplaceContainer: }

    property ReplaceHistory;
    property ReplaceText;

    property ShowReplaceAll;
    property ShowPromptOnReplace;
    property ShowDeleteOnReplace;
    property ShowKeepCapitalCase;

    property WantPromptOnReplace;
    property WantDeleteOnReplace;
    property WantKeepCapitalCase;    

    property OnHelpClick;
  end;

implementation
{$ifdef FPC}
uses
  lazuktfrmsearchtextdialog,
  lazuktfrmreplacetextdialog;
{$else}
uses
  vcluktfrmsearchtextdialog,
  vcluktfrmreplacetextdialog;
{$endif}

(* TCustomSDVSearchTextDialog *)

function TCustomSDVSearchTextDialog.CreateDialog(): TCustomForm;
begin
  Application.CreateForm(TuktfrmSearchTextDialog, FForm);
  Application.ProcessMessages();

  // --> assign some initial properties to the form,
  // --> before showing
  (FForm as TuktfrmSearchTextDialog).Container := Self;

  // --> ready
  Result := FForm;
  { Goal: To create the dialog box.}
  { Objetivo Construir la caja dialogo .}
end;

(* TCustomSDVReplaceTextDialog *)

function TCustomSDVReplaceTextDialog.CreateDialog(): TCustomForm;
begin
  Application.CreateForm(TuktfrmReplaceTextDialog, FForm);
  Application.ProcessMessages();

  // --> assign some initial properties to the form,
  // --> before showing
  (FForm as TuktfrmReplaceTextDialog).Container := Self;

  // --> ready
  Result := FForm;
  { Goal: To create the dialog box.}
  { Objetivo Construir la caja dialogo .}
end;

end.
