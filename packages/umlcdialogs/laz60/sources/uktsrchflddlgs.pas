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

unit uktsrchflddlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Graphics, Controls,
{$ENDIF}
  Forms, Dialogs,
  uktsrchtypes,
  uktdlgctrls,
  uktsrchdlgs,
  dummy;

type

{ TCustomSDVSearchFieldDialog }

  TCustomSDVSearchFieldDialog = class(TCustomSDVSearchContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FFieldNames: TStrings;
    FFieldIndex: Integer;
  protected
    (* Protected declarations *)

    function getFieldNames: TStrings; virtual;
    function getFieldIndex: Integer; virtual;

    procedure setFieldNames(Value: TStrings); virtual;
    procedure setFieldIndex(Value: Integer); virtual;
  public
    (* Public declarations *)

    function CreateDialog(): TCustomForm; override;
    procedure DestroyDialog(); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    (* UnPublished declarations *)

    property FieldNames: TStrings
      read getFieldNames write setFieldNames;
    property FieldIndex: Integer
      read getFieldIndex write setFieldIndex;
  end;

{ TCustomSDVReplaceFieldDialog }

  TCustomSDVReplaceFieldDialog = class(TCustomSDVReplaceContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FFieldNames: TStrings;
    FFieldIndex: Integer;
  protected
    (* Protected declarations *)

    function getFieldNames: TStrings; virtual;
    function getFieldIndex: Integer; virtual;

    procedure setFieldNames(Value: TStrings); virtual;
    procedure setFieldIndex(Value: Integer); virtual;
  public
    (* Public declarations *)

    function CreateDialog(): TCustomForm; override;
    procedure DestroyDialog(); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* Never Published declarations *)

    (* UnPublished declarations *)

    property FieldNames: TStrings
      read getFieldNames write setFieldNames;
    property FieldIndex: Integer
      read getFieldIndex write setFieldIndex;
  end;

(* TSDVSearchFieldDialog *)

  TSDVSearchFieldDialog = class(TCustomSDVSearchFieldDialog)
  published
    (* Published declarations *)

    (* TDialogComponent: *)

    property HelpContext;
    property Title;

    property BeforeExecute;
    property AfterExecute;

    (* TCustomSDVSearchContainer: *)

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

    { TCustomSDVSearchFieldDialog: }

    property FieldNames;
    property FieldIndex;
  end;

(* TSDVReplaceFieldDialog *)

  TSDVReplaceFieldDialog = class(TCustomSDVReplaceFieldDialog)
  published
    (* Published declarations *)

    (* TDialogComponent: *)

    property HelpContext;
    property Title;

    property BeforeExecute;
    property AfterExecute;

    (* TCustomSDVSearchContainer: *)

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

    { TCustomSDVReplaceFieldDialog: }

    property FieldNames;
    property FieldIndex;
  end;

implementation
{$ifdef FPC}
uses
  lazuktfrmsearchfielddialog,
  lazuktfrmreplacefielddialog;
{$else}
uses
  vcluktfrmsearchfielddialog,
  vcluktfrmreplacefielddialog;
{$endif}

{ TCustomSDVSearchFieldDialog }

function TCustomSDVSearchFieldDialog.getFieldNames: TStrings;
begin
  Result := FFieldNames;
  { Goal: "FieldNames" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldNames" .}
end;

function TCustomSDVSearchFieldDialog.getFieldIndex: Integer;
begin
  Result := FFieldIndex;
  { Goal: "FieldIndex" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldIndex" .}
end;

procedure TCustomSDVSearchFieldDialog.setFieldNames(Value: TStrings);
begin
  FFieldNames.Assign(Value);
  { Goal: "FieldNames" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldNames" .}
end;

procedure TCustomSDVSearchFieldDialog.setFieldIndex(Value: Integer);
begin
  FFieldIndex := Value;
  { Goal: "FieldIndex" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldIndex" .}
end;

function TCustomSDVSearchFieldDialog.CreateDialog(): TCustomForm;
var F: TuktfrmSearchFieldDialog;
begin
  Application.CreateForm(TuktfrmSearchFieldDialog, FForm);
  Application.ProcessMessages;

  // --> assign some initial properties to the form,
  // --> before showing
  F := (FForm as TuktfrmSearchFieldDialog);
  F.Container := Self;

  if (FFieldNames.Count > 0) and (FFieldIndex = -1)
    then FieldIndex := 0;

  Application.ProcessMessages;
  F.FieldNames := FFieldNames;
  F.FieldIndex := FieldIndex;

  // --> ready
  Result := FForm;
  { Goal: To create the dialog box.}
  { Objetivo Construir la caja dialogo .}
end;

procedure TCustomSDVSearchFieldDialog.DestroyDialog();
begin
  // --> recover some properties from the form,
  // --> before releasing the form from memory
  FieldIndex := (FForm as TuktfrmSearchFieldDialog).FieldIndex;

  // --> done
  inherited DestroyDialog();
  { Goal: To destroy the dialog box.}
  { Objetivo Destruir la cajadialogo .}
end;

constructor TCustomSDVSearchFieldDialog.Create(AOwner: TComponent);
begin
  inherited;
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FFieldNames := TStringList.Create;
  FFieldIndex := -1;  
end;

destructor TCustomSDVSearchFieldDialog.Destroy;
begin
  FFieldNames.Free;
  // Clean the new properties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy;
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

{ TCustomSDVReplaceFieldDialog }

function TCustomSDVReplaceFieldDialog.getFieldNames: TStrings;
begin
  Result := FFieldNames;
  { Goal: "FieldNames" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldNames" .}
end;

function TCustomSDVReplaceFieldDialog.getFieldIndex: Integer;
begin
  Result := FFieldIndex;
  { Goal: "FieldIndex" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldIndex" .}
end;

procedure TCustomSDVReplaceFieldDialog.setFieldNames(Value: TStrings);
begin
  FFieldNames.Assign(Value);
  { Goal: "FieldNames" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldNames" .}
end;

procedure TCustomSDVReplaceFieldDialog.setFieldIndex(Value: Integer);
begin
  FFieldIndex := Value;
  { Goal: "FieldIndex" property get method .}
  { Objetivo: Metodo lectura propiedad "FieldIndex" .}
end;

function TCustomSDVReplaceFieldDialog.CreateDialog(): TCustomForm;
var F: TuktfrmReplaceFieldDialog;
begin
  Application.CreateForm(TuktfrmReplaceFieldDialog, FForm);
  Application.ProcessMessages;

  // --> assign some initial properties to the form,
  // --> before showing
  F := (FForm as TuktfrmReplaceFieldDialog);
  F.Container := Self;

  if (FFieldNames.Count > 0) and (FFieldIndex = -1)
    then FieldIndex := 0;

  Application.ProcessMessages;
  F.FieldNames := FFieldNames;
  F.FieldIndex := FieldIndex;

  // --> ready
  Result := FForm;
  { Goal: To create the dialog box.}
  { Objetivo Construir la caja dialogo .}
end;

procedure TCustomSDVReplaceFieldDialog.DestroyDialog();
begin
  // --> recover some properties from the form,
  // --> before releasing the form from memory
  FieldIndex := (FForm as TuktfrmReplaceFieldDialog).FieldIndex;

  // --> done
  inherited DestroyDialog();
  { Goal: To destroy the dialog box.}
  { Objetivo Destruir la cajadialogo .}
end;

constructor TCustomSDVReplaceFieldDialog.Create(AOwner: TComponent);
begin
  inherited;
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FFieldNames := TStringList.Create();
  FFieldIndex := -1;  
end;

destructor TCustomSDVReplaceFieldDialog.Destroy();
begin
  FFieldNames.Free();
  // Clean the new properties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy();
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

end.
