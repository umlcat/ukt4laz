unit vclsdvcesrchmngr.pas;

interface
uses
  SysUtils, Classes,
  Dialogs, Forms,
  sdvresSrchTxtDlgs,
  vclsdvDlgCtrls,
  vclsdvSrchTxtDlgs,
  vclsdvSrchFldDlgs,
  vclsdvfrmSearchTextDialog,
  vclsdvfrmReplaceTextDialog,
  vclsdvfrmSearchFieldDialog,
  vclsdvfrmReplaceFieldDialog,
{$IFDEF VER140}
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

type

{ TSDVDialogComponentComponentEditor }

  TSDVDialogComponentComponentEditor = class(TComponentEditor)
    function  Container: TSDVDialogComponent; virtual; abstract;
  end;

{ TSDVSearchTextDialogComponentEditor }

  TSDVSearchTextDialogComponentEditor = class(TSDVDialogComponentComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;

    function Container: TSDVDialogComponent; override;

    procedure ExecuteVerb(Index: Integer); override;
  end;

{ TSDVSearchFieldDialogComponentEditor }

  TSDVSearchFieldDialogComponentEditor = class(TSDVDialogComponentComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;

    function Container: TSDVDialogComponent; override;

    procedure ExecuteVerb(Index: Integer); override;
  end;

{ TSDVReplaceTextDialogComponentEditor }

  TSDVReplaceTextDialogComponentEditor = class(TSDVDialogComponentComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;

    function Container: TSDVDialogComponent; override;

    procedure ExecuteVerb(Index: Integer); override;
  end;

{ TSDVReplaceFieldDialogComponentEditor }

  TSDVReplaceFieldDialogComponentEditor = class(TSDVDialogComponentComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;

    function Container: TSDVDialogComponent; override;

    procedure ExecuteVerb(Index: Integer); override;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor
    (TCustomSDVSearchTextDialog, TSDVSearchTextDialogComponentEditor);
  RegisterComponentEditor
    (TCustomSDVReplaceTextDialog, TSDVReplaceTextDialogComponentEditor);

  RegisterComponentEditor
    (TsdvfrmSearchFieldDialog, TSDVSearchFieldDialogComponentEditor);
  RegisterComponentEditor
    (TsdvfrmReplaceFieldDialog, TSDVReplaceFieldDialogComponentEditor);
end;

{ TSDVSearchTextDialogComponentEditor }

function TSDVSearchTextDialogComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
  { Goal: To get the number of menu options .}
  { Objetivo: Obtener el no. de opciones de menu .}
end;

function TSDVSearchTextDialogComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
     0: Result := resTestDialog;
  end;
  { Goal: To get the caption for a menu option .}
  { Objetivo: Obtener el titulo para una opcion de menu .}
end;

function TSDVSearchTextDialogComponentEditor.Container: TSDVDialogComponent;
begin
  Result := (Component as TCustomSDVSearchTextDialog);
end;

procedure TSDVSearchTextDialogComponentEditor.ExecuteVerb(Index: Integer);
begin
  vclsdvfrmSearchTextDialog.Execute(Container as TCustomSDVSearchTextDialog);
  { Goal: Execute the given option .}
  { Objetivo: Ejecutar la opcion indicada .}
end;

{ TSDVSearchFieldDialogComponentEditor }

function TSDVSearchFieldDialogComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
  { Goal: To get the number of menu options .}
  { Objetivo: Obtener el no. de opciones de menu .}
end;

function TSDVSearchFieldDialogComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
     0: Result := resTestDialog;
  end;
  { Goal: To get the caption for a menu option .}
  { Objetivo: Obtener el titulo para una opcion de menu .}
end;

function TSDVSearchFieldDialogComponentEditor.Container: TSDVDialogComponent;
begin
  Result := (Component as TSDVDialogComponent);
end;

procedure TSDVSearchFieldDialogComponentEditor.ExecuteVerb(Index: Integer);
var Form: TsdvfrmSearchFieldDialog; AContainer: TCustomSDVSearchFieldDialog;
begin
  AContainer := (Container as TCustomSDVSearchFieldDialog);
  Application.CreateForm(TCustomSDVSearchFieldDialog, Form);
     Form.Container  := AContainer;
     Form.FieldNames := AContainer.FieldNames;
     Form.FieldIndex := AContainer.FieldIndex;
     Form.ShowModal;
  Form.Release; Form := nil;

//  vclsdfrmSearchFileDialog.Execute(Container as TCustomSDSearchFileDialog);
  { Goal: Execute the given option .}
  { Objetivo: Ejecutar la opcion indicada .}
end;

{ TSDVReplaceTextDialogComponentEditor }

function TSDVReplaceTextDialogComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
  { Goal: To get the number of menu options .}
  { Objetivo: Obtener el no. de opciones de menu .}
end;

function TSDVReplaceTextDialogComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
     0: Result := resTestDialog;
  end;
  { Goal: To get the caption for a menu option .}
  { Objetivo: Obtener el titulo para una opcion de menu .}
end;

function TSDVReplaceTextDialogComponentEditor.Container: TSDVDialogComponent;
begin
  Result := (Component as TCustomSDVReplaceTextDialog);
end;

procedure TSDVReplaceTextDialogComponentEditor.ExecuteVerb(Index: Integer);
var Form: TsdvfrmReplaceTextDialog; AContainer: TCustomSDVReplaceTextDialog;
begin
  AContainer := (Container as TCustomSDVReplaceTextDialog);
  Application.CreateForm(TCustomSDVReplaceTextDialog, Form);
     Form.Container  := AContainer;
     Form.ShowModal;
  Form.Release; Form := nil;

//  vclsdfrmReplaceTextDialog.Execute(Container as TCustomSDVReplaceTextDialog);
  { Goal: Execute the given option .}
  { Objetivo: Ejecutar la opcion indicada .}
end;

{ TSDVReplaceFieldDialogComponentEditor }

function TSDVReplaceFieldDialogComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
  { Goal: To get the number of menu options .}
  { Objetivo: Obtener el no. de opciones de menu .}
end;

function TSDVReplaceFieldDialogComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
     0: Result := resTestDialog;
  end;
  { Goal: To get the caption for a menu option .}
  { Objetivo: Obtener el titulo para una opcion de menu .}
end;

function TSDVReplaceFieldDialogComponentEditor.Container: TSDVDialogComponent;
begin
  Result := (Component as TCustomSDVReplaceFieldDialog);
end;

procedure TSDVReplaceFieldDialogComponentEditor.ExecuteVerb(Index: Integer);
var Form: TsdvfrmReplaceFieldDialog; AContainer: TCustomSDVReplaceFieldDialog;
begin
  AContainer := (Container as TCustomSDVReplaceFieldDialog);
  Application.CreateForm(TCustomSDVReplaceFieldDialog, Form);
     Form.Container  := AContainer;
     Form.FieldNames := AContainer.FieldNames;
     Form.FieldIndex := AContainer.FieldIndex;
     Form.ShowModal;
  Form.Release; Form := nil;

//  vclsdfrmReplaceFieldDialog.Execute(Container as TCustomSDVReplaceFieldDialog);
  { Goal: Execute the given option .}
  { Objetivo: Ejecutar la opcion indicada .}
end;

end.
