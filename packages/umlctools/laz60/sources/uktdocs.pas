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

unit uktdocs;

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
  //Graphics,
  Controls, ComCtrls, Forms,
{$ENDIF}
  SysUtils, Classes, Types,
  uktstdevents,
  uktstrings,
  uktcomponents,
  uktossys,
  uktpaths,
  uktfilesbyname,
  uktsrchtypes,
  uktdocsres,
  dummy;

(**
 ** Description:
 ** This unit implements the "Document" Software Design Pattern,
 ** which allows to use a file in an application,
 ** with common features like: opening a file, saving a file, etc.
 **
 ** Examples: Bitmap editor, text or word processor editor,
 ** spreadsheet application.
 **)

type
  TOnSearchModeChangeEvent =
    (* ^ *)procedure (Sender: TObject; Value: TSDVSearchResult) of object;
type
  TOnConfirmUserEvent =
    (* ^ *)function (Sender: TObject; const Message: string): Boolean of object;

(* TCustomSDVDocument *)

  TCustomSDVDocument = class(TSDVHalfNormalizedComponent)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    (* Field declarations *)

    FAskOverwrite: Boolean;
    FHasName:      Boolean;
    FHasEncoding:  Boolean;
    FModified:     Boolean;
    FReadOnly:     Boolean;
    FIsClipboardEmpty: Boolean;

    FEncoding:  string;
    FExtNew:    string;
    FExtNewAs:  string;
    FExtOpen:   string;
    FExtSave:   string;
    FExtSaveAs: string;
    FFullPath:  string;

    FSearchMode:  TSDVSearchResult;
    FSearchIndex: Word;
  protected
    (* Protected declarations *)

    (* Functors declarations *)

    FOnModified:          TOnBooleanChangeEvent;
    FOnPathChanged:       TOnStringChangeEvent;
    FOnSearchModeChanged: TOnSearchModeChangeEvent;
    FOnConfirmUser:       TOnConfirmUserEvent;

    FBeforeNewFile:    TConfirmEvent;
    FAfterNewFile:     TNotifyEvent;

    FBeforeNewAsFile:  TConfirmEvent;
    FAfterNewAsFile:   TNotifyEvent;

    FBeforeOpenFile:   TConfirmEvent;
    FAfterOpenFile:    TNotifyEvent;

    FBeforeSaveFile:   TConfirmEvent;
    FAfterSaveFile:    TNotifyEvent;

    FBeforeSaveAsFile: TConfirmEvent;
    FAfterSaveAsFile:  TNotifyEvent;
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    function getAskOverwrite(): Boolean;
    function getEncoding(): string;
    function getExtNew(): string;
    function getExtNewAs(): string;
    function getExtOpen(): string;
    function getExtSave(): string;
    function getExtSaveAs(): string;
    function getHasName(): Boolean;
    function getHasEncoding(): Boolean;
    function getIsClipboardEmpty(): Boolean;
    function getModified(): Boolean;
    function getReadOnly(): Boolean;
    function getFullPath(): string;
    function getSearchMode(): TSDVSearchResult;

    procedure setAskOverwrite(const AValue: Boolean); virtual;
    procedure setEncoding(const AValue: string); virtual;
    procedure setExtNew(AValue: string); virtual;
    procedure setExtNewAs(AValue: string); virtual;
    procedure setExtOpen(const AValue: string); virtual;
    procedure setExtSave(const AValue: string); virtual;
    procedure setExtSaveAs(const AValue: string); virtual;
    procedure setHasName(const AValue: Boolean); virtual;
    procedure setHasEncoding(const AValue: Boolean); virtual;
    procedure setIsClipboardEmpty(const AValue: Boolean); virtual;
    procedure setModified(const AValue: Boolean); virtual;
    procedure setReadOnly(const AValue: Boolean); virtual;
    procedure setFullPath(const AValue: string); virtual;
    procedure setSearchMode(const AValue: TSDVSearchResult); virtual;
  protected
    (* Protected declarations *)

    (* Delegate declarations *)

    procedure DelegateOnModified(const AValue: Boolean);
    procedure DelegateOnPathChanged(const AValue: string);
    procedure DelegateOnSearchModeChanged(const AValue: TSDVSearchResult);

    function DelegateOnConfirmUser
      (const Message: string): Boolean;

    function DelegateBeforeNewFile(): Boolean;
    procedure DelegateAfterNewFile();

    function DelegateBeforeNewAsFile(): Boolean;
    procedure DelegateAfterNewAsFile();

    function DelegateBeforeOpenFile(): Boolean;
    procedure DelegateAfterOpenFile();

    function DelegateBeforeSaveFile(): Boolean;
    procedure DelegateAfterSaveFile();

    function DelegateBeforeSaveAsFile(): Boolean;
    procedure DelegateAfterSaveAsFile();
  protected
    (* Protected declarations *)

    function NewFileName(const Extension: string): string;

    procedure BackupPreviousFile();

    function InternalBeforeNewFile(): Boolean; virtual;
    procedure InternalNewFile();       virtual;
    procedure InternalAfterNewFile();  virtual;

    function InternalBeforeOpenFile(): Boolean; virtual;
    procedure InternalOpenFile();       virtual;
    procedure InternalAfterOpenFile();  virtual;

    function InternalBeforeNewAsFile(): Boolean; virtual;
    procedure InternalNewAsFile();       virtual;
    procedure InternalAfterNewAsFile();  virtual;

    function InternalBeforeSaveFile(): Boolean; virtual;
    procedure InternalSaveFile();       virtual;
    procedure InternalAfterSaveFile();  virtual;

    function InternalBeforeSaveAsFile(): Boolean; virtual;
    procedure InternalSaveAsFile();       virtual;
    procedure InternalAfterSaveAsFile();  virtual;
  public
    (* Public declarations *)

    function IgnoreFile(): Boolean;

    procedure NewFile();
    procedure NewAsFile();
    procedure OpenFile();
    procedure SaveFile();
    procedure SaveAsFile();
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* read-only properties *)

    function FileName(): string;
    function FullFileName(): string;
  public
    (* Public declarations *)

    (* Property declarations *)

    property AskOverwrite: Boolean
      read getAskOverwrite write setAskOverwrite;
    property Encoding: string
      read getEncoding write setEncoding;
    property ExtNew: string
      read getExtNew write setExtNew;
    property ExtNewAs: string
      read getExtNewAs write setExtNewAs;
    property ExtOpen: string
      read getExtOpen write setExtOpen;
    property ExtSave: string
      read getExtSave write setExtSave;
    property ExtSaveAs: string
      read getExtSaveAs write setExtSaveAs;
    property HasName: Boolean
      read getHasName write setHasName;
    property HasEncoding: Boolean
      read getHasEncoding write setHasEncoding;
    property IsClipboardEmpty: Boolean
      read getIsClipboardEmpty write setIsClipboardEmpty;
    property Modified: Boolean
      read getModified write setModified;
    property ReadOnly: Boolean
      read getReadOnly write setReadOnly;
    property FullPath: string
      read getFullPath write setFullPath;
    property SearchMode: TSDVSearchResult
      read getSearchMode write setSearchMode;
  public
    (* Public declarations *)

    (* Event declarations *)

    property OnModified: TOnBooleanChangeEvent
      read FOnModified write FOnModified;
    property OnPathChanged: TOnStringChangeEvent
      read FOnPathChanged write FOnPathChanged;
    property OnSearchModeChanged: TOnSearchModeChangeEvent
      read FOnSearchModeChanged write FOnSearchModeChanged;
    property OnConfirmUser: TOnConfirmUserEvent
      read FOnConfirmUser write FOnConfirmUser;

    property BeforeNewFile: TConfirmEvent
      read FBeforeNewFile write FBeforeNewFile;
    property AfterNewFile: TNotifyEvent
      read FAfterNewFile write FAfterNewFile;

    property BeforeNewAsFile: TConfirmEvent
      read FBeforeNewAsFile write FBeforeNewAsFile;
    property AfterNewAsFile: TNotifyEvent
      read FAfterNewAsFile write FAfterNewAsFile;

    property BeforeOpenFile: TConfirmEvent
      read FBeforeOpenFile write FBeforeOpenFile;
    property AfterOpenFile: TNotifyEvent
      read FAfterOpenFile write FAfterOpenFile;

    property BeforeSaveFile: TConfirmEvent
      read FBeforeSaveFile write FBeforeSaveFile;
    property AfterSaveFile: TNotifyEvent
      read FAfterSaveFile write FAfterSaveFile;

    property BeforeSaveAsFile: TConfirmEvent
      read FBeforeSaveAsFile write FBeforeSaveAsFile;
    property AfterSaveAsFile: TNotifyEvent
      read FAfterSaveAsFile write FAfterSaveAsFile;
  end;

(* TCustomSDVDelegateDocument *)

  TCustomSDVDelegateDocument = class(TCustomSDVDocument)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    (* Delegate declarations *)

    procedure DelegateOnNewFile();
    procedure DelegateOnNewAsFile();

    procedure DelegateOnOpenFile();

    procedure DelegateOnSaveFile();
    procedure DelegateOnSaveAsFile();
  protected
    (* Protected declarations *)

    procedure InternalNewFile(); override;
    procedure InternalNewAsFile(); override;
    procedure InternalOpenFile(); override;
    procedure InternalSaveFile(); override;
    procedure InternalSaveAsFile(); override;
  protected
    (* Protected declarations *)

    (* Functors declarations *)

    FOnNewFile:    TNotifyEvent;
    FOnNewAsFile:  TNotifyEvent;
    FOnOpenFile:   TNotifyEvent;
    FOnSaveFile:   TNotifyEvent;
    FOnSaveAsFile: TNotifyEvent;
  public
    (* Public declarations *)
  public
    (* Public declarations *)

    (* Event declarations *)

    property OnNewFile: TNotifyEvent
      read FOnNewFile write FOnNewFile;
    property OnNewAsFile: TNotifyEvent
      read FOnNewAsFile write FOnNewAsFile;
    property OnOpenFile: TNotifyEvent
      read FOnOpenFile write FOnOpenFile;
    property OnSaveFile: TNotifyEvent
      read FOnSaveFile write FOnSaveFile;
    property OnSaveAsFile: TNotifyEvent
      read FOnSaveAsFile write FOnSaveAsFile;
  end;

(* TCustomSDVTextDocument *)

  TCustomSDVTextDocument = class(TCustomSDVDocument)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    (* Field declarations *)

    FIdentation:   Integer;
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    function getIdentation(): Integer;

    procedure setIdentation(const AValue: Integer);
  protected
    (* Protected declarations *)

    procedure InternalNewFile(); override;
  public
    (* Public declarations *)

    (* Property declarations *)

    property Identation: Integer
      read getIdentation write setIdentation;
  end;

(* TSDVDocument *)

  TSDVDocument = class(TCustomSDVDocument)
  published
    (* Published declarations *)

    (* TCustomSDVDocument: *)

    property AskOverwrite;
    property Encoding;
    property ExtNewAs;
    property ExtOpen;
    property ExtSave;
    property ExtSaveAs;
    property HasName;
    property HasEncoding;
    property IsClipboardEmpty;
    property Modified;
    property ReadOnly;
    property FullPath;
    property SearchMode;

    property OnModified;
    property OnPathChanged;
    property OnSearchModeChanged;
    property OnConfirmUser;

    property BeforeNewFile;
    property AfterNewFile;

    property BeforeNewAsFile;
    property AfterNewAsFile;

    property BeforeOpenFile;
    property AfterOpenFile;

    property BeforeSaveFile;
    property AfterSaveFile;

    property BeforeSaveAsFile;
    property AfterSaveAsFile;
  end;

(* TSDVDelegateDocument *)

  TSDVDelegateDocument = class(TCustomSDVDelegateDocument)
  published
    (* Published declarations *)

    (* TCustomSDVDocument: *)

    property AskOverwrite;
    property Encoding;
    property ExtNewAs;
    property ExtOpen;
    property ExtSave;
    property ExtSaveAs;
    property HasName;
    property HasEncoding;
    property IsClipboardEmpty;
    property Modified;
    property ReadOnly;
    property FullPath;
    property SearchMode;

    property OnModified;
    property OnPathChanged;
    property OnSearchModeChanged;
    property OnConfirmUser;

    property BeforeOpenFile;
    property AfterOpenFile;

    (* TCustomSDVDelegateDocument: *)

    property OnNewFile;
    property OnNewAsFile;
    property OnOpenFile;
    property OnSaveFile;
    property OnSaveAsFile;
  end;

implementation

function TCustomSDVDocument.getAskOverwrite(): Boolean;
begin
  Result := FAskOverwrite;
end;

function TCustomSDVDocument.getEncoding(): string;
begin
  Result := FEncoding;
end;

function TCustomSDVDocument.getExtNew(): string;
begin
  Result := FExtNew;
end;

function TCustomSDVDocument.getExtNewAs(): string;
begin
  Result := FExtNewAs;
end;

function TCustomSDVDocument.getExtOpen(): string;
begin
  Result := FExtOpen;
end;

function TCustomSDVDocument.getExtSave(): string;
begin
  Result := FExtSave;
end;

function TCustomSDVDocument.getExtSaveAs(): string;
begin
  Result := FExtSaveAs;
end;

function TCustomSDVDocument.getHasName(): Boolean;
begin
  Result := FHasName;
end;

function TCustomSDVDocument.getHasEncoding(): Boolean;
begin
  Result := FHasEncoding;
end;

function TCustomSDVDocument.getIsClipboardEmpty(): Boolean;
begin
  Result := FIsClipboardEmpty;
end;

function TCustomSDVDocument.getModified(): Boolean;
begin
  Result := FModified;
end;

function TCustomSDVDocument.getReadOnly(): Boolean;
begin
  Result := FReadOnly;
end;

function TCustomSDVDocument.getFullPath(): string;
begin
  Result := FFullPath;
end;

function TCustomSDVDocument.getSearchMode(): TSDVSearchResult;
begin
  Result := FSearchMode;
end;

procedure TCustomSDVDocument.setFullPath(const AValue: string);
begin
  FFullPath := AValue;
  DelegateOnPathChanged(AValue);
end;

procedure TCustomSDVDocument.setSearchMode(const AValue: TSDVSearchResult);
begin
  if (FSearchMode <> AValue) then
  begin
    FSearchMode := AValue;
    DelegateOnSearchModeChanged(AValue);
  end;
end;

procedure TCustomSDVDocument.setAskOverwrite(const AValue: Boolean);
begin
  FAskOverwrite := AValue;
end;

procedure TCustomSDVDocument.setEncoding(const AValue: string);
begin
  FEncoding := AValue;
end;

procedure TCustomSDVDocument.setExtNew(AValue: string);
var ANewValue: string;
begin
  // remove optional prefixes,
  // example:
  // "*.txt" becomes "txt"
  // ".txt" becomes "txt"
  ANewValue := uktstrings.TrimPrefixCopy(AValue, '*.');
  ANewValue := uktstrings.TrimPrefixCopy(ANewValue, '.');

  FExtNew := ANewValue;
end;

procedure TCustomSDVDocument.setExtNewAs(AValue: string);
var ANewValue: string;
begin
  // remove optional prefixes,
  // example:
  // "*.txt" becomes "txt"
  // ".txt" becomes "txt"
  ANewValue := uktstrings.TrimPrefixCopy(AValue, '*.');
  ANewValue := uktstrings.TrimPrefixCopy(ANewValue, '.');

  FExtNewAs := ANewValue;
end;

procedure TCustomSDVDocument.setExtOpen(const AValue: string);
var ANewValue: string;
begin
  // remove optional prefixes,
  // example:
  // "*.txt" becomes "txt"
  // ".txt" becomes "txt"
  ANewValue := uktstrings.TrimPrefixCopy(AValue, '*.');
  ANewValue := uktstrings.TrimPrefixCopy(ANewValue, '.');

  FExtOpen := ANewValue;
end;

procedure TCustomSDVDocument.setExtSave(const AValue: string);
var ANewValue: string;
begin
  // remove optional prefixes,
  // example:
  // "*.txt" becomes "txt"
  // ".txt" becomes "txt"
  ANewValue := uktstrings.TrimPrefixCopy(AValue, '*.');
  ANewValue := uktstrings.TrimPrefixCopy(ANewValue, '.');

  FExtSave := AValue;
end;

procedure TCustomSDVDocument.setExtSaveAs(const AValue: string);
var ANewValue: string;
begin
  // remove optional prefixes,
  // example:
  // "*.txt" becomes "txt"
  // ".txt" becomes "txt"
  ANewValue := uktstrings.TrimPrefixCopy(AValue, '*.');
  ANewValue := uktstrings.TrimPrefixCopy(ANewValue, '.');

  FExtSaveAs := AValue;
end;

procedure TCustomSDVDocument.setHasName(const AValue: Boolean);
begin
  FHasName := AValue;
end;

procedure TCustomSDVDocument.setHasEncoding(const AValue: Boolean);
begin
  FHasEncoding := AValue;
end;

procedure TCustomSDVDocument.setIsClipboardEmpty(const AValue: Boolean);
begin
  FIsClipboardEmpty := AValue;

//  CloseMessagesEnabled:= Value;
//  SaveMessagesEnabled:= Value;
end;

procedure TCustomSDVDocument.setModified(const AValue: Boolean);
begin
  FModified := AValue;
  DelegateOnModified(AValue);
end;

procedure TCustomSDVDocument.setReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
end;

procedure TCustomSDVDocument.DelegateOnModified(const AValue: Boolean);
begin
  if (FOnModified <> nil) then
  begin
    FOnModified(Self, AValue);
  end;
end;

procedure TCustomSDVDocument.DelegateOnPathChanged(const AValue: string);
begin
  if (FOnPathChanged <> nil) then
  begin
    FOnPathChanged(Self, AValue);
  end;
end;

procedure TCustomSDVDocument.DelegateOnSearchModeChanged
  (const AValue: TSDVSearchResult);
begin
  if (FOnSearchModeChanged <> nil) then
  begin
    FOnSearchModeChanged(Self, AValue);
  end;
end;

function TCustomSDVDocument.DelegateOnConfirmUser
  (const Message: string): Boolean;
begin
  Result := FALSE;
  if (FOnConfirmUser <> nil) then
  begin
    Result := FOnConfirmUser(Self, Message);
  end;
end;

function TCustomSDVDocument.DelegateBeforeNewFile(): Boolean;
begin
  // by default, can continue
  Result := true;
  if (FBeforeNewFile <> nil) then
  begin
    Result := FBeforeNewFile(Self);
  end;
end;

procedure TCustomSDVDocument.DelegateAfterNewFile();
begin
  if (FAfterNewFile <> nil) then
  begin
    FAfterNewFile(Self);
  end;
end;

function TCustomSDVDocument.DelegateBeforeNewAsFile(): Boolean;
begin
  if (FBeforeNewAsFile <> nil) then
  begin
    Result := FBeforeNewAsFile(Self);
  end;
end;

procedure TCustomSDVDocument.DelegateAfterNewAsFile();
begin
  if (FAfterNewAsFile <> nil) then
  begin
    FAfterNewAsFile(Self);
  end;
end;

function TCustomSDVDocument.DelegateBeforeOpenFile(): Boolean;
begin
  if (FBeforeOpenFile <> nil) then
  begin
    Result := FBeforeOpenFile(Self);
  end;
end;

procedure TCustomSDVDocument.DelegateAfterOpenFile();
begin
  if (FAfterOpenFile <> nil) then
  begin
    FAfterOpenFile(Self);
  end;
end;

function TCustomSDVDocument.DelegateBeforeSaveFile(): Boolean;
begin
  if (FBeforeSaveFile <> nil) then
  begin
    Result := FBeforeSaveFile(Self);
  end;
end;

procedure TCustomSDVDocument.DelegateAfterSaveFile();
begin
  if (FAfterSaveFile <> nil) then
  begin
    FAfterSaveFile(Self);
  end;
end;

function TCustomSDVDocument.DelegateBeforeSaveAsFile(): Boolean;
begin
  if (FBeforeSaveAsFile <> nil) then
  begin
    Result := FBeforeSaveAsFile(Self);
  end;
end;

procedure TCustomSDVDocument.DelegateAfterSaveAsFile();
begin
  if (FAfterSaveAsFile <> nil) then
  begin
    FAfterSaveAsFile(Self);
  end;
end;

function TCustomSDVDocument.NewFileName(const Extension: string): string;
var APrevPath, APath, AFileName, ANewExt: string;
begin
  // remove potential optional prefix
  ANewExt := uktstrings.TrimPrefixCopy(Extension, '*.');
  ANewExt := uktstrings.TrimPrefixCopy(ANewExt, '.');

  APrevPath := uktossys.getSystemPath();
  AFileName := resNoName; // <--
  APath     := uktpaths.EncodePath(APrevPath, AFileName, Extension);

  Result := resNoName + '.' + ANewExt;
end;

procedure TCustomSDVDocument.BackupPreviousFile();
var CanSave, CanOverwrite: Boolean;
begin
  if (Self.Modified) then
  begin
    CanOverwrite := not FileExists(FFullPath);
    // revisar que archivo no exista
    // check file doesn*t exists

    if ((not CanOverwrite) and (AskOverwrite)) then
    begin
      CanSave := DelegateOnConfirmUser(Format(resFileOverwrite, [FullPath]));
      AskOverwrite := not CanSave;
    end else CanSave := not AskOverwrite;
    // detecta si el archivo deberia ser
    // detect if file should be overwritten

    if (CanSave) then
    begin
      Self.SaveFile();
    end;
  end;
end;

function TCustomSDVDocument.InternalBeforeNewFile(): Boolean;
begin
  Result := DelegateBeforeNewFile();
end;

procedure TCustomSDVDocument.InternalNewFile();
begin
  FAskOverwrite := FALSE;
  FHasName      := FALSE;

  FullPath := NewFileName(ExtNew);
  // limpiar propiedades de interfaz
  // clear interface properties

  FSearchIndex := 1;
  FSearchMode  := uktsrchtypes.srrSearch;
  SearchMode   := uktsrchtypes.srrNone;
  // sera actualizado al activar ventana
  // will be updated when window is activated

  FIsClipboardEmpty := TRUE;
  FModified     := FALSE;
  FHasName      := FALSE;
  FReadOnly     := FALSE;
  FHasEncoding  := FALSE;

  FAskOverwrite := FALSE;
end;

procedure TCustomSDVDocument.InternalAfterNewFile();
begin
  DelegateAfterNewFile();
end;

function TCustomSDVDocument.InternalBeforeOpenFile(): Boolean;
begin
  Result := DelegateBeforeOpenFile();
end;

procedure TCustomSDVDocument.InternalOpenFile();
begin
  HasName := TRUE;
  Modified := FALSE;
  AskOverwrite := FALSE;
  // update "modified" status
  // actualizar estado de "modificado"
end;

procedure TCustomSDVDocument.InternalAfterOpenFile();
begin
  DelegateAfterOpenFile();
end;

function TCustomSDVDocument.InternalBeforeNewAsFile(): Boolean;
begin
  Result := DelegateBeforeNewAsFile();
end;

procedure TCustomSDVDocument.InternalNewAsFile();
begin
  InternalNewFile();
end;

procedure TCustomSDVDocument.InternalAfterNewAsFile();
begin
  DelegateAfterNewAsFile();
end;

function TCustomSDVDocument.InternalBeforeSaveFile(): Boolean;
begin
  Result := DelegateBeforeSaveFile();
end;

procedure TCustomSDVDocument.InternalSaveFile();
begin
  HasName  := TRUE;
  Modified := FALSE;
  // update "modified" status
  // actualizar estado de "modificado"
end;

procedure TCustomSDVDocument.InternalAfterSaveFile();
begin
  DelegateAfterSaveFile();
end;

function TCustomSDVDocument.InternalBeforeSaveAsFile(): Boolean;
begin
  Result := DelegateBeforeSaveAsFile();
end;

procedure TCustomSDVDocument.InternalSaveAsFile();
begin
  HasName  := TRUE;
  Modified := FALSE;
  // update "modified" status
  // actualizar estado de "modificado"
end;

procedure TCustomSDVDocument.InternalAfterSaveAsFile();
begin
  DelegateAfterSaveAsFile();
end;

function TCustomSDVDocument.IgnoreFile(): Boolean;
begin
  Result := not Modified;
  if (Modified) then
  begin
    Result := not DelegateOnConfirmUser(Format(resFileSave, [FullPath]));
  end;
end;

procedure TCustomSDVDocument.NewFile();
begin
  BackupPreviousFile();

  if (InternalBeforeNewFile()) then
  begin
    InternalNewFile();
    InternalAfterNewFile();
  end;
end;

procedure TCustomSDVDocument.NewAsFile();
begin
  BackupPreviousFile();

  if (InternalBeforeNewAsFile()) then
  begin
    InternalNewAsFile();
    InternalAfterNewAsFile();
  end;
end;

procedure TCustomSDVDocument.OpenFile();
begin
  BackupPreviousFile();

  if (InternalBeforeOpenFile()) then
  begin
    InternalOpenFile();
    InternalAfterOpenFile();
  end;
end;

procedure TCustomSDVDocument.SaveFile();
var CanSave, CanOverwrite: Boolean;
begin
  CanOverwrite := not FileExists(FFullPath);
  // revisar que archivo no exista
  // check file doesn*t exists

  if ((not CanOverwrite) and (AskOverwrite)) then
  begin
    CanSave := DelegateOnConfirmUser(Format(resFileOverwrite, [FullPath]));
    AskOverwrite := not CanSave;
  end else CanSave := not AskOverwrite;
  // detecta si el archivo deberia ser
  // detect if file should be overwritten

  if (CanSave) then
  begin
    if (InternalBeforeSaveFile()) then
    begin
      InternalSaveFile();
      InternalAfterSaveFile();
    end;
  end;
end;

procedure TCustomSDVDocument.SaveAsFile();
begin
  if (InternalBeforeSaveAsFile()) then
  begin
    InternalSaveAsFile();
    InternalAfterSaveAsFile();
  end;
end;

constructor TCustomSDVDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // --> properties that are independent of
  // --> how many times a file is opened or closed
  FExtNewAs  := '';
  FExtOpen   := '';
  FExtSave   := '';
  FExtSaveAs := '';
  // default file extensions
  // extensiones de archivo por default
end;

destructor TCustomSDVDocument.Destroy();
begin
  inherited Destroy();
end;

function TCustomSDVDocument.FileName: string;
begin
  Result := uktpaths.ExtractFileName(Self.FullPath);
  // Goal: Returns filename, without extension, and without FullPath.
end;

function TCustomSDVDocument.FullFileName(): string;
begin
  Result := uktpaths.ExtractFileFullName(Self.FullPath);
  // Goal: Returns filename & fileext, without FullPath.
end;

(* TCustomSDVDelegateDocument *)

procedure TCustomSDVDelegateDocument.DelegateOnNewFile();
begin
  if (FOnNewFile <> nil) then
  begin
    FOnNewFile(Self);
  end;
end;

procedure TCustomSDVDelegateDocument.DelegateOnNewAsFile();
begin
  if (FOnNewAsFile <> nil) then
  begin
    FOnNewAsFile(Self);
  end;
end;

procedure TCustomSDVDelegateDocument.DelegateOnOpenFile();
begin
  if (FOnOpenFile <> nil) then
  begin
    FOnOpenFile(Self);
  end;
end;

procedure TCustomSDVDelegateDocument.DelegateOnSaveFile();
begin
  if (FOnSaveFile <> nil) then
  begin
    FOnSaveFile(Self);
  end;
end;

procedure TCustomSDVDelegateDocument.DelegateOnSaveAsFile();
begin
  if (FOnSaveAsFile <> nil) then
  begin
    FOnSaveAsFile(Self);
  end;
end;

procedure TCustomSDVDelegateDocument.InternalNewFile();
begin
  inherited InternalNewFile();
  DelegateOnNewFile();
end;

procedure TCustomSDVDelegateDocument.InternalNewAsFile();
begin
  inherited InternalNewAsFile();
  DelegateOnNewAsFile();
end;

procedure TCustomSDVDelegateDocument.InternalOpenFile();
begin
  inherited InternalOpenFile();
  DelegateOnOpenFile();
end;

procedure TCustomSDVDelegateDocument.InternalSaveFile();
begin
  inherited InternalSaveFile();
  DelegateOnSaveFile();
end;

procedure TCustomSDVDelegateDocument.InternalSaveAsFile();
begin
  inherited InternalSaveFile();
  DelegateOnSaveAsFile();
end;

(* TCustomSDVTextDocument *)

function TCustomSDVTextDocument.getIdentation(): Integer;
begin
  Result := FIdentation;
end;

procedure TCustomSDVTextDocument.setIdentation(const AValue: Integer);
begin
  FIdentation := AValue;
end;

procedure TCustomSDVTextDocument.InternalNewFile();
begin
  inherited InternalNewFile();
  FIdentation := 2;
end;

end.
