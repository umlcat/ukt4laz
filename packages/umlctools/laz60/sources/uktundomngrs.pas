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

unit uktundomngrs;

interface
uses
  SysUtils, Classes,
  dummy;

type

  TSDVTextOperationCode =
    (txopNone,
     txopBackupText, txopRestoreText,
     txopUnDoRemoveText, txopUnDoInsertText);

(* TSDVTextOperation *)

  TSDVTextOperation = class(TCollectionItem)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FOperation: TSDVTextOperationCode;
    FTextIndex: Integer;
    FCaretIndex: Integer;
    FText: string;
  public
    (* public declarations *)

    constructor Create(ACollection: TCollection); override;

    property Operation: TSDVTextOperationCode
      read FOperation write FOperation;
    property CaretIndex: Integer
      read FCaretIndex write FCaretIndex;
    property TextIndex: Integer
      read FTextIndex write FTextIndex;
    property Text: string
      read FText write FText;
  end;

(* TSDVTextOperations *)

  TSDVTextOperations = class(TCollection)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)
  end;

  TTextChangeEvent =
    procedure (Sender: TObject; const Item: TSDVTextOperation)
    of object;

(* TCustomSDVUnDoManager *)

  TCustomSDVUnDoManager = class(TComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FOnUnDo: TTextChangeEvent;
    FOnReDo: TTextChangeEvent;

    FBackupList:  TSDVTextOperations;
    FRestoreList: TSDVTextOperations;

    procedure DelegateOnUnDo(const Item: TSDVTextOperation);
    procedure DelegateOnReDo(const Item: TSDVTextOperation);
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BackupText(CaretIndex, TextIndex: Integer; Data: string);
    procedure RestoreText(CaretIndex, TextIndex: Integer; Data: string);

    procedure InsertText(CaretIndex, TextIndex: Integer; Data: string);
    procedure RemoveText(CaretIndex, TextIndex: Integer; Data: string);

    function UnDo: Boolean;
    function ReDo: Boolean;

    property BackupList: TSDVTextOperations
      read FBackupList write FBackupList;
    property RestoreList: TSDVTextOperations
      read FRestoreList write FRestoreList;
  published
    (* published declarations *)

    property OnUnDo: TTextChangeEvent
      read FOnUnDo write FOnUnDo;
    property OnReDo: TTextChangeEvent
      read FOnReDo write FOnReDo;
  end;

(* TSDVUnDoManager *)

  TSDVUnDoManager = class(TCustomSDVUnDoManager)

    (* TCustomSDVUnDoManager: *)

    property OnUnDo;
    property OnReDo;
  end;

implementation

(* TSDVTextOperation *)

constructor TSDVTextOperation.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FOperation := txopNone;

  FCaretIndex := -1;
  // posicion del caqret antes de la modificacion, comienza en 0
  // position of caret before modification, starts in 0

  FTextIndex := -1;
  // ubicacion de buffer en donde el texto fue cambiado, comienza en 0
  // location in buffer where text was changed, starts in 0

  FText := '';
  // text that was modified
  // texto que fue modificado
end;

(* TSDVTextOperations *)

(* TCustomSDVUnDoManager *)

procedure TCustomSDVUnDoManager.DelegateOnUnDo
  (const Item: TSDVTextOperation);
begin
  if (Assigned(FOnUnDo))
    then FOnUnDo(Self, Item);
end;

procedure TCustomSDVUnDoManager.DelegateOnReDo
  (const Item: TSDVTextOperation);
begin
  if (Assigned(FOnReDo))
    then FOnReDo(Self, Item);
end;

constructor TCustomSDVUnDoManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupList  := TSDVTextOperations.Create(TSDVTextOperation);
  FRestoreList := TSDVTextOperations.Create(TSDVTextOperation);

  FOnUnDo:= nil;
  FOnReDo:= nil;
end;

destructor TCustomSDVUnDoManager.Destroy;
begin
  FRestoreList.Free;
  FBackupList.Free;
  inherited Destroy;
end;

procedure TCustomSDVUnDoManager.BackupText
  (CaretIndex, TextIndex: Integer; Data: string);
var AItem: TSDVTextOperation;
begin
  AItem := (BackupList.Add as TSDVTextOperation);
  AItem.Operation := txopRestoreText;
  AItem.CaretIndex := CaretIndex;
  AItem.TextIndex := TextIndex;
  AItem.Text := Data;
  (* Objetivo: Respaldar una porcion de texto que no cambio su longuitud .*)
  (* Goal: Backup a portion of text that didn*t change its length .*)
end;

procedure TCustomSDVUnDoManager.RestoreText
  (CaretIndex, TextIndex: Integer; Data: string);
var AItem: TSDVTextOperation;
begin
  AItem := (BackupList.Add as TSDVTextOperation);
  AItem.Operation := txopBackupText;
  AItem.CaretIndex := CaretIndex;
  AItem.TextIndex := TextIndex;
  AItem.Text := Data;
  (* Objetivo: Restaurar una porcion de texto que no cambio su longuitud .*)
  (* Goal: Restore a portion of text that didn*t change its length .*)
end;

procedure TCustomSDVUnDoManager.InsertText
  (CaretIndex, TextIndex: Integer; Data: string);
var AItem: TSDVTextOperation;
begin
  AItem := (BackupList.Add as TSDVTextOperation);
  AItem.Operation := txopUnDoInsertText;
  AItem.CaretIndex := CaretIndex;
  AItem.TextIndex := TextIndex;
  AItem.Text := Data;
end;

procedure TCustomSDVUnDoManager.RemoveText
  (CaretIndex, TextIndex: Integer; Data: string);
var AItem: TSDVTextOperation;
begin
  AItem := (BackupList.Add as TSDVTextOperation);
  AItem.Operation := txopUnDoRemoveText;
  AItem.CaretIndex := CaretIndex;
  AItem.TextIndex := TextIndex;
  AItem.Text := Data;
end;

function TCustomSDVUnDoManager.UnDo: Boolean;
var ACount: Integer; Source, Dest: TSDVTextOperation;
begin
  ACount := BackupList.Count;
  Result := (BackupList.Count > 0);
  if (Result) then
  begin
    Source := (BackupList.Items[Pred(ACount)] as TSDVTextOperation);

    Dest := (RestoreList.Add as TSDVTextOperation);
    case (Source.Operation) of
      txopUnDoRemoveText: Dest.Operation := txopUnDoInsertText;
      txopUnDoInsertText: Dest.Operation := txopUnDoRemoveText;
      txopBackUpText:     Dest.Operation := txopRestoreText;
      txopRestoreText:    Dest.Operation := txopBackUpText;
      else Dest.Operation := txopNone;
    end;

    Dest.CaretIndex := Source.CaretIndex;
    Dest.TextIndex := Source.TextIndex;
    Dest.Text := Source.Text;
    // mover el elemento a la lista de restaurar
    // move the item to RestoreList*s list

    DelegateOnUnDo(Source);
  end;
end;

function TCustomSDVUnDoManager.ReDo: Boolean;
var ACount: Integer; AItem: TSDVTextOperation;
begin
  ACount := RestoreList.Count;
  Result := (RestoreList.Count > 0);
  AItem  := (RestoreList.Items[Pred(ACount)] as TSDVTextOperation);
  DelegateOnReDo(AItem);
end;

end.
