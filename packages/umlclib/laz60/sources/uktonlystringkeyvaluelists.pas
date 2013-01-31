(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktonlystringkeyvaluelists;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  uktstrings,
  uktnormobjects,
  uktlists,
  uktstrstrlists,
  uktkeyvaluemodes,
  dummy;

type

(* TSDVOnlyStringKeyValueItem *)

  TSDVOnlyStringKeyValueItem = class(TSDVHalfNormalizedObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    _Key:   string;
    _Value: string;
  public
    (* Public declarations *)

    (* accesor declarations *)

    function getKey(): string; virtual;
    procedure setKey(const AValue: string); virtual;

    function getValue(): string; virtual;
    procedure setValue(const AValue: string); virtual;
  public
    (* Public declarations *)

    constructor CreateItem(const akey, avalue: string); virtual;
  public
    (* Public declarations *)

    function EqualKey(const AKey: string): Boolean;
    function EqualValue(const AValue: string): Boolean;

    function SameKey(const AKey: string): Boolean;
    function SameValue(const AValue: string): Boolean;
  public
    (* Public declarations *)

    property Key: string
      read getKey write setKey;
    property Value: string
      read getValue write setValue;
  end;

(* TSDVOnlyStringKeyValueListInmediate *)

  TSDVOnlyStringKeyValueListInmediate =
    procedure
      (const Item: TSDVOnlyStringKeyValueItem;
       const Param: pointer) of object;

(* TSDVOnlyStringKeyValueList *)

  TSDVOnlyStringKeyValueList = class(TSDVObjectList)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    _AllowDuplicates: boolean;
    _IgnoreKeyCase: boolean;
    _IgnoreValueCase: boolean;
  public
    (* Public declarations *)

    (* Accessors declarations *)

    function getAllowDuplicates(): boolean;
    procedure setAllowDuplicates(const AValue: boolean);

    function getIgnoreKeyCase(): boolean;
    procedure setIgnoreKeyCase(const AValue: boolean);

    function getIgnoreValueCase(): boolean;
    procedure setIgnoreValueCase(const AValue: boolean);

    function getItems
      (AIndex: Integer): TSDVOnlyStringKeyValueItem;
    procedure setItems
      (AIndex: Integer; Item: TSDVOnlyStringKeyValueItem);
  protected
    (* Protected declarations *)

    function MatchKeys(const A, B: string): Boolean;
    function MatchValues(const A, B: string): Boolean;
  public
    (* Public declarations *)

    constructor Create(); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    function IndexOfKey(const AKey: string): Integer;
    function IndexOfValue(const AValue: string): Integer;

    function KeyFound(const AKey: string): Boolean;
    function ValueFound(const AValue: string): Boolean;

    function ChangeKey
      (const APrevKey, ANewKey: string): Boolean;
    function ChangeValue
      (const AKey, AValue: string): Boolean;

    function ExtractKeysCopy(): TStrings;
    function ExtractValuesCopy(): TStrings;

    function Insert
      (const AItem: TSDVOnlyStringKeyValueItem): Integer;
    procedure InsertAt
      (const AIndex: Integer; const AItem: TSDVOnlyStringKeyValueItem);

    function Extract(const AIndex: Integer): TSDVOnlyStringKeyValueItem;
    procedure DeleteAt(const AIndex: Integer);

    function Remove(const AItem: TSDVOnlyStringKeyValueItem): Integer;
    procedure Empty();
  public
    (* Public declarations *)

    procedure ForEach
      (const AProc: TSDVOnlyStringKeyValueListInmediate; const AParam: pointer);
    procedure ForBack
      (const AProc: TSDVOnlyStringKeyValueListInmediate; const AParam: pointer);
  public
    (* Public declarations *)

    property AllowDuplicates: boolean
      read getAllowDuplicates write setAllowDuplicates;
    property IgnoreKeyCase: boolean
      read getIgnoreKeyCase write setIgnoreKeyCase;
    property IgnoreValueCase: boolean
      read getIgnoreValueCase write setIgnoreValueCase;

    property Items[AIndex: Integer]: TSDVOnlyStringKeyValueItem
      read getItems write setItems; default;
  end;

implementation

(* TSDVOnlyStringKeyValueItem *)

constructor TSDVOnlyStringKeyValueItem.CreateItem(const akey, avalue: string);
begin
  Self._Key := akey;
  Self._Value := avalue;
end;

function TSDVOnlyStringKeyValueItem.EqualKey(const AKey: string): Boolean;
begin
  Result := (Self._Key = AKey);
end;

function TSDVOnlyStringKeyValueItem.EqualValue(const AValue: string): Boolean;
begin
  Result := (Self._Value = AValue);
end;

function TSDVOnlyStringKeyValueItem.SameKey(const AKey: string): Boolean;
begin
  Result := uktstrings.SameText(Self._Key, AKey);
end;

function TSDVOnlyStringKeyValueItem.SameValue(const AValue: string): Boolean;
begin
  Result := uktstrings.SameText(Self._Value, AValue);
end;

function TSDVOnlyStringKeyValueItem.getKey(): string;
begin
  Result := _Key;
end;

procedure TSDVOnlyStringKeyValueItem.setKey(const AValue: string);
begin
  _Key := AValue;
end;

function TSDVOnlyStringKeyValueItem.getValue(): string;
begin
  Result := _Value;
end;

procedure TSDVOnlyStringKeyValueItem.setValue(const AValue: string);
begin
  _Value := AValue;
end;

(* TSDVOnlyStringKeyValueList *)

function TSDVOnlyStringKeyValueList.getAllowDuplicates(): boolean;
begin
  Result := Self._AllowDuplicates;
end;

procedure TSDVOnlyStringKeyValueList.setAllowDuplicates(const AValue: boolean);
begin
  Self._AllowDuplicates := AValue;
end;

function TSDVOnlyStringKeyValueList.getIgnoreKeyCase(): boolean;
begin
  Result := Self._IgnoreKeyCase;
end;

procedure TSDVOnlyStringKeyValueList.setIgnoreKeyCase(const AValue: boolean);
begin
  Self._IgnoreKeyCase := AValue;
end;

function TSDVOnlyStringKeyValueList.getIgnoreValueCase(): boolean;
begin
  Result := Self._IgnoreValueCase;
end;

procedure TSDVOnlyStringKeyValueList.setIgnoreValueCase(const AValue: boolean);
begin
  Self._IgnoreValueCase := AValue;
end;

function TSDVOnlyStringKeyValueList.getItems(AIndex: Integer): TSDVOnlyStringKeyValueItem;
begin
  Result := TSDVOnlyStringKeyValueItem(getInternalItems(AIndex));
end;

procedure TSDVOnlyStringKeyValueList.setItems
  (AIndex: Integer; Item: TSDVOnlyStringKeyValueItem);
begin
  setInternalItems(AIndex, Item);
end;

function TSDVOnlyStringKeyValueList.MatchKeys(const A, B: string): Boolean;
begin
  Result := false;

  if (IgnoreKeyCase) then
  begin
    Result := SameText(A, B);
  end else
  begin
    Result := (A = B)
  end;
end;

function TSDVOnlyStringKeyValueList.MatchValues(const A, B: string): Boolean;
begin
  Result := false;

  if (IgnoreValueCase) then
  begin
    Result := SameText(A, B);
  end else
  begin
    Result := (A = B)
  end;
end;

constructor TSDVOnlyStringKeyValueList.Create();
begin
  inherited Create();
  // properties not used in constructor,
  // initial values require to use properties* fields
  _AllowDuplicates := true;
  _IgnoreKeyCase := true;
  _IgnoreValueCase := true;
end;

destructor TSDVOnlyStringKeyValueList.Destroy();
begin
  InternalEmpty();
  inherited Destroy();
end;

// busca la primera ocurrencia de la llave indicada
function TSDVOnlyStringKeyValueList.IndexOfKey(const AKey: string): Integer;
var Item: TSDVOnlyStringKeyValueItem; EachIndex, ALast: Integer; Found: boolean;
begin
  Result := -1;
  ALast  := (Self.Count - 1);

  EachIndex := 0; Found := false;
  while ((not Found) and (EachIndex <= ALast)) do
  begin
    Item := TSDVOnlyStringKeyValueItem(getInternalItems(EachIndex));

    Found := Self.MatchKeys(Item.Key, AKey);
    Inc(EachIndex);
  end;

  if (Found) then
  begin
    Result := (EachIndex - 1);
  end;
end;

function TSDVOnlyStringKeyValueList.IndexOfValue(const AValue: string): Integer;
var Item: TSDVOnlyStringKeyValueItem; EachIndex, ALast: Integer; Found: boolean;
begin
  Result := -1;
  ALast  := (Self.Count - 1);

  EachIndex := 0; Found := false;
  while ((not Found) and (EachIndex <= ALast)) do
  begin
    Item := TSDVOnlyStringKeyValueItem(getInternalItems(EachIndex));

    Found := Self.MatchValues(Item.Value, AValue);
    Inc(EachIndex);
  end;

  if (Found) then
  begin
    Result := (EachIndex - 1);
  end;
end;

function TSDVOnlyStringKeyValueList.KeyFound(const AKey: string): Boolean;
begin
  Result := (Self.IndexOfKey(AKey) > -1);
end;

function TSDVOnlyStringKeyValueList.ValueFound(const AValue: string): Boolean;
begin
  Result := (Self.IndexOfValue(AValue) > -1);
end;

function TSDVOnlyStringKeyValueList.ChangeKey(const APrevKey, ANewKey: string): Boolean;
var ThisIndex, DupIndex: Integer; ThisItem: TSDVOnlyStringKeyValueItem;
begin
  Result := false;
  ThisIndex := Self.IndexOfKey(APrevKey);
  DupIndex  := Self.IndexOfKey(ANewKey);

  Result := ((ThisIndex > -1) and (DupIndex = -1));
  if (Result) then
  begin
    ThisItem := TSDVOnlyStringKeyValueItem(getInternalItems(ThisIndex));

    ThisItem.Key := ANewKey;

    setInternalItems(ThisIndex, ThisItem);
  end;
end;

function TSDVOnlyStringKeyValueList.ChangeValue(const AKey, AValue: string): Boolean;
var ThisIndex: Integer; ThisItem: TSDVOnlyStringKeyValueItem;
begin
  Result := false;
  ThisIndex := Self.IndexOfKey(AKey);

  Result := (ThisIndex > -1);
  if (Result) then
  begin
    ThisItem := TSDVOnlyStringKeyValueItem(getInternalItems(ThisIndex));

    ThisItem.Value := AValue;

    setInternalItems(ThisIndex, ThisItem);
  end;
end;

function TSDVOnlyStringKeyValueList.ExtractKeysCopy(): TStrings;
var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyValueItem;
begin
  Result := TStringList.Create();

  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    Result.Add(AItem.Key);
  end;
  // Goal: Returns a "TStringList" filled with a copy,
  // of all keys. The list must be deallocated by the programmer, later.
end;

function TSDVOnlyStringKeyValueList.ExtractValuesCopy(): TStrings;
var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyValueItem;
//var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyValueItem;
begin
  Result := TStringList.Create();

  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    Result.Add(AItem.Value);
  end;


  (*
  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    TuktstringstringList(Result).AddWithData(AItem.Key, AItem.Value);
  end;
  *)
  // Goal: Returns a "TStringList" filled with a copy,
  // of all values. The list must be deallocated by the programmer, later.
end;

function TSDVOnlyStringKeyValueList.Insert(const AItem: TSDVOnlyStringKeyValueItem): Integer;
begin
  Result := -1;
  if (AllowDuplicates) then
  begin
    Result := InternalInsert(AItem);
  end else
  begin
    // buscar no este duplicado
    if (not Self.KeyFound(AItem.Key)) then
    begin
       Result := InternalInsert(AItem);
    end;
  end;
end;

procedure TSDVOnlyStringKeyValueList.InsertAt(const AIndex: Integer;
  const AItem: TSDVOnlyStringKeyValueItem);
begin
  if (AllowDuplicates) then
  begin
    InternalInsertAt(AIndex, AItem);
  end else
  begin
    // buscar no este duplicado
    if (not Self.KeyFound(AItem.Key)) then
    begin
       InternalInsertAt(AIndex, AItem);
    end;
  end;
end;

function TSDVOnlyStringKeyValueList.Extract(const AIndex: Integer): TSDVOnlyStringKeyValueItem;
begin
  Result := TSDVOnlyStringKeyValueItem(InternalExtract(AIndex));
end;

procedure TSDVOnlyStringKeyValueList.DeleteAt(const AIndex: Integer);
begin
  InternalDeleteAt(AIndex);
end;

function TSDVOnlyStringKeyValueList.Remove(const AItem: TSDVOnlyStringKeyValueItem): Integer;
begin
  Result := InternalRemove(AItem);
end;

procedure TSDVOnlyStringKeyValueList.Empty();
begin
  InternalEmpty();
end;

procedure TSDVOnlyStringKeyValueList.ForEach
  (const AProc: TSDVOnlyStringKeyValueListInmediate;
   const AParam: pointer);
var Index: Integer; Item: TSDVOnlyStringKeyValueItem;
begin
  for Index := 0 to Pred(Count) do
  begin
    Item := TSDVOnlyStringKeyValueItem(getInternalItems(Index));
    AProc(Item, AParam);
  end;
end;

procedure TSDVOnlyStringKeyValueList.ForBack
  (const AProc: TSDVOnlyStringKeyValueListInmediate;
   const AParam: pointer);
  var Index: Integer; Item: TSDVOnlyStringKeyValueItem;
begin
  for Index := Pred(Count) downto 0 do
  begin
    Item := TSDVOnlyStringKeyValueItem(getInternalItems(Index));
    AProc(Item, AParam);
  end;
end;

end.

