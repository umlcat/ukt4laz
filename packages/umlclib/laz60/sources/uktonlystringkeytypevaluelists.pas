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

unit uktonlystringkeytypevaluelists;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  uktnormobjects,
  uktlists,
  uktkeyvaluemodes,
  dummy;

type

(* TSDVOnlyStringKeyTypeValueItem *)

  TSDVOnlyStringKeyTypeValueItem = class(TSDVHalfNormalizedObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FKey:    string;
    FTypeID: string;
    FValue:  string;
  public
    (* Public declarations *)

    constructor CreateItem(const AKey, ATypeID, AValue: string); virtual;

    function MatchesKey(AKey: string): Boolean;

    function AsText(): string; override;
  public
    (* Public declarations *)

    (* accesors declarations *)

    function getKey: string; virtual;
    procedure setKey(const AKey: string); virtual;

    function getTypeID: string; virtual;
    procedure setTypeID(const ATypeID: string); virtual;

    function getValue: string; virtual;
    procedure setValue(const AValue: string); virtual;
  public
    (* Public declarations *)

    (* properties declarations *)

    property Key: string
      read getKey write setKey;

    property TypeID: string
      read getTypeID write setTypeID;

    property Value: string
      read getValue write setValue;
  end;

(* TSDVKeyValueTypeListInmediate *)

  TSDVOnlyStringKeyValueTypeListInmediate =
    procedure
      (const AItem: TSDVOnlyStringKeyTypeValueItem;
       const AParam: pointer) of object;

(* TSDVOnlyStringKeyTypeValueList *)

  TSDVOnlyStringKeyTypeValueList = class(TSDVObjectList)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    FAllowDuplicates: boolean;
    FIgnoreKeyCase: boolean;
    FIgnoreValueCase: boolean;
  public
    (* Public declarations *)

    (* Accessors declarations *)

    function getAllowDuplicates(): boolean;
    procedure setAllowDuplicates(const value: boolean);

    function getIgnoreKeyCase(): boolean;
    procedure setIgnoreKeyCase(const value: boolean);

    function getIgnoreValueCase(): boolean;
    procedure setIgnoreValueCase(const value: boolean);

    function getItems(AIndex: Integer): TSDVOnlyStringKeyTypeValueItem;
    procedure setItems(AIndex: Integer; Item: TSDVOnlyStringKeyTypeValueItem);
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

    function ChangeValue
      (const AKey, AValue: string): Boolean;

    function Insert
      (const AItem: TSDVOnlyStringKeyTypeValueItem): Integer;
    procedure InsertAt
      (const AIndex: Integer; const AItem: TSDVOnlyStringKeyTypeValueItem);

    function Extract
      (const AIndex: Integer): TSDVOnlyStringKeyTypeValueItem;
    procedure DeleteAt
      (const AIndex: Integer);

    function Remove
      (const AItem: TSDVOnlyStringKeyTypeValueItem): Integer;
    procedure Empty();

    function ExtractKeysCopy(): TStringList;
    function ExtractTypesCopy(): TStringList;
    function ExtractValuesCopy(): TStringList;
  public
    (* Public declarations *)

    procedure ForEachForward
      (const Proc: TSDVOnlyStringKeyValueTypeListInmediate;
       const AParam: pointer);
    procedure ForEachBackward
      (const Proc: TSDVOnlyStringKeyValueTypeListInmediate;
       const AParam: pointer);
  public
    (* Public declarations *)

    property AllowDuplicates: boolean
      read getAllowDuplicates write setAllowDuplicates;

    property IgnoreKeyCase: boolean
      read getIgnoreKeyCase write setIgnoreKeyCase;
    property IgnoreValueCase: boolean
      read getIgnoreValueCase write setIgnoreValueCase;
    property Items[AIndex: Integer]: TSDVOnlyStringKeyTypeValueItem
      read getItems write setItems; default;
  end;

implementation

(* TSDVOnlyStringKeyTypeValueItem *)

constructor TSDVOnlyStringKeyTypeValueItem.CreateItem
  (const AKey, ATypeID, AValue: string);
begin
  FKey := AKey;
  FValue := AValue;
  FTypeID := ATypeID;
end;

function TSDVOnlyStringKeyTypeValueItem.MatchesKey(AKey: string): Boolean;
begin
  // not case sensitive
  Result := Sametext(self.Key, AKey);
end;

function TSDVOnlyStringKeyTypeValueItem.AsText(): string;
begin
  Result :=
    '{' +
    'Key:"'   + FKey    + '",' +
    'Type:"'  + FTypeID + '",' +
    'Value:"' + FValue  + '",' +
    '}';
end;

function TSDVOnlyStringKeyTypeValueItem.getKey(): string;
begin
  Result := FKey;
end;

procedure TSDVOnlyStringKeyTypeValueItem.setKey(const AKey: string);
begin
  FKey := AKey;
end;

function TSDVOnlyStringKeyTypeValueItem.getValue(): string;
begin
  Result := FValue;
end;

procedure TSDVOnlyStringKeyTypeValueItem.setValue(const AValue: string);
begin
  FValue := AValue;
end;

function TSDVOnlyStringKeyTypeValueItem.getTypeID(): string;
begin
  Result := FTypeID;
end;

procedure TSDVOnlyStringKeyTypeValueItem.setTypeID(const ATypeID: string);
begin
  FTypeID := ATypeID;
end;

(* TSDVOnlyStringKeyTypeValueList *)

function TSDVOnlyStringKeyTypeValueList.getAllowDuplicates(): boolean;
begin
  Result := FAllowDuplicates;
end;

procedure TSDVOnlyStringKeyTypeValueList.setAllowDuplicates(const value: boolean);
begin
  FAllowDuplicates := value;
end;

function TSDVOnlyStringKeyTypeValueList.getIgnoreKeyCase(): boolean;
begin
  Result := FIgnoreKeyCase;
end;

procedure TSDVOnlyStringKeyTypeValueList.setIgnoreKeyCase(const value: boolean);
begin
  FIgnoreKeyCase := value;
end;

function TSDVOnlyStringKeyTypeValueList.getIgnoreValueCase(): boolean;
begin
  Result := FIgnoreValueCase;
end;

procedure TSDVOnlyStringKeyTypeValueList.setIgnoreValueCase(const value: boolean);
begin
  FIgnoreValueCase := value;
end;

function TSDVOnlyStringKeyTypeValueList.getItems(AIndex: Integer): TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TSDVOnlyStringKeyTypeValueItem(getInternalItems(AIndex));
end;

procedure TSDVOnlyStringKeyTypeValueList.setItems(AIndex: Integer; Item: TSDVOnlyStringKeyTypeValueItem);
begin
  setInternalItems(AIndex, Item);
end;

function TSDVOnlyStringKeyTypeValueList.MatchKeys(const A, B: string): Boolean;
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

function TSDVOnlyStringKeyTypeValueList.MatchValues(const A, B: string): Boolean;
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

constructor TSDVOnlyStringKeyTypeValueList.Create();
begin
  inherited Create();
  // properties not used in constructor,
  // initial values require to use properties* fields
  FAllowDuplicates := true;
  FIgnoreKeyCase := true;
  FIgnoreValueCase := true;
end;

destructor TSDVOnlyStringKeyTypeValueList.Destroy();
begin
  InternalEmpty();
  inherited Destroy();
end;

// busca la primera ocurrencia de la llave indicada
function TSDVOnlyStringKeyTypeValueList.IndexOfKey(const AKey: string): Integer;
var Item: TSDVOnlyStringKeyTypeValueItem; EachIndex, ALast: Integer; Found: boolean;
begin
  Result := -1;
  ALast  := (Self.Count - 1);

  EachIndex := 0; Found := false;
  while ((not Found) and (EachIndex <= ALast)) do
  begin
    Item := TSDVOnlyStringKeyTypeValueItem(getInternalItems(EachIndex));

    Found := Self.MatchKeys(Item.Key, AKey);
    Inc(EachIndex);
  end;

  if (Found) then
  begin
    Result := (EachIndex - 1);
  end;
end;

function TSDVOnlyStringKeyTypeValueList.IndexOfValue(const AValue: string): Integer;
var Item: TSDVOnlyStringKeyTypeValueItem; EachIndex, ALast: Integer; Found: boolean;
begin
  Result := -1;
  ALast  := (Self.Count - 1);

  EachIndex := 0; Found := false;
  while ((not Found) and (EachIndex <= ALast)) do
  begin
    Item := TSDVOnlyStringKeyTypeValueItem(getInternalItems(EachIndex));

    Found := Self.MatchValues(Item.Value, AValue);
    Inc(EachIndex);
  end;

  if (Found) then
  begin
    Result := (EachIndex - 1);
  end;
end;

function TSDVOnlyStringKeyTypeValueList.KeyFound(const AKey: string): Boolean;
begin
  Result := (Self.IndexOfKey(AKey) > -1);
end;

function TSDVOnlyStringKeyTypeValueList.ValueFound(const AValue: string): Boolean;
begin
  Result := (Self.IndexOfValue(AValue) > -1);
end;

function TSDVOnlyStringKeyTypeValueList.ChangeValue(const AKey, AValue: string): Boolean;
var Index: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  Result := false;
  Index := Self.IndexOfKey(AKey);

  Result := (Index > -1);
  if (Result) then
  begin
    Item := TSDVOnlyStringKeyTypeValueItem(getInternalItems(Index));

    Item.Value := AValue;

    setInternalItems(Index, Item);
  end;
end;

function TSDVOnlyStringKeyTypeValueList.Insert(const AItem: TSDVOnlyStringKeyTypeValueItem): Integer;
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

procedure TSDVOnlyStringKeyTypeValueList.InsertAt(const AIndex: Integer;
  const AItem: TSDVOnlyStringKeyTypeValueItem);
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

function TSDVOnlyStringKeyTypeValueList.Extract(const AIndex: Integer): TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TSDVOnlyStringKeyTypeValueItem(InternalExtract(AIndex));
end;

procedure TSDVOnlyStringKeyTypeValueList.DeleteAt(const AIndex: Integer);
begin
  InternalDeleteAt(AIndex);
end;

function TSDVOnlyStringKeyTypeValueList.Remove(const AItem: TSDVOnlyStringKeyTypeValueItem): Integer;
begin
  Result := InternalRemove(AItem);
end;

procedure TSDVOnlyStringKeyTypeValueList.Empty();
begin
  InternalEmpty();
end;

function TSDVOnlyStringKeyTypeValueList.ExtractKeysCopy(): TStringList;
var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TStringList.Create();

  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    Result.Add(AItem.Key);
  end;
  // Goal: Returns a "TStringList" filled with a copy,
  // of all keys.
  // The list must be deallocated by the programmer, later.
end;

function TSDVOnlyStringKeyTypeValueList.ExtractTypesCopy(): TStringList;
var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TStringList.Create();

  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    Result.Add(AItem.Key);
  end;
  // Goal: Returns a "TStringList" filled with a copy,
  // of all types.
  // The list must be deallocated by the programmer, later.
end;

function TSDVOnlyStringKeyTypeValueList.ExtractValuesCopy(): TStringList;
var EachIndex, LastIndex: Integer; AItem: TSDVOnlyStringKeyTypeValueItem;
begin
  Result := TStringList.Create();

  LastIndex := (Self.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AItem := Self.Items[EachIndex];
    Result.Add(AItem.Value);
  end;
  // Goal: Returns a "TStringList" filled with a copy,
  // of all values.
  // The list must be deallocated by the programmer, later.
end;

procedure TSDVOnlyStringKeyTypeValueList.ForEachForward(const Proc: TSDVOnlyStringKeyValueTypeListInmediate;
  const AParam: pointer);
var Index: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  for Index := 0 to Pred(Count) do
  begin
    Item := TSDVOnlyStringKeyTypeValueItem(getInternalItems(Index));
    Proc(Item, AParam);
  end;
end;

procedure TSDVOnlyStringKeyTypeValueList.ForEachBackward(const Proc: TSDVOnlyStringKeyValueTypeListInmediate;
  const AParam: pointer);
  var Index: Integer; Item: TSDVOnlyStringKeyTypeValueItem;
begin
  for Index := Pred(Count) downto 0 do
  begin
    Item := TSDVOnlyStringKeyTypeValueItem(getInternalItems(Index));
    Proc(Item, AParam);
  end;
end;

end.

