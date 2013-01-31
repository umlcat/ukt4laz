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

unit uktlists;

interface
uses SysUtils, 
  uktreslists,
  dummy;

const

(* Maximum TList size *)

  MaxListSize = Maxint div 16;

(* List error *)

  IndexNotFound = -1;

  IgnoreIndex   = -1;

type

(* TSDVListError *)

  TSDVListError = class(Exception);

(* TSDVPointerArray *)

  PSDVPointerArray = ^TSDVPointerArray;
  TSDVPointerArray = array[0..MaxListSize - 1] of pointer;

  TListSortCompare = function (Item1, Item2: pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

(* TCustomSDVList *)

  TCustomSDVList = class(TObject)
  private
    { Private declarations }

    FCapacity: Integer;
  protected
    { Protected declarations }

    FCount:  Integer;
    FList:   PSDVPointerArray;

    function getInternalItems(Index: Integer): pointer;
    function getCapacity(): Integer;
    function getCount(): Integer;
    function getList(): PSDVPointerArray;

    procedure setInternalItems(Index: Integer; Item: pointer);
    procedure setCapacity(const NewCapacity: Integer);
    procedure setCount(const Value: Integer);
    procedure setList(const Value: PSDVPointerArray);

    procedure Grow; virtual;
//    procedure Notify(Ptr: pointer; Action: TListNotification); virtual;

    function InternalIndexOf(Item: pointer): Integer;
    function InternalFirst(): pointer;
    function InternalLast(): pointer;

    function InternalInsert(const AItem: pointer): Integer;
    procedure InternalInsertAt(Index: Integer; Item: pointer);

    procedure InternalEmpty();
    procedure InternalDeleteAt(const AIndex: Integer);

    function InternalRemove(const AItem: pointer): Integer;
    function InternalExtract(const AIndex: Integer): pointer;
  public
    { Public declarations }

    constructor Create(); virtual;
    destructor Destroy(); override;

    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;

    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);

    function IsEmpty(): Boolean;
    function Expand(): TCustomSDVList;

    procedure Pack();
    procedure Sort(Compare: TListSortCompare);

    procedure Copy(const Source: TCustomSDVList);
    procedure Intersect(const Source: TCustomSDVList);
    procedure Union(const Source: TCustomSDVList);

    { Unpublished declarations }

    property Capacity: Integer
      read getCapacity write setCapacity;
    property Count: Integer
      read getCount write setCount;
    property InternalItems[Index: Integer]: pointer
      read getInternalItems write setInternalItems; default;
    property List: PSDVPointerArray
      read getList write setList;
  end;

{ TSDVPointerListFirstThatFunc }

  TSDVPointerListFirstThatFunc =
   function (const AItem: pointer; const AParam: pointer): Boolean of object;

{ TSDVPointerListForEachProc }

  TSDVPointerListForEachProc =
   procedure (const AItem: pointer; const AParam: pointer) of object;

(* TSDVPointerList *)

  TSDVPointerList = class(TCustomSDVList)
  private
    { Private declarations }
  protected
    { Protected declarations }

    function getItems(AIndex: Integer): pointer;

    procedure setItems(AIndex: Integer; AItem: pointer);
  public
    { Public declarations }

    function IndexOf(const AItem: pointer): Integer;
    function First(): pointer;
    function Last(): pointer;

    function Insert(const AItem: pointer): Integer;
    procedure InsertAt(const AIndex: Integer; const AItem: pointer);

    function Extract(const AIndex: Integer): pointer;
    procedure DeleteAt(const AIndex: Integer);
    function Remove(const AItem: pointer): Integer;
    procedure Empty();

    function FirstThat
      (const Func: TSDVPointerListFirstThatFunc; const AParam: pointer): pointer;

    procedure ForEach
      (const Proc: TSDVPointerListForEachProc; const AParam: pointer);
    procedure ForBack
      (const Proc: TSDVPointerListForEachProc; const AParam: pointer);

    property Items[Index: Integer]: pointer
      read getItems write setItems; default;
  end;

{ TSDVOwnerList }

  TSDVOwnerList = class(TCustomSDVList)
  private
    { Private declarations }

    FOwnsObjects: Boolean;
  protected
    { Protected declarations }

//    function InternalCreateItem: pointer; virtual; abstract;
//    procedure InternalDestroyItem(const AItem: pointer); virtual; abstract;
  public
    { Public declarations }

    procedure DeleteAt(const AIndex: Integer);
    procedure Empty();

    property OwnsObjects: Boolean
      read FOwnsObjects write FOwnsObjects;
  end;

{ TSDVObjectListInmediate }

  TSDVObjectListInmediate =
    procedure (const AItem: TObject;
     const AParam: pointer) of object;

(* TSDVObjectList *)

  TSDVObjectList = class(TSDVOwnerList)
  private
    { Private declarations }
  protected
    { Protected declarations }

    function getItems(AIndex: Integer): TObject;

    procedure setItems(AIndex: Integer; AItem: TObject);
  public
    { Public declarations }

    function IndexOf(const AItem: TObject): Integer;
    function First(): TObject;
    function Last(): TObject;

    function Insert(const AItem: TObject): Integer;
    procedure InsertAt(const AIndex: Integer; const AItem: TObject);

    function Extract(const AIndex: Integer): TObject;
    procedure DeleteAt(const AIndex: Integer);
    function Remove(const AItem: TObject): Integer;
    procedure Empty();

    procedure ForEach
      (const Proc: TSDVObjectListInmediate; const AParam: pointer);
    procedure ForBack
      (const Proc: TSDVObjectListInmediate; const AParam: pointer);

    property Items[AIndex: Integer]: TObject
      read getItems write setItems; default;
  end;

implementation

(* TCustomSDVList *)

function TCustomSDVList.getInternalItems(Index: Integer): pointer;
begin
  if ((Index < 0) or (Index >= FCount)) then
  begin
    Error(@err_ListIndexError, Index);
  end;

  Result := FList^[Index];
end;

function TCustomSDVList.getCapacity(): Integer;
begin
  Result := FCapacity;
end;

function TCustomSDVList.getCount(): Integer;
begin
  Result := FCount;
end;

function TCustomSDVList.getList(): PSDVPointerArray;
begin
  Result := FList;
end;

procedure TCustomSDVList.setInternalItems(Index: Integer; Item: pointer);
//var Temp: pointer;
begin
  if ((Index < 0) or (Index >= FCount)) then
  begin
    Error(@err_ListIndexError, Index);
  end;

  if (Item <> FList^[Index]) then
  begin
//    Temp := FList^[Index];
    FList^[Index] := Item;
//    if (Temp <> nil)
//        then Notify(Temp, lnDeleted);
//    if (Item <> nil)
//        then Notify(Item, lnAdded);
  end;
end;

procedure TCustomSDVList.setCapacity(const NewCapacity: Integer);
begin
  if ((NewCapacity < FCount) or (NewCapacity > MaxListSize)) then
  begin
    Error(@err_ListIndexError, NewCapacity);
  end;

  if (NewCapacity <> FCapacity) then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TCustomSDVList.setCount(const Value: Integer);
var NewCount, I: Integer;
begin
  NewCount := Value;
  if ((NewCount < 0) or (NewCount > MaxListSize)) then
  begin
    Error(@err_ListIndexError, NewCount);
  end;

  if (NewCount > FCapacity) then
  begin
    SetCapacity(NewCount);
  end;

  if (NewCount > FCount) then
  begin
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(pointer), 0)
  end else
    // to while
    for I := FCount - 1 downto NewCount do
    begin
      InternalDeleteAt(I);
    end;

  FCount := NewCount;
end;

procedure TCustomSDVList.setList(const Value: PSDVPointerArray);
begin
  FList := Value;
end;

constructor TCustomSDVList.Create();
begin
  inherited Create();
end;

destructor TCustomSDVList.Destroy();
begin
  InternalEmpty();
  inherited Destroy();
end;

class procedure TCustomSDVList.Error(const Msg: string; Data: Integer);

  {$ifdef Delphi}
  function ReturnAddr(): pointer;
  asm
          MOV     EAX,[EBP+4]
  end;
  {$endif}

begin
  {$ifdef Delphi}
  raise TSDVListError.CreateFmt(Msg, [Data]) at ReturnAddr();
  {$else}
  raise TSDVListError.CreateFmt(Msg, [Data]);
  {$endif}
end;

class procedure TCustomSDVList.Error(Msg: PResStringRec; Data: Integer);
begin
  TCustomSDVList.Error(LoadResString(Msg), Data);
end;

procedure TCustomSDVList.Exchange(Index1, Index2: Integer);
var Item: pointer;
begin
  if ((Index1 < 0) or (Index1 >= FCount)) then
  begin
    Error(@err_ListIndexError, Index1);
  end;

  if ((Index2 < 0) or (Index2 >= FCount)) then
  begin
    Error(@err_ListIndexError, Index2);
  end;

  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

procedure TCustomSDVList.Move(CurIndex, NewIndex: Integer);
var Item: pointer;
begin
  if (CurIndex <> NewIndex) then
  begin
    if ((NewIndex < 0) or (NewIndex >= FCount)) then
    begin
      Error(@err_ListIndexError, NewIndex);
    end;

    Item := getInternalItems(CurIndex);
    FList^[CurIndex] := nil;
    InternalDeleteAt(CurIndex);
    InternalInsertAt(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

function TCustomSDVList.IsEmpty(): Boolean;
begin
  Result := (Count < 1);
end;

function TCustomSDVList.Expand(): TCustomSDVList;
begin
  if (FCount = FCapacity) then
  begin
    Grow();
  end;

  Result := Self;
end;

procedure TCustomSDVList.Grow;
var Delta: Integer;
begin
  if (FCapacity > 64) then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if (FCapacity > 8)
      then Delta := 16
      else Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

procedure TCustomSDVList.Pack();
var I: Integer;
begin
  // while
  for I := FCount - 1 downto 0 do
  begin
    if (InternalItems[I] = nil) then
    begin
      InternalDeleteAt(I);
    end;
  end;
end;

procedure QuickSort
 (SortList: PSDVPointerArray; L, R: Integer; Compare: TListSortCompare);
var I, J: Integer; P, T: pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while Compare(SortList^[I], P) < 0 do
      begin
        Inc(I);
      end;

      while Compare(SortList^[J], P) > 0 do
      begin
        Dec(J);
      end;

      if (I <= J) then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until (I > J);

    if (L < J) then
    begin
      QuickSort(SortList, L, J, Compare);
    end;

    L := I;
  until (I >= R);
end;

procedure TCustomSDVList.Sort(Compare: TListSortCompare);
begin
  if ((FList <> nil) and (Count > 0)) then
  begin
    QuickSort(FList, 0, Count - 1, Compare);
  end;
end;

(*
procedure TCustomSDVList.Notify(Ptr: pointer; Action: TListNotification);
begin
end;
*)

function TCustomSDVList.InternalIndexOf(Item: pointer): Integer;
begin
  Result := 0;
  while( (Result < FCount) and (FList^[Result] <> Item)) do
  begin
    Inc(Result);
  end;

  if (Result = FCount) then
  begin
    Result := IndexNotFound;
  end;
end;

function TCustomSDVList.InternalFirst(): pointer;
begin
  Result := getInternalItems(0);
end;

function TCustomSDVList.InternalLast(): pointer;
begin
  Result := getInternalItems(FCount - 1);
end;

function TCustomSDVList.InternalInsert(const AItem: pointer): Integer;
begin
  Result := FCount;
  if (Result = FCapacity) then
  begin
    Grow();
  end;

  FList^[Result] := AItem;
  Inc(FCount);
//if (Item <> nil)
//  then Notify(Item, lnAdded);
end;

procedure TCustomSDVList.InternalInsertAt(Index: Integer; Item: pointer);
begin
  if ((Index < 0) or (Index > FCount)) then
  begin
    Error(@err_ListIndexError, Index);
  end;

  if (FCount = FCapacity) then
  begin
    Grow();
  end;

  if (Index < FCount) then
  begin
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(pointer));
  end;

  FList^[Index] := Item;
  Inc(FCount);
//if (Item <> nil) then
//  Notify(Item, lnAdded);
end;

procedure TCustomSDVList.InternalEmpty();
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCustomSDVList.InternalDeleteAt(const AIndex: Integer);
//var Temp: pointer;
begin
  if ((AIndex < 0) or (AIndex >= FCount)) then
  begin
    Error(@err_ListIndexError, AIndex);
  end;

//  Temp := InternalItems[AIndex];
  Dec(FCount);

  if (AIndex < FCount) then
  begin
    System.Move(FList^[AIndex + 1], FList^[AIndex],
      (FCount - AIndex) * SizeOf(pointer));
  end;
//if (Temp <> nil)
//  then Notify(Temp, lnDeleted);
end;

function TCustomSDVList.InternalRemove(const AItem: pointer): Integer;
begin
  Result := InternalIndexOf(AItem);
  if (Result >= 0) then
  begin
    InternalDeleteAt(Result);
  end;
end;

function TCustomSDVList.InternalExtract(const AIndex: Integer): pointer;
begin
  Result := nil;
  if ((AIndex >= 0) and (AIndex <= Count)) then
  begin
    Result := getInternalItems(AIndex);
    FList^[AIndex] := nil;
    InternalDeleteAt(AIndex);
//  Notify(Result, lnExtracted);
  end;
end;

procedure TCustomSDVList.Copy(const Source: TCustomSDVList);
var I: Integer;
begin
  InternalEmpty();
  Capacity := Source.Capacity;
  for I := 0 to Pred(Source.Count) do
  begin
    InternalInsert(Source[I]);
  end;
end;

procedure TCustomSDVList.Intersect(const Source: TCustomSDVList);
var I, EachIndex: Integer;
begin
  // while
  for I := Count - 1 downto 0 do
  begin
    EachIndex := Source.InternalIndexOf(InternalItems[I]);
    if (EachIndex = IndexNotFound) then
    begin
      InternalDeleteAt(I);
    end;
  end;
end;

procedure TCustomSDVList.Union(const Source: TCustomSDVList);
var I, EachIndex: Integer;
begin
  for I := 0 to Source.Count - 1 do
  begin
    EachIndex := Source.InternalIndexOf(InternalItems[I]);
    if (EachIndex = IndexNotFound) then
    begin
      InternalInsert(Source[I]);
    end;
  end;
end;

(* TSDVPointerList *)

function TSDVPointerList.getItems(AIndex: Integer): pointer;
begin
  Result := getInternalItems(AIndex);
end;

procedure TSDVPointerList.setItems(AIndex: Integer; AItem: pointer);
begin
  setInternalItems(AIndex, AItem);
end;

function TSDVPointerList.IndexOf(const AItem: pointer): Integer;
begin
  Result := InternalIndexOf(AItem);
end;

function TSDVPointerList.First(): pointer;
begin
  Result := InternalFirst();
end;

function TSDVPointerList.Last(): pointer;
begin
  Result := InternalLast();
end;

function TSDVPointerList.Insert(const AItem: pointer): Integer;
begin
  Result := InternalInsert(AItem);
end;

procedure TSDVPointerList.InsertAt
  (const AIndex: Integer; const AItem: pointer);
begin
  InternalInsertAt(AIndex, AItem);
end;

function TSDVPointerList.Extract(const AIndex: Integer): pointer;
begin
  Result := InternalExtract(AIndex);
end;

procedure TSDVPointerList.DeleteAt(const AIndex: Integer);
begin
  InternalDeleteAt(AIndex);
end;

function TSDVPointerList.Remove(const AItem: pointer): Integer;
begin
  Result := InternalRemove(AItem);
end;

procedure TSDVPointerList.Empty();
begin
  InternalEmpty();
end;

function TSDVPointerList.FirstThat
  (const Func: TSDVPointerListFirstThatFunc;
   const AParam: pointer): pointer;
var Found: Boolean; Index, LastIndex: Integer; Item: pointer;
begin
  Result := nil;

  Found := false;
  Index := 0;
  LastIndex  := Count;
  while ((not Found) and (Index < LastIndex)) do
  begin
    Item := getInternalItems(Index);
    Found := Func(Item, AParam);
    Inc(Index);
  end;

  if (Found) then
  begin
    Result := Item;
  end;
end;

procedure TSDVPointerList.ForEach
  (const Proc: TSDVPointerListForEachProc; const AParam: pointer);
var Index: Integer; Item: pointer;
begin
  for Index := 0 to Pred(Count) do
  begin
    Item := getInternalItems(Index);
    Proc(Item, AParam);
  end;
end;

procedure TSDVPointerList.ForBack
  (const Proc: TSDVPointerListForEachProc; const AParam: pointer);
var Index: Integer; Item: pointer;
begin
  for Index := Pred(Count) downto 0 do
  begin
    Item := getInternalItems(Index);
    Proc(Item, AParam);
  end;
end;

{ TSDVOwnerList }

procedure TSDVOwnerList.DeleteAt(const AIndex: Integer);
//var Item: TObject;
begin
//  Item := getInternalItems(Index);
//  InternalDestroyItem(Item);
  InternalDeleteAt(AIndex);
  { Goal: Destroys of the list by its index .}
  { Objetivo: Destruye un elemento de la lista por su indice .}
end;

procedure TSDVOwnerList.Empty();
var AIndex: Integer;
begin
  // to while
  for AIndex := Pred(Count) downto 0 do
  begin
    InternalDeleteAt(AIndex);
  end;

  InternalEmpty();
  { Goal: Destroys all the items of the list.}
  { Objetivo: Destruye todos los elementos de la lista .}
end;

(* TSDVObjectList *)

function TSDVObjectList.getItems(AIndex: Integer): TObject;
begin
  Result := TObject(getInternalItems(AIndex));
end;

procedure TSDVObjectList.setItems(AIndex: Integer; AItem: TObject);
begin
  setInternalItems(AIndex, AItem);
end;

function TSDVObjectList.IndexOf(const AItem: TObject): Integer;
begin
  Result := InternalIndexOf(AItem);
end;

function TSDVObjectList.First(): TObject;
begin
  Result := TObject(InternalFirst());
end;

function TSDVObjectList.Last(): TObject;
begin
  Result := TObject(InternalLast());
end;

function TSDVObjectList.Insert(const AItem: TObject): Integer;
begin
  Result := Integer(InternalInsert(AItem));
end;

procedure TSDVObjectList.InsertAt(const AIndex: Integer; const AItem: TObject);
begin
  InternalInsertAt(AIndex, AItem);
end;

function TSDVObjectList.Extract(const AIndex: Integer): TObject;
begin
  Result := TObject(InternalExtract(AIndex));
end;

procedure TSDVObjectList.DeleteAt(const AIndex: Integer);
begin
  InternalDeleteAt(AIndex);
end;

function TSDVObjectList.Remove(const AItem: TObject): Integer;
begin
  Result := InternalRemove(AItem);
end;

procedure TSDVObjectList.Empty();
begin
  InternalEmpty();
end;

procedure TSDVObjectList.ForEach
  (const Proc: TSDVObjectListInmediate; const AParam: pointer);
var AIndex: Integer; AItem: TObject;
begin
  for AIndex := 0 to Pred(Count) do
  begin
    AItem := TObject(getInternalItems(AIndex));
    Proc(AItem, AParam);
  end;
end;

procedure TSDVObjectList.ForBack
  (const Proc: TSDVObjectListInmediate; const AParam: pointer);
var AIndex: Integer; AItem: TObject;
begin
  for AIndex := Pred(Count) downto 0 do
  begin
    AItem := TObject(getInternalItems(AIndex));
    Proc(AItem, AParam);
  end;
end;

end.
