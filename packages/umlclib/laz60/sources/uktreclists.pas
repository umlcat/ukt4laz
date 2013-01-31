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

unit uktreclists;

interface
uses 
  Classes,
  uktlists,
  dummy;

type

{ TSDVRecordList }

  TSDVRecordList = class(TSDVOwnerList)
  private
    { Private declarations }

    FRecordSize: Integer;
  protected
    { Protected declarations }

    function getItems(Index: Integer): pointer;
    function getRecordSize: Integer;

    procedure setItems(Index: Integer; Item: pointer);
    procedure setRecordSize(const Value: Integer);
  public
    { Public declarations }

    constructor Create(); override;

    function InternalCreateItem: pointer; virtual;
    procedure InternalDestroyItem(const Item: pointer); virtual;    

    function IndexOf(const Item: pointer): Integer;
    function First: pointer;
    function Last: pointer;

    function Insert: pointer;

    function Extract(const Index: Integer): pointer;
    function Remove(const Item: pointer): Integer;

    function FirstThat(const Func: TSDVPointerListFirstThatFunc; const Param: pointer): pointer;

    procedure ForEach(const Proc: TSDVPointerListForEachProc; const Param: pointer);
    procedure ForBack(const Proc: TSDVPointerListForEachProc; const Param: pointer);

    property Items[Index: Integer]: pointer
      read getItems write setItems; default;
    property RecordSize: Integer
      read getRecordSize write setRecordSize;
  end;

implementation

{ TSDVRecordList }

function TSDVRecordList.getItems(Index: Integer): pointer;
begin
  Result := getInternalItems(Index);
end;

function TSDVRecordList.getRecordSize: Integer;
begin
  Result := FRecordSize;
end;

procedure TSDVRecordList.setItems(Index: Integer; Item: pointer);
begin
  setInternalItems(Index, Item);
end;

procedure TSDVRecordList.setRecordSize(const Value: Integer);
begin
  FRecordSize := Value;
end;

constructor TSDVRecordList.Create();
begin
  inherited Create();
  FRecordSize := 0;
end;

function TSDVRecordList.InternalCreateItem: pointer;
begin
  System.GetMem(Result, FRecordSize);
  System.FillChar(Result^, FRecordSize, 0);
  // Goal: To create a new record of the previous defined type.
  // Objetivo: Crear un nuevo registro del tipo definido previamente.
end;

procedure TSDVRecordList.InternalDestroyItem(const Item: pointer);
begin
  System.FreeMem(Item, FRecordSize);
  // Goal: To destroy a given record of the previous defined type.
  // Objetivo: Destruir un registro dado del tipo definido previamente.
end;

function TSDVRecordList.IndexOf(const Item: Pointer): Integer;
begin
  Result := InternalIndexOf(Item);
end;

function TSDVRecordList.First: Pointer;
begin
  Result := InternalFirst;
end;

function TSDVRecordList.Last: Pointer;
begin
  Result := InternalLast;
end;

function TSDVRecordList.Insert: pointer;
begin
  Result := InternalCreateItem;
  InternalInsert(Result);
end;

function TSDVRecordList.Extract(const Index: Integer): pointer;
begin
  Result := Items[Index];
  DeleteAt(Index);
  // Goal: Removes & returns an item of the list without destroying it.
  // Objetivo: Remueve y regresa un elemento de la lista sin destruirlo.
end;

function TSDVRecordList.Remove(const Item: pointer): Integer;
begin
  Result := IndexOf(Item);
  if (Result <> IndexNotFound)
    then DeleteAt(Result);
  // Goal: Destroys of the list.
  // Objetivo: Destruye un elemento de la lista.
end;

function TSDVRecordList.FirstThat(const Func: TSDVPointerListFirstThatFunc;
  const Param: pointer): pointer;
var Found: Boolean; Index, LastIndex: Integer; Item: pointer;
begin
  Result := nil;

  Found := false;
  Index := 0;
  LastIndex  := Count;
  while (not Found and (Index < LastIndex)) do
  begin
    Item := getInternalItems(Index);
    Found := Func(Item, Param);
    Inc(Index);
  end;

  if (Found) then
  begin
    Result := Item;
  end;
end;

procedure TSDVRecordList.ForEach(const Proc: TSDVPointerListForEachProc; const Param: pointer);
var Index: Integer; Item: pointer;
begin
  for Index := 0 to Pred(Count) do
  begin
    Item := getInternalItems(Index);
    Proc(Item, Param);
  end;
end;

procedure TSDVRecordList.ForBack(const Proc: TSDVPointerListForEachProc; const Param: pointer);
var Index: Integer; Item: pointer;
begin
  for Index := Pred(Count) downto 0 do
  begin
    Item := getInternalItems(Index);
    Proc(Item, Param);
  end;
end;

end.
