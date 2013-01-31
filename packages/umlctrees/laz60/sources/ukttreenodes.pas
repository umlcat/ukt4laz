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

unit ukttreenodes;

interface
uses
  SysUtils,
  Classes,
  //Forms, Dialogs,
  uktactivatedcontrols,
  uktguids,
  //uktBooleans,
  uktlists,
  uktcomponents,
  dummy;

(**
 ** Description:
 ** This unit contains several in order to support an non visual,
 ** hierarchical ( "tree" ) collection of data.
 ** It was not designed with generics collections.
 **)

type

  TSDVTreeDirection = (tdNone, tdStart, tdFinish);

(* TSDVTreeNodeForEachProc *)

  TSDVTreeNode = class;

  TSDVTreeNodeFunc =
    function
      (const ANode: TSDVTreeNode; const AParam: pointer): Boolean of object;
  TSDVTreeNodeForEachProc =
    procedure
      (var ANode: TSDVTreeNode; const AParam: pointer) of object;
  TSDVTreeNodeForBothProc =
    procedure
      (var ANode: TSDVTreeNode;
       const AParam: pointer; const ADirection: TSDVTreeDirection) of object;

(* TSDVTreeNode *)

  TSDVTreeNodeList = class;
  TSDVTreeCollection = class;

  TSDVTreeNode = class(TSDVNormalizedPersistent)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    // unique identifier in collection
    FID:          TGUID;

    // not used, available to the controls user
    FData:        pointer;
  protected
    (* Protected declarations *)

    function getData(): pointer;
    function getID(): TGUID;

    procedure setData(const AValue: pointer);
    procedure setID(const AValue: TGUID);
  protected
    (* Protected declarations *)

    function MatchesNode
      (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
    function MatchesID
      (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
    function MatchesData
      (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
  protected
    (* Protected declarations *)

    function CreateList(): TSDVTreeNodeList; (* nonvirtual; *)

    procedure ConfirmedCopyTo(var ADestNode: TSDVTreeNode); virtual;
    procedure ConfirmedMoveTo(var ADestNode: TSDVTreeNode); virtual;
    procedure ConfirmedDuplicateTo(var ADestNode: TSDVTreeNode); virtual;
  public
    (* Protected Friend declarations *)

    (* DO NOT become public or published declarations *)

    // reference to collection that contains all treenodes
    // it's not the subitems
    FInternalCollection:  TSDVTreeCollection;
    FInternalIsRoot:      Boolean;
    FInternalLevel:       Integer;
    // similar libraries use "items" or "children" instead of "list"
    FInternalList:        TSDVTreeNodeList;
    FInternalLocalIndex:  Integer;
    FInternalGlobalIndex: Integer;
    FInternalParent:      TSDVTreeNode;
  public
    (* Protected Friend declarations *)

    (* DO NOT become public or published declarations *)

    function getInternalCollection(): TSDVTreeCollection;
    function getInternalIsRoot(): Boolean;
    function getInternalLevel(): Integer;
    function getInternalList(): TSDVTreeNodeList;
    function getInternalLocalIndex(): Integer;
    function getInternalGlobalIndex(): Integer;
    function getInternalParent(): TSDVTreeNode;

    procedure setInternalCollection(const AValue: TSDVTreeCollection);
    procedure setInternalIsRoot(const AValue: Boolean);
    procedure setInternalLevel(const AValue: Integer);
    procedure setInternalList(const AValue: TSDVTreeNodeList);
    procedure setInternalLocalIndex(const AValue: Integer);
    procedure setInternalGlobalIndex(const AValue: Integer);
    procedure setInternalParent(const AValue: TSDVTreeNode);
  public
    (* Protected Friend declarations *)

    (* DO NOT become public or published declarations *)

    // reference to collection that contains all treenodes
    // it's not the subitems
    property InternalCollection: TSDVTreeCollection
      read getInternalCollection write setInternalCollection;
    property InternalIsRoot: Boolean
      read getInternalIsRoot write setInternalIsRoot;
    property InternalLevel: Integer
      read getInternalLevel write setInternalLevel;
    property InternalList:  TSDVTreeNodeList
      read getInternalList write setInternalList;
    property InternalLocalIndex: Integer
      read getInternalLocalIndex write setInternalLocalIndex;
    property InternalGlobalIndex: Integer
      read getInternalGlobalIndex write setInternalGlobalIndex;
    property InternalParent: TSDVTreeNode
      read getInternalParent write setInternalParent;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Friend Protected declarations *)

    procedure ConfirmedInsertRoot(); virtual;
    procedure ConfirmedInsert(var ANode: TSDVTreeNode); virtual;
    procedure ConfirmedInsertAt
      (const AIndex: Integer; var ANode: TSDVTreeNode); virtual;

    procedure ConfirmedRemove(); virtual;
    procedure ConfirmedEmpty(); virtual;

    procedure UpdateLocalIndex
      (var ANode: TSDVTreeNode; const AParam: pointer);
    procedure UpdateLocalIndices();

    procedure DuplicateNode
      (var ANode: TSDVTreeNode;
       const AParam: pointer; const ADirection: TSDVTreeDirection);
  public
    (* Public declarations *)

    function Insert(): TSDVTreeNode; (* nonvirtual; *)
    function InsertAt
      (const AIndex: Integer): TSDVTreeNode; (* nonvirtual; *)

    function InsertFirst(): TSDVTreeNode; (* nonvirtual; *)

    function InsertAfterFirst(): TSDVTreeNode; (* nonvirtual; *)
    function InsertBeforeLast(): TSDVTreeNode; (* nonvirtual; *)

    procedure Remove(); (* nonvirtual; *)
    procedure Empty(); (* nonvirtual; *)
  public
    (* Public declarations *)

    procedure CopyTo(var ADestNode: TSDVTreeNode); (* nonvirtual; *)
    procedure MoveTo(var ADestNode: TSDVTreeNode); (* nonvirtual; *)

    function CanDuplicateTo(var ADestNode: TSDVTreeNode): Boolean;
    procedure DuplicateTo(var ADestNode: TSDVTreeNode); (* nonvirtual; *)
  public
    (* Public declarations *)

    function NodeByID(const ANID: TGUID): TSDVTreeNode;
    function NodeByData(const AData: pointer): TSDVTreeNode;

    function First(): TSDVTreeNode;
    function Prev(): TSDVTreeNode;
    function Next(): TSDVTreeNode;
    function Last(): TSDVTreeNode;
  protected
    (* Protected declarations *)

    function ConfirmedFirstThat
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function ConfirmedLastThat
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function ConfirmedFirstUp
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
  protected
    (* Protected declarations *)

    procedure ConfirmedForEachForward
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ConfirmedForEachBackward
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ConfirmedForUp
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ConfirmedForBoth
      (const AProc: TSDVTreeNodeForBothProc; const AParam: pointer);
  public
    (* Public declarations *)

    function FirstThat
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function LastThat
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function FirstUp
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;

    function FirstThatInmediate
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function LastThatInmediate
      (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
  public
    (* Public declarations *)

    procedure ForEachForward
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ForEachBackward
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ForUp
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ForBoth
      (const AProc: TSDVTreeNodeForBothProc; const AParam: pointer);

    procedure ForEachForwardInmediate
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ForEachBackwardInmediate
      (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
  public
    (* Public declarations *)

    (* Read-Only properties *)

    function Count(): Integer;
    function HasItems(): Boolean;

    function Collection(): TSDVTreeCollection;
    function GlobalIndex(): Integer;
    function Level(): Integer;
    function List(): TSDVTreeNodeList;
    function LocalIndex(): Integer;
    function IsRoot(): Boolean;
    function Parent(): TSDVTreeNode;
  public
    (* Public declarations *)

    (* DO NOT become published declarations *)

    property Data: pointer
      read getData write setData;
    property ID: TGUID
      read getID write setID;
  end;

 (*
  TSDVTreeNodeList. It's a sequential collection,
  not a hierarchical collection,
  that stores references to treenodes.

  It's used for memory management.
 *)

(* TSDVTreeNodeList *)

  TSDVTreeNodeList = class(TCustomSDVList)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function getItems(Index: Integer): TSDVTreeNode;

    procedure setItems(Index: Integer; Item: TSDVTreeNode);
  public
    (* Public declarations *)

    function IndexOf(const Item: TObject): Integer;
    function First(): TObject;
    function Last(): TObject;

    function Insert(const Item: TSDVTreeNode): Integer;
    procedure InsertAt
      (const AIndex: Integer; const Item: TSDVTreeNode);

    procedure Empty();
    procedure DeleteAt(const AIndex: Integer);

    function Remove(const Item: TObject): Integer;
    function Extract(const AIndex: Integer): TObject;

    function FirstThatInmediate
      (const Func: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
    function FirstBackInmediate
      (const Func: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;

    procedure ForEachForwardInmediate
      (const Proc: TSDVTreeNodeForEachProc; const AParam: pointer);
    procedure ForEachBackwardInmediate
      (const Proc: TSDVTreeNodeForEachProc; const AParam: pointer);

    property Items[Index: Integer]: TSDVTreeNode
      read getItems write setItems; default;
  end;

(**
 ** TSDVTreeCollection. It's a non visual, non G.U.I., non R.A.D.,
 ** hierarchical collection of items.
 **
 ** The collection its also the shared "Public Relations Department",
 ** for all nodes. External delegates, events or notifications,
 ** of nodes, to other objects, pass thru the collection.
 **)

(* TSDVTreeCollection *)

  TSDVTreeCollection = class(TSDVActivatedPersistent)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FInternalRoot: TSDVTreeNode;

    // this increments & reduces
    FGlobalCount: Integer;
    // this does not reduces as List.Count()
    FGlobalSequencer: Integer;
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; virtual;

    procedure CreateRoot();
    procedure DestroyRoot();
  public
    (* Friend Protected declarations *)

    FBeenIterated: Boolean;

    FClipboardSourceRootNode: TSDVTreeNode;
    FClipboardDestRootNode:   TSDVTreeNode;
    FClipboardDestPtrNode:    TSDVTreeNode;

    (* DO NOT become published declarations *)

    property InternalRoot: TSDVTreeNode
      read FInternalRoot write FInternalRoot;
  public
    (* Friend Protected declarations *)

    (* Read-Only properties *)

    function GlobalSequencer(): Integer;
  protected
    (* Protected declarations *)

    procedure InternalBeforeInsert
      (const AParentNode, ANewNode: TSDVTreeNode;
       const AIndex: Integer); virtual;
    procedure InternalAfterInsert
      (const AParentNode, ANewNode: TSDVTreeNode;
       const AIndex: Integer); virtual;

    procedure InternalConfirmedInsert
      (const AParentNode: TSDVTreeNode; var ANewNode: TSDVTreeNode); virtual;
    procedure InternalConfirmedInsertAt
      (const AParentNode: TSDVTreeNode;
       const AIndex: Integer; var ANewNode: TSDVTreeNode); virtual;

    procedure InternalBeforeRemove
      (const ANode: TSDVTreeNode); virtual;
    procedure InternalAfterRemove
      (const ANode: TSDVTreeNode); virtual;

    procedure InternalConfirmedRemove
      (const AParentNode: TSDVTreeNode;
       const ANode: TSDVTreeNode); virtual;

    procedure InternalBeforeEmpty
      (const ANode: TSDVTreeNode); virtual;
    procedure InternalAfterEmpty(
     const ANode: TSDVTreeNode); virtual;

    procedure InternalConfirmedEmpty
      (const ANode: TSDVTreeNode); virtual;
  public
    (* Friend Protected declarations *)

    function RequestInsert
      (const AParentNode: TSDVTreeNode): TSDVTreeNode;(* nonvirtual; *)
    function RequestInsertAt
      (const AParentNode: TSDVTreeNode;
       const AIndex: Integer): TSDVTreeNode; (* nonvirtual; *)

    procedure RequestRemove
      (const AParentNode: TSDVTreeNode;
       const ANode: TSDVTreeNode); (* nonvirtual; *)
    procedure RequestRemoveAt
      (const AParentNode: TSDVTreeNode;
       const AIndex: Integer); (* nonvirtual; *)

    procedure RequestEmpty
      (const ANode: TSDVTreeNode); (* nonvirtual; *)
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Public declarations *)

    function InsertRoot(): TSDVTreeNode; virtual;
    procedure DropRoot(); virtual;
  public
    (* Public declarations *)

    (* Read-Only properties *)

    function GlobalCount(): Integer;
    function IsEmpty(): Boolean;
    function BeenIterated(): Boolean;
    function Root(): TSDVTreeNode; virtual;
  end;

implementation

(* TSDVTreeNode *)

function TSDVTreeNode.getData(): pointer;
begin
  Result := FData;
end;

function TSDVTreeNode.getID(): TGUID;
begin
  Result := FID;
end;

procedure TSDVTreeNode.setData(const AValue: pointer);
begin
  if (FData <> AValue) then
  begin
    FData := AValue;
  end;
end;

procedure TSDVTreeNode.setID(const AValue: TGUID);
begin
  if (uktguids.EqualGUID(FID, AValue)) then
  begin
    FID := AValue;
  end;
end;

function TSDVTreeNode.MatchesNode
  (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
begin
  Result := (ANode = Self);
end;

function TSDVTreeNode.MatchesID
  (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
var AID: PGUID;
begin
  AID := PGUID(AParam);
  Result := EqualGUID(ANode.ID, AID^);
  // Goal: Returns if the selected node stores the given data.
  // Objetivo: Regresa si el nodo seleccionado almacena el dato dado.
end;

function TSDVTreeNode.MatchesData
  (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
begin
  Result := (ANode.Data = AParam);
  // Goal: Returns if the selected node stores the given data.
  // Objetivo: Regresa si el nodo seleccionado almacena el dato dado.
end;

function TSDVTreeNode.CreateList(): TSDVTreeNodeList;
begin
  Result := TSDVTreeNodeList.Create();
end;

procedure TSDVTreeNode.ConfirmedCopyTo(var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVTreeNode;
begin
  // perform copy of fields specific to parent class
  Self.DoNothing();

  // cast to current type
  ThisDestNode := TSDVTreeNode(ADestNode);

  // perform copy of fields specific to this class
  ThisDestNode.Data := Self.Data;
end;

procedure TSDVTreeNode.ConfirmedMoveTo(var ADestNode: TSDVTreeNode);
var ThisDestNode: TSDVTreeNode;
begin
  // perform copy of fields specific to parent class
  Self.DoNothing();

  // cast to current type
  ThisDestNode := TSDVTreeNode(ADestNode);

  // perform move of fields specific to this class
  ThisDestNode.Data := Self.Data;
  Self.Data      := nil;

  ThisDestNode.FID := Self.FID;
  uktguids.ClearGUID((* var *) Self.FID);

  ThisDestNode.FInternalGlobalIndex := Self.FInternalGlobalIndex;
  Self.FInternalGlobalIndex      := 0;

  // pendiente
  //FInternalIsRoot:      Boolean;

  // similar libraries use "items" or "children" instead of "list"
  // omitir, se maneja con otra operacion
  // FInternalLevel:       Integer;
  //FInternalCollection:  TSDVTreeCollection;
  // FInternalList:        TSDVTreeNodeList;
  // FInternalLocalIndex:  Integer;
  // FInternalParent:      TSDVTreeNode;

  // Note: "Move" means same treecollection, "export" or "import"
  // means different treecollection.
end;

procedure TSDVTreeNode.ConfirmedDuplicateTo(var ADestNode: TSDVTreeNode);
begin
  // store source root-node & destination root-node
  InternalCollection.FClipboardSourceRootNode := Self;
  InternalCollection.FClipboardDestRootNode   := ADestNode;

  // the duplication starts with he already assigned
  // destination parent node
  InternalCollection.FClipboardDestPtrNode :=
    InternalCollection.FClipboardDestRootNode;

  {$ifdef Delphi}
  // does not apply under iteration block !!!
  InternalCollection.FClipboardSourceContainerNode.ConfirmedForBoth
    (DuplicateNode, nil);
  {$endif}

  {$ifdef FPC}
  // does not apply under iteration block !!!
  InternalCollection.FClipboardSourceRootNode.ConfirmedForBoth
    (@DuplicateNode, nil);
  {$endif}
end;

function TSDVTreeNode.getInternalCollection(): TSDVTreeCollection;
begin
  Result := FInternalCollection;
end;

function TSDVTreeNode.getInternalGlobalIndex(): Integer;
begin
  Result := FInternalGlobalIndex;
end;

function TSDVTreeNode.getInternalLevel(): Integer;
begin
  Result := FInternalLevel;
end;

function TSDVTreeNode.getInternalList(): TSDVTreeNodeList;
begin
  Result := FInternalList;
end;

function TSDVTreeNode.getInternalIsRoot(): Boolean;
begin
  Result := FInternalIsRoot;
end;

function TSDVTreeNode.getInternalLocalIndex(): Integer;
begin
  Result := FInternalLocalIndex;
end;

function TSDVTreeNode.getInternalParent(): TSDVTreeNode;
begin
  Result := FInternalParent;
end;

procedure TSDVTreeNode.setInternalGlobalIndex(const AValue: Integer);
begin
  if (FInternalGlobalIndex <> AValue) then
  begin
    FInternalGlobalIndex := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalCollection
  (const AValue: TSDVTreeCollection);
begin
  if (FInternalCollection <> AValue) then
  begin
    FInternalCollection := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalIsRoot(const AValue: Boolean);
begin
  if (FInternalIsRoot <> AValue) then
  begin
    FInternalIsRoot := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalLevel(const AValue: Integer);
begin
  if (FInternalLevel <> AValue) then
  begin
    FInternalLevel := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalList(const AValue: TSDVTreeNodeList);
begin
  if (FInternalList <> AValue) then
  begin
    FInternalList := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalLocalIndex(const AValue: Integer);
begin
  if (FInternalLocalIndex <> AValue) then
  begin
    FInternalLocalIndex := AValue;
  end;
end;

procedure TSDVTreeNode.setInternalParent(const AValue: TSDVTreeNode);
begin
  if (FInternalParent <> AValue) then
  begin
    FInternalParent := AValue;
  end;
end;

procedure TSDVTreeNode.DoCreate();
begin
  inherited DoCreate();

  EmptyGUID( (* var *) FID);

  FInternalGlobalIndex := 0;
  FInternalLocalIndex  := -1;
  FInternalLevel       := 0;

  FData := nil;

  FInternalParent := nil;
  FInternalList   := CreateList();
  FInternalCollection := nil;
end;

procedure TSDVTreeNode.DoDestroy();
begin
  FData := nil;

  FInternalList.Free();
  FInternalList := nil;
  inherited DoDestroy();
end;

procedure TSDVTreeNode.ConfirmedInsertRoot();
begin
  Self.InternalIsRoot := TRUE;
  Self.InternalLevel  := 0;
  // assign default properties
  // asignar propiedades default

  Self.InternalParent  := nil;
  // insert into parent node*s item*s list
  // insertar en lista de elementos del nodo padre

  Self.InternalLocalIndex := 0;
  // insert into global treenodes* list
  // insertar en lista de nodosarboles global
end;

procedure TSDVTreeNode.ConfirmedInsert(var ANode: TSDVTreeNode);
begin
  ANode.InternalIsRoot := FALSE;
  ANode.InternalLevel  := (InternalLevel + 1);
  // assign default properties
  // asignar propiedades default

  //ShowMessage(IntToStr(AParentNode.List.Count));

  ANode.InternalParent  := Self;
  InternalList.Insert(ANode);
  ANode.InternalLocalIndex := (InternalList.Count - 1);
  // insert into parent node*s item*s list
  // insertar en lista de elementos del nodo padre
end;

procedure TSDVTreeNode.ConfirmedInsertAt
  (const AIndex: Integer; var ANode: TSDVTreeNode);
begin
  ANode.InternalIsRoot := FALSE;
  ANode.InternalLevel  := (InternalLevel + 1);
  // assign default properties
  // asignar propiedades default

  ANode.InternalParent  := Self;
  InternalList.InsertAt(AIndex, ANode);
  ANode.InternalLocalIndex := AIndex;
  // insert into parent node*s item*s list
  // insertar en lista de elementos del nodo padre

  // to-do: check index for all nodes
end;

procedure TSDVTreeNode.ConfirmedRemove();
begin
  if (InternalParent <> nil) then
  begin
    // remove from parent node's subitems list
    InternalParent.InternalList.DeleteAt(Self.InternalLocalIndex);
  end;

  // delete itself from memory
  Self.DoDestroy();
  Self.Free();
end;

procedure TSDVTreeNode.ConfirmedEmpty();
var EachIndex, LastIndex: Integer;
  EachNode: TSDVTreeNode;
begin
  // when deleting pointers, doesn't apply iterators here
  // al eliminar apuntadores, no aplican interadores aqui
  LastIndex := (InternalList.Count + 1);
  for EachIndex := 0 to LastIndex do
  begin
    EachNode := TSDVTreeNode(Self.InternalList.Items[EachIndex]);
    EachNode.Remove();
  end;

  // delete from internal list
  // eliminar de lista interna
  List.Empty();
end;

procedure TSDVTreeNode.UpdateLocalIndex
  (var ANode: TSDVTreeNode; const AParam: pointer);
var AIndex: ^Integer absolute AParam;
begin
  ANode.InternalLocalIndex := AIndex^;
  Inc(AIndex^);
end;

procedure TSDVTreeNode.UpdateLocalIndices();
var AIndex: Integer;
begin
  AIndex := 0;

  {$ifdef Delphi}
  ForEachForwardInmediate(UpdateLocalIndex, @AIndex);
  {$endif}

  {$ifdef FPC}
  ForEachForwardInmediate(@UpdateLocalIndex, @AIndex);
  {$endif}
end;

procedure TSDVTreeNode.DuplicateNode
  (var ANode: TSDVTreeNode;
   const AParam: pointer; const ADirection: TSDVTreeDirection);
begin
  if (ADirection = ukttreenodes.tdStart) then
  begin
    // add a new node, and make dest-ptr reference it
    InternalCollection.FClipboardDestPtrNode :=
      InternalCollection.FClipboardDestPtrNode.Insert();

    // assign fields that can be copied
    ANode.CopyTo(InternalCollection.FClipboardDestPtrNode);
  end;

  if (ADirection = ukttreenodes.tdFinish) then
  begin
    // make dest-ptr reference its own parent node
    InternalCollection.FClipboardDestPtrNode :=
      InternalCollection.FClipboardDestPtrNode.Parent();
  end;
end;

function TSDVTreeNode.Insert(): TSDVTreeNode;
begin
  Result := nil;
  if (not InternalCollection.FBeenIterated) then
  begin
    Result := InternalCollection.RequestInsert(Self);
  end;
end;

function TSDVTreeNode.InsertAt(const AIndex: Integer): TSDVTreeNode;
begin
  Result := nil;
  if (not InternalCollection.FBeenIterated) then
  begin
    Result := InternalCollection.RequestInsertAt(Self, AIndex);
  end;
end;

function TSDVTreeNode.InsertFirst(): TSDVTreeNode;
begin
  Result := Self.InsertAt(0);
end;

function TSDVTreeNode.InsertAfterFirst(): TSDVTreeNode;
var ACount: Integer;
begin
  Result := nil;

  ACount := Self.List().Count;
  if (ACount > 1) then
  begin
    Result := Self.InsertAt(1);
  end else
  begin
    Result := Self.Insert();
  end;
end;

function TSDVTreeNode.InsertBeforeLast(): TSDVTreeNode;
var ACount, AIndex: Integer;
begin
  Result := nil;

  ACount := Self.List().Count;
  if (ACount > 1) then
  begin
    AIndex := (ACount - 1);
    Result := Self.InsertAt(AIndex);
  end else
  begin
    Result := Self.Insert();
  end;
end;

procedure TSDVTreeNode.Remove();
begin
  // Notify collection of removal,
  // does not remove from memory here, yet
  if (not InternalCollection.FBeenIterated) then
  begin
    InternalCollection.RequestRemove(Self.Parent(), Self);
  end;
end;

procedure TSDVTreeNode.Empty();
begin
  // Notify collection of removal,
  // does not remove from memory here, yet
  if (not InternalCollection.FBeenIterated) then
  begin
    InternalCollection.RequestEmpty(Self);
  end;
end;

procedure TSDVTreeNode.CopyTo(var ADestNode: TSDVTreeNode);
begin
  if (ADestNode <> nil) then
  begin
    Self.ConfirmedCopyTo((* var *) ADestNode);
  end;
end;

procedure TSDVTreeNode.MoveTo(var ADestNode: TSDVTreeNode);
begin
  if (ADestNode <> nil) then
  begin
    Self.ConfirmedMoveTo((* var *) ADestNode);
  end;
end;

function TSDVTreeNode.CanDuplicateTo(var ADestNode: TSDVTreeNode): Boolean;
var ANode: TSDVTreeNode;
begin
  Result := (ADestNode <> nil);
  if (Result) then
  begin
    // verify destnode should NOT be the child of the sourcenode,
    // or the sourcenode, itself

    // matches
    {$ifdef FPC}
    ANode := ADestNode.FirstUp(@MatchesNode, Self);
    Result := (ANode = nil);
    {$endif}
  end;
end;

procedure TSDVTreeNode.DuplicateTo(var ADestNode: TSDVTreeNode);
begin
  if (CanDuplicateTo(ADestNode)) then
  begin
    ConfirmedDuplicateTo(ADestNode);
  end;
end;

function TSDVTreeNode.NodeByID(const ANID: TGUID): TSDVTreeNode;
begin
  {$ifdef Delphi}
  Result := FirstThat(MatchesID, @ANID);
  {$endif}
  {$ifdef FPC}
  Result := FirstThat(@MatchesID, @ANID);
  {$endif}
  // Goal: Returns the node that stores the given data.
  // Objetivo: Regresa el nodo que almacena el dato indicado.
end;

function TSDVTreeNode.NodeByData(const AData: pointer): TSDVTreeNode;
begin
  {$ifdef Delphi}
  Result := FirstThat(MatchesData, Data);
  {$else}
  Result := FirstThat(@MatchesData, AData);
  {$endif}
  // Goal: Returns the node that stores the given data.
  // Objetivo: Regresa el nodo que almacena el dato indicado.
end;

function TSDVTreeNode.First(): TSDVTreeNode;
begin
  Result := (List.First() as TSDVTreeNode);
  // Goal: Returns the first child node.
  // Objetivo: Regresa el primer nodo hijo.
end;

function TSDVTreeNode.Prev(): TSDVTreeNode;
begin
  Result := nil;
  if ((InternalParent <> nil) and (InternalGlobalIndex > 0)) then
  begin
    Result := InternalParent.List.Items[InternalGlobalIndex - 1]
  end;
  // Goal: Returns the previous node.
  // Objetivo: Regresa el nodo previo.
end;

function TSDVTreeNode.Next(): TSDVTreeNode;
begin
  Result := nil;
  if ((InternalParent <> nil) and (InternalGlobalIndex < InternalParent.Count)) then
  begin
    Result := InternalParent.List.Items[InternalGlobalIndex + 1];
  end;
  // Goal: Returns the next node.
  // Objetivo: Regresa el siguiente nodo.
end;

function TSDVTreeNode.Last(): TSDVTreeNode;
begin
  Result := (List.Last() as TSDVTreeNode);
  // Goal: Returns the last child node.
  // Objetivo: Regresa el ultimo nodo hijo.
end;

function TSDVTreeNode.ConfirmedFirstThat
 (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
var AIndex, LastIndex: Integer; Item: TSDVTreeNode; Found: Boolean;
begin
  Result := nil;
  Found  := AFunc(Self, AParam);

  if (Found) then
  begin
    Result := Self;
  end else
  begin
    AIndex := 0; Found := FALSE; LastIndex := (Count() - 1);
    while ((AIndex <= LastIndex) and (not Found)) do
    begin
      Item := List.Items[AIndex];

      Result := Item.ConfirmedFirstThat(AFunc, AParam);
      Found  := (Result <> nil);

      if (not Found) then
      begin
        Inc(AIndex);
      end;
    end;
  end;
end;

function TSDVTreeNode.ConfirmedLastThat
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
var AIndex: Integer; Item: TSDVTreeNode; Found: Boolean;
begin
  Result := nil;
  Found := AFunc(Self, AParam);
  if (Found) then
  begin
    Result := Self;
  end else
  begin
    AIndex := (Count - 1); Found := FALSE;
    while ((AIndex >= 0) and (not Found)) do
    begin
      Item := List.Items[AIndex];

      Result := Item.ConfirmedLastThat(AFunc, AParam);
      Found  := (Result <> nil);

      if (not Found) then
      begin
        Dec(AIndex);
      end;
    end;
  end;
end;

function TSDVTreeNode.ConfirmedFirstUp
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  if (not AFunc(Self, AParam)) then
  begin
    if (InternalParent <> nil) then
    begin
      Result := InternalParent.ConfirmedFirstUp(AFunc, AParam);
    end;
  end else
  begin
    Result := Self;
  end;
end;

procedure TSDVTreeNode.ConfirmedForEachForward
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
var AIndex, LastIndex: Integer; Item: TSDVTreeNode;
  D: string;
begin
  AProc(Self, AParam);

  LastIndex := (List.Count - 1);
  if (List.Count > 0) then
  begin
    for AIndex := 0 to LastIndex do
    begin
      Item := List.Items[AIndex];
      Item.ConfirmedForEachForward(AProc, AParam);
    end;
  end;
  // Goal: Execute a procedure in each treenode,
  // starting with the parentnode.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con el nodopadre.
end;

procedure TSDVTreeNode.ConfirmedForEachBackward
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
var AIndex, LastIndex: Integer; Item: TSDVTreeNode;
begin
  LastIndex := (List.Count - 1);
  for AIndex := LastIndex downto 0 do
  begin
    Item := List.Items[AIndex];
    Item.ForEachBackward(AProc, AParam);
  end;
  AProc(Self, AParam);
  // Goal: Execute a procedure in each treenode,
  // starting with the children nodes.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con los nodos hijo.
end;

procedure TSDVTreeNode.ConfirmedForUp
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  AProc(Self, AParam);
  if (Self.InternalParent <> nil) then
  begin
    Self.InternalParent.ConfirmedForUp(AProc, AParam);
  end;
  // Goal: Execute a procedure in each treenode,
  // starting with the current node and continue with each InternalParent.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con el nodo actual y continuando con cada nodo padre.
end;

procedure TSDVTreeNode.ConfirmedForBoth
 (const AProc: TSDVTreeNodeForBothProc; const AParam: pointer);
var AIndex, LastIndex: Integer; Item: TSDVTreeNode;
begin
  AProc(Self, AParam, tdStart);

  LastIndex := (List.Count - 1);
  for AIndex := 0 to LastIndex do
  begin
    Item := List.Items[AIndex];
    Item.ConfirmedForBoth(AProc, AParam);
  end;
  AProc(Self, AParam, tdFinish);
  // Goal: Execute a procedure in each treenode,
  // starting with the children nodes.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con los nodos hijo.
end;

function TSDVTreeNode.FirstThat
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  InternalCollection.FBeenIterated := true;
  Result := ConfirmedFirstThat(AFunc, AParam);
  InternalCollection.FBeenIterated := false;
end;

function TSDVTreeNode.LastThat
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  InternalCollection.FBeenIterated := true;
  Result := ConfirmedLastThat(AFunc, AParam);
  InternalCollection.FBeenIterated := false;
end;

function TSDVTreeNode.FirstUp
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  InternalCollection.FBeenIterated := true;
  Result := ConfirmedFirstUp(AFunc, AParam);
  InternalCollection.FBeenIterated := false;
end;

function TSDVTreeNode.FirstThatInmediate
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  InternalCollection.FBeenIterated := true;
  Result := List.FirstThatInmediate(AFunc, AParam);
  InternalCollection.FBeenIterated := false;
end;

function TSDVTreeNode.LastThatInmediate
  (const AFunc: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
begin
  Result := nil;
  InternalCollection.FBeenIterated := true;
  Result := List.FirstBackInmediate(AFunc, AParam);
  InternalCollection.FBeenIterated := false;
end;

procedure TSDVTreeNode.ForEachForward
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  ConfirmedForEachForward(AProc, AParam);
  InternalCollection.FBeenIterated := false;
  // Goal: Execute a procedure in each treenode,
  // starting with the parentnode.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con el nodopadre.
end;

procedure TSDVTreeNode.ForEachBackward
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  ConfirmedForEachBackward(AProc, AParam);
  InternalCollection.FBeenIterated := false;
  // Goal: Execute a procedure in each treenode,
  // starting with the children nodes.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con los nodos hijo.
end;

procedure TSDVTreeNode.ForUp
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  ConfirmedForUp(AProc, AParam);
  InternalCollection.FBeenIterated := false;
  // Goal: Execute a procedure in each treenode,
  // starting with the current node and continue with each InternalParent.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con el nodo actual y continuando con cada nodo padre.
end;

procedure TSDVTreeNode.ForBoth
  (const AProc: TSDVTreeNodeForBothProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  ConfirmedForBoth(AProc, AParam);
  InternalCollection.FBeenIterated := false;
  // Goal: Execute a procedure in each treenode,
  // starting with the children nodes.

  // Objetivo: Ejecutar un procedimiento en cada nodoarbol,
  // comenzando con los nodos hijo.
end;

procedure TSDVTreeNode.ForEachForwardInmediate
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  List.ForEachForwardInmediate(AProc, AParam);
  InternalCollection.FBeenIterated := false;
end;

procedure TSDVTreeNode.ForEachBackwardInmediate
  (const AProc: TSDVTreeNodeForEachProc; const AParam: pointer);
begin
  InternalCollection.FBeenIterated := true;
  List.ForEachBackwardInmediate(AProc, AParam);
  InternalCollection.FBeenIterated := false;
end;

function TSDVTreeNode.Count(): Integer;
begin
  Result := List.Count;
end;

function TSDVTreeNode.HasItems(): Boolean;
begin
  Result := not (List.IsEmpty());
end;

function TSDVTreeNode.Collection(): TSDVTreeCollection;
begin
  Result := FInternalCollection;
end;

function TSDVTreeNode.GlobalIndex(): Integer;
begin
  Result := FInternalGlobalIndex;
end;

function TSDVTreeNode.Level(): Integer;
begin
  Result := FInternalLevel;
end;

function TSDVTreeNode.List(): TSDVTreeNodeList;
begin
  Result := FInternalList;
end;

function TSDVTreeNode.LocalIndex(): Integer;
begin
  Result := FInternalLocalIndex;
end;

function TSDVTreeNode.IsRoot(): Boolean;
begin
  Result := FInternalIsRoot;
end;

function TSDVTreeNode.Parent(): TSDVTreeNode;
begin
  Result := FInternalParent;
end;

(* TSDVTreeNodeList *)

function TSDVTreeNodeList.getItems(Index: Integer): TSDVTreeNode;
begin
  Result := TSDVTreeNode(getInternalItems(Index));
end;

procedure TSDVTreeNodeList.setItems(Index: Integer; Item: TSDVTreeNode);
begin
  setInternalItems(Index, Pointer(Item));
end;

function TSDVTreeNodeList.IndexOf(const Item: TObject): Integer;
begin
  Result := InternalIndexOf(Item);
end;

function TSDVTreeNodeList.First(): TObject;
begin
  Result := TObject(InternalFirst());
end;

function TSDVTreeNodeList.Last(): TObject;
begin
  Result := TObject(InternalLast());
end;

function TSDVTreeNodeList.Insert(const Item: TSDVTreeNode): Integer;
begin
  Result := InternalInsert(Item);
end;

procedure TSDVTreeNodeList.InsertAt(const AIndex: Integer; const Item: TSDVTreeNode);
begin
  InternalInsertAt(AIndex, Pointer(Item));
end;

procedure TSDVTreeNodeList.Empty();
var AIndex, ALast: Integer;
begin
  if (Count <> 0) then
  begin
    ALast := (Count - 1);
    for AIndex := ALast to 0 do
    begin
      InternalDeleteAt(AIndex);
    end;
  end;
  InternalEmpty();
end;

procedure TSDVTreeNodeList.DeleteAt(const AIndex: Integer);
begin
  InternalDeleteAt(AIndex);
end;

function TSDVTreeNodeList.Remove(const Item: TObject): Integer;
begin
  Result := InternalRemove(Pointer(Item));
end;

function TSDVTreeNodeList.Extract(const AIndex: Integer): TObject;
begin
  Result := TObject(InternalExtract(AIndex));
end;

function TSDVTreeNodeList.FirstThatInmediate
  (const Func: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
var AIndex, LastIndex: Integer; AItem: TSDVTreeNode; Found: Boolean;
begin
  Result := nil; AItem:= nil;

  AIndex := 0; Found := FALSE; LastIndex := (Count - 1);
  while ((AIndex <= LastIndex) and (not Found)) do
  begin
    AItem := TSDVTreeNode(getInternalItems(AIndex));
    Found := Func(AItem, AParam);

    if (not Found) then
    begin
      Inc(AIndex);
    end;
  end;

  if (Found) then
  begin
    Result := AItem;
  end;
end;

function TSDVTreeNodeList.FirstBackInmediate
  (const Func: TSDVTreeNodeFunc; const AParam: pointer): TSDVTreeNode;
var AIndex: Integer; AItem: TSDVTreeNode; Found: Boolean;
begin
  Result := nil; AItem:= nil;
  AIndex := (Count - 1); Found := FALSE;
  while ((AIndex >= 0) and (not Found)) do
  begin
    AItem := TSDVTreeNode(getInternalItems(AIndex));
    Found := Func(AItem, AParam);

    if (not Found) then
    begin
      Dec(AIndex);
    end;
  end;

  if (Found) then
  begin
    Result := AItem;
  end;
end;

procedure TSDVTreeNodeList.ForEachForwardInmediate
  (const Proc: TSDVTreeNodeForEachProc; const AParam: pointer);
var AIndex, LastIndex: Integer; AItem: TSDVTreeNode;
begin
  LastIndex := (Count - 1);
  for AIndex := 0 to LastIndex do
  begin
    AItem := TSDVTreeNode(getInternalItems(AIndex));
    Proc(AItem, AParam);
  end;
end;

procedure TSDVTreeNodeList.ForEachBackwardInmediate
  (const Proc: TSDVTreeNodeForEachProc; const AParam: pointer);
var AIndex, LastIndex: Integer; AItem: TSDVTreeNode;
begin
  LastIndex := (Count - 1);
  for AIndex := LastIndex downto 0 do
  begin
    AItem := TSDVTreeNode(getInternalItems(AIndex));
    Proc(AItem, AParam);
  end;
end;

(* TSDVTreeCollection *)

function TSDVTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVTreeNode.Create();
end;

procedure TSDVTreeCollection.CreateRoot();
begin
  FInternalRoot := RequestInsert(nil);
  FInternalRoot.InternalCollection := Self;
end;

procedure TSDVTreeCollection.DestroyRoot();
begin
  if (FInternalRoot <> nil) then
  begin
    FInternalRoot.Remove();
    FInternalRoot := nil;
  end;
end;

function TSDVTreeCollection.GlobalSequencer(): Integer;
begin
  Result := FGlobalSequencer;
end;

procedure TSDVTreeCollection.InternalBeforeInsert
  (const AParentNode, ANewNode: TSDVTreeNode;
   const AIndex: Integer);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalAfterInsert
   (const AParentNode, ANewNode: TSDVTreeNode;
    const AIndex: Integer);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalConfirmedInsert
  (const AParentNode: TSDVTreeNode; var ANewNode: TSDVTreeNode);
var BecomeRoot: Boolean;
begin
  // "CreateNodeByClass()" is polymorphic
  ANewNode := Self.CreateNodeByClass();

  // assign relevant properties
  ANewNode.InternalCollection  := Self;
  uktguids.CreateGUID(ANewNode.FID);

  // perform insertion
  BecomeRoot := (AParentNode = nil);
  if (BecomeRoot) then
  begin
    ANewNode.InternalGlobalIndex := 0;
    ANewNode.ConfirmedInsertRoot();
  end else
  begin
    ANewNode.InternalGlobalIndex := GlobalSequencer();
    AParentNode.ConfirmedInsert(ANewNode);
    AParentNode.UpdateLocalIndices();
  end;

  // some "shared" after operations
  Inc(FGlobalSequencer);
end;

procedure TSDVTreeCollection.InternalConfirmedInsertAt
  (const AParentNode: TSDVTreeNode;
   const AIndex: Integer;
   var ANewNode: TSDVTreeNode);
begin
  // "CreateNodeByClass()" is polymorphic
  ANewNode := Self.CreateNodeByClass();

  // assign relevant properties
  ANewNode.InternalCollection  := Self;
  ANewNode.InternalGlobalIndex := GlobalSequencer();
  uktguids.CreateGUID(ANewNode.FID);

  // perform insertion
  AParentNode.ConfirmedInsertAt(AIndex, ANewNode);

  // some "shared" after operations
  AParentNode.UpdateLocalIndices();
  Inc(FGlobalSequencer);
end;

procedure TSDVTreeCollection.InternalBeforeRemove
    (const ANode: TSDVTreeNode);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalAfterRemove
  (const ANode: TSDVTreeNode);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalConfirmedRemove
  (const AParentNode: TSDVTreeNode;
   const ANode: TSDVTreeNode);
begin
  if (ANode = FInternalRoot) then
  begin
    FInternalRoot := nil;
  end;

  // now, apply the removal
  ANode.ConfirmedRemove();

  // some "shared" after operations
  if (AParentNode <> nil) then
  begin
    AParentNode.UpdateLocalIndices();
  end;
end;

procedure TSDVTreeCollection.InternalBeforeEmpty
  (const ANode: TSDVTreeNode);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalAfterEmpty
  (const ANode: TSDVTreeNode);
begin
  Self.DoNothing();
end;

procedure TSDVTreeCollection.InternalConfirmedEmpty
  (const ANode: TSDVTreeNode);
begin
  // now, apply the empty action
  ANode.ConfirmedEmpty();
end;

function TSDVTreeCollection.RequestInsert
  (const AParentNode: TSDVTreeNode): TSDVTreeNode;
begin
  Result := nil;

  InternalBeforeInsert(AParentNode, Result, uktlists.IgnoreIndex);
  InternalConfirmedInsert(AParentNode, (* var *) Result);
  InternalAfterInsert(AParentNode, Result, uktlists.IgnoreIndex);
end;

function TSDVTreeCollection.RequestInsertAt
  (const AParentNode: TSDVTreeNode;
   const AIndex: Integer): TSDVTreeNode;
begin
  Result := nil;

  InternalBeforeInsert(AParentNode, Result, AIndex);
  InternalConfirmedInsertAt(AParentNode, AIndex, (* var *) Result);
  InternalAfterInsert(AParentNode, Result, AIndex);
end;

procedure TSDVTreeCollection.DoCreate();
begin
  inherited DoCreate();

  // root will be assigned, later
  FInternalRoot := nil;
  FBeenIterated := false;

  FGlobalCount     := 0;
  FGlobalSequencer := 0;
  // add reference to treenodes
end;

procedure TSDVTreeCollection.DoDestroy();
begin
  // maybe, there is a root
  DestroyRoot();

  inherited DoDestroy();
end;

procedure TSDVTreeCollection.RequestRemove
  (const AParentNode: TSDVTreeNode;
   const ANode: TSDVTreeNode);
begin
  InternalBeforeRemove(ANode);
  InternalConfirmedRemove(AParentNode, ANode);
  InternalAfterRemove(ANode);
  // Goal: Removes the given item from the collection.
  // Objetivo: Remueve el elemento dado de la coleccion.
end;

procedure TSDVTreeCollection.RequestRemoveAt
  (const AParentNode: TSDVTreeNode; const AIndex: Integer);
begin
  (*
  InternalBeforeRemoveAt(Result, AIndex);
  InternalConfirmedRemoveAt(AParentNode, AIndex, .* var *. Result);
  InternalAfterRemoveAt(AParentNode, Result, AIndex);
  *)
end;

procedure TSDVTreeCollection.RequestEmpty
  (const ANode: TSDVTreeNode);
begin
  InternalBeforeEmpty(ANode);
  InternalConfirmedEmpty(ANode);
  InternalAfterEmpty(ANode);
  // Goal: Removes all the subitems from the given item,
  // without removing the item itself.

  // Objetivo: Remueve todos los subelementos del elemento indicado,
  // sin remover el elemento en si mismo.
end;

function TSDVTreeCollection.GlobalCount(): Integer;
begin
  Result := FGlobalCount;
end;

function TSDVTreeCollection.InsertRoot(): TSDVTreeNode;
begin
  Result := nil;
  if (not FBeenIterated) then
  begin
    // check previous existing root node
    Self.DestroyRoot();

    Self.CreateRoot();
    Result := Self.InternalRoot;
  end;
  // Goal: Inserts the root item to the collection.
  // Objetivo: Inserta el elemento raiz en la coleccion.
end;

procedure TSDVTreeCollection.DropRoot();
begin
  if (not FBeenIterated) then
  begin
    // check previous existing root node
    Self.DestroyRoot();
  end;
end;

function TSDVTreeCollection.IsEmpty(): Boolean;
begin
  Result := (FInternalRoot = nil);
end;

function TSDVTreeCollection.BeenIterated(): Boolean;
begin
  Result := FBeenIterated;
end;

function TSDVTreeCollection.Root(): TSDVTreeNode;
begin
  Result := FInternalRoot;
end;

end.
