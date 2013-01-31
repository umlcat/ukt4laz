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

unit ukttreecntrs;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, Classes,
  uktlists,
  ukttreestates,
  ukttreenodes,
  uktactivatedcontrols,
  ukttextreenodes,
  dummy;

type

  TSDVContainerTreeNode = class;

  TOnSDVBeforeChangeStringTreeNodeEvent =
    procedure
      (const ANode: TSDVContainerTreeNode;
       const AValue: string)
      of object;

  TOnSDVAfterChangeStringTreeNodeEvent =
    procedure
      (const ANode: TSDVContainerTreeNode)
      of object;

  TOnSDVBeforeBooleanTreeNodeEvent =
    procedure
      (const ANode: TSDVContainerTreeNode;
       const AValue: Boolean)
      of object;

  TOnSDVAfterBooleanTreeNodeEvent =
    procedure
      (const ANode: TSDVContainerTreeNode)
      of object;

  TOnSDVInsertAtTreeNodeEvent =
    procedure
    (const AParentNode, ANode: TSDVContainerTreeNode;
     const AIndex: Integer)
      of object;

  TOnSDVTreeContainerNodeEvent =
    procedure
      (const ANode: TSDVContainerTreeNode)
      of object;

(* TSDVContainerTreeNode *)

  TCustomSDVTreeContainer = class;

  TSDVContainerTreeNode = class(TSDVTextTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FSelected:   Boolean;
  protected
    (* Protected declarations *)

    (* Accessors declarations *)

    function getSelected(): Boolean; virtual;

    procedure setSelected(const AValue: Boolean); virtual;
  protected
    (* Protected declarations *)

    procedure ConfirmedCopyTo(var ADestNode: TSDVTreeNode); override;
    procedure ConfirmedMoveTo(var ADestNode: TSDVTreeNode); override;
  protected
    (* Protected declarations *)

    procedure ApplySelectAll
      (var ANode: TSDVTreeNode; const AParam: pointer);
    procedure ApplySelectNone
      (var ANode: TSDVTreeNode; const AParam: pointer);
    procedure ApplySelectToggle
      (var ANode: TSDVTreeNode; const AParam: pointer);
    procedure ApplySelectAs
      (var ANode: TSDVTreeNode; const AParam: pointer);
  public
    (* Friend Protected declarations *)

    procedure ConfirmedChangeSelected(const ASelected: Boolean); virtual;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Public declarations *)

    procedure SelectAll();
    procedure SelectNone();
    procedure SelectToggle();
    procedure SelectAs(const AValue: Boolean);
  public
    (* Public declarations *)

    property Selected: Boolean
      read getSelected write setSelected;
  end;

(* TSDVContainerTreeCollection *)

  TSDVContainerTreeCollection = class(TSDVTextTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FContainer:  TCustomSDVTreeContainer;
  protected
    (* Protected declarations *)

    (* Accessors declarations *)

    function getContainer(): TCustomSDVTreeContainer;

    procedure setContainer(const AValue: TCustomSDVTreeContainer);
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  protected
    (* Protected declarations *)

    procedure InternalBeforeChangeText
      (const ANode: TSDVTextTreeNode;
       const AText: string); override;
    procedure InternalAfterChangeText
      (const ANode: TSDVTextTreeNode); override;

    procedure InternalBeforeChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const ASelected: Boolean); virtual;
    procedure InternalAfterChangeSelected
      (const ANode: TSDVContainerTreeNode); virtual;
    procedure InternalConfirmedChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const ASelected: Boolean); virtual;

    procedure InternalBeforeInsert
      (const AParentNode, ANewNode: TSDVTreeNode;
       const AIndex: Integer); override;
    procedure InternalAfterInsert
      (const AParentNode, ANewNode: TSDVTreeNode;
       const AIndex: Integer); override;

    procedure InternalBeforeRemove
      (const ANode: TSDVTreeNode); override;
    procedure InternalAfterRemove
      (const ANode: TSDVTreeNode); override;

    procedure InternalBeforeEmpty
      (const ANode: TSDVTreeNode); override;
    procedure InternalAfterEmpty(
     const ANode: TSDVTreeNode); override;
  public
    (* Friend Protected declarations *)

    procedure RequestChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const ASelected: Boolean); (* nonvirtual; *)
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  public
    (* Public declarations *)

    property Container: TCustomSDVTreeContainer
      read getContainer write setContainer;
  end;

(* TCustomSDVTreeContainer *)

  TCustomSDVTreeContainer = class(TSDVActivatedComponent)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FItems: TSDVContainerTreeCollection;
  protected
    (* Protected declarations *)

    (* Accessors declarations *)

    function getItems(): TSDVContainerTreeCollection;

    procedure setItems(const AValue: TSDVContainerTreeCollection);
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; virtual;

    procedure CreateCollection(); virtual;
    procedure DestroyCollection(); virtual;
  public
    (* Friend Protected declarations *)

    procedure NotifyBeforeChangeText
      (const ANode: TSDVContainerTreeNode;
       const AText: string); virtual;
    procedure NotifyAfterChangeText
      (const ANode: TSDVContainerTreeNode); virtual;

    procedure NotifyBeforeChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const ASelected: Boolean); virtual;
    procedure NotifyAfterChangeSelected
      (const ANode: TSDVContainerTreeNode); virtual;

    procedure NotifyBeforeInsert
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); virtual;
    procedure NotifyAfterInsert
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); virtual;

    procedure NotifyBeforeRemove
      (const ANode: TSDVContainerTreeNode); virtual;
    procedure NotifyAfterRemove
      (const ANode: TSDVContainerTreeNode); virtual;

    procedure NotifyBeforeEmpty
      (const ANode: TSDVContainerTreeNode); virtual;
    procedure NotifyAfterEmpty
      (const ANode: TSDVContainerTreeNode); virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    property Items: TSDVContainerTreeCollection
      read getItems write setItems;
  end;

(* TCustomSDVTreeContainer *)

  TSDVTreeContainer = class(TCustomSDVTreeContainer)
  published
    (* published declarations *)

    (* TSDVActivatedComponent *)

    (* TCustomSDVTreeContainer *)
  end;

implementation

(* TSDVContainerTreeNode *)

function TSDVContainerTreeNode.getSelected(): Boolean;
begin
  Result := FSelected;
end;

procedure TSDVContainerTreeNode.setSelected(const AValue: Boolean);
var ACollection: TSDVContainerTreeCollection;
begin
  if (FSelected <> AValue) then
  begin
    ACollection := TSDVContainerTreeCollection(InternalCollection);
    ACollection.RequestChangeSelected(Self, AValue);
  end;
end;

procedure TSDVContainerTreeNode.ConfirmedCopyTo(var ADestNode: TSDVTreeNode);
begin
  inherited ConfirmedCopyTo(ADestNode);

  // this field does not applies to be copied or moved
  Self.FSelected := false;
end;

procedure TSDVContainerTreeNode.ConfirmedMoveTo(var ADestNode: TSDVTreeNode);
begin
  inherited ConfirmedMoveTo(ADestNode);

  // this field does not applies to be copied or moved
  Self.FSelected := false;
end;

procedure TSDVContainerTreeNode.ApplySelectAll
 (var ANode: TSDVTreeNode; const AParam: pointer);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  AContainerNode.Selected := true;
end;

procedure TSDVContainerTreeNode.ApplySelectNone
(var ANode: TSDVTreeNode; const AParam: pointer);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode.Selected := false;
end;

procedure TSDVContainerTreeNode.ApplySelectToggle
(var ANode: TSDVTreeNode; const AParam: pointer);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode.Selected :=
    (not AContainerNode.Selected);
end;

procedure TSDVContainerTreeNode.ApplySelectAs
  (var ANode: TSDVTreeNode; const AParam: pointer);
var AContainerNode: TSDVContainerTreeNode; AValue: Boolean;
begin
  AValue := PBoolean(AParam)^;
  AContainerNode.Selected := AValue;
end;

procedure TSDVContainerTreeNode.ConfirmedChangeSelected
  (const ASelected: Boolean);
begin
  FSelected := ASelected;
end;

procedure TSDVContainerTreeNode.DoCreate();
begin
  inherited DoCreate();
  FSelected := false;
end;

procedure TSDVContainerTreeNode.DoDestroy();
begin
  FSelected := false;
  inherited DoDestroy();
end;

procedure TSDVContainerTreeNode.SelectAll();
begin
  {$ifdef Delphi}
  ForEachForward(ApplySelectAll, nil);
  {$else}
  ForEachForward(@ApplySelectAll, nil);
  {$endif}
end;

procedure TSDVContainerTreeNode.SelectNone();
begin
  {$ifdef Delphi}
  ForEachForward(ApplySelectNone, nil);
  {$else}
  ForEachForward(@ApplySelectNone, nil);
  {$endif}
end;

procedure TSDVContainerTreeNode.SelectToggle();
begin
  {$ifdef Delphi}
  ForEachForward(ApplySelectToggle, nil);
  {$else}
  ForEachForward(@ApplySelectToggle, nil);
  {$endif}
end;

procedure TSDVContainerTreeNode.SelectAs(const AValue: Boolean);
begin
  {$ifdef Delphi}
  ForEachForward(ApplySelectAs, @AValue);
  {$else}
  ForEachForward(@ApplySelectAs, @AValue);
  {$endif}
end;

(* TSDVContainerTreeCollection *)

function TSDVContainerTreeCollection.getContainer(): TCustomSDVTreeContainer;
begin
  Result := FContainer;
end;

procedure TSDVContainerTreeCollection.setContainer
  (const AValue: TCustomSDVTreeContainer);
begin
  FContainer := AValue;
end;

function TSDVContainerTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVContainerTreeNode.Create();
  Result.DoCreate();
end;

procedure TSDVContainerTreeCollection.InternalBeforeChangeText
  (const ANode: TSDVTextTreeNode; const AText: string);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyBeforeChangeText(AContainerNode, AText);
end;

procedure TSDVContainerTreeCollection.InternalAfterChangeText
  (const ANode: TSDVTextTreeNode);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyAfterChangeText(AContainerNode);
end;

procedure TSDVContainerTreeCollection.InternalBeforeChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
begin
  Self.Container.NotifyBeforeChangeSelected(ANode, ASelected);
end;

procedure TSDVContainerTreeCollection.InternalAfterChangeSelected
  (const ANode: TSDVContainerTreeNode);
begin
  Self.Container.NotifyAfterChangeSelected(ANode);
end;

procedure TSDVContainerTreeCollection.InternalConfirmedChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
begin
  ANode.ConfirmedChangeSelected(ASelected);
end;

procedure TSDVContainerTreeCollection.InternalBeforeInsert
  (const AParentNode, ANewNode: TSDVTreeNode; const AIndex: Integer);
var AContainerParentNode, AContainerNewNode: TSDVContainerTreeNode;
begin
  AContainerParentNode := TSDVContainerTreeNode(AParentNode);
  AContainerNewNode    := TSDVContainerTreeNode(ANewNode);
  Self.Container.NotifyBeforeInsert
    (AContainerParentNode, AContainerNewNode, AIndex);
end;

procedure TSDVContainerTreeCollection.InternalAfterInsert
  (const AParentNode, ANewNode: TSDVTreeNode; const AIndex: Integer);
var AContainerParentNode, AContainerNewNode: TSDVContainerTreeNode;
begin
  inherited InternalAfterInsert(AParentNode, ANewNode, AIndex);

  AContainerParentNode := TSDVContainerTreeNode(AParentNode);
  AContainerNewNode    := TSDVContainerTreeNode(ANewNode);
  Self.Container.NotifyAfterInsert
    (AContainerParentNode, AContainerNewNode, AIndex);
end;

procedure TSDVContainerTreeCollection.InternalBeforeRemove
  (const ANode: TSDVTreeNode);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyBeforeRemove(AContainerNode);
end;

procedure TSDVContainerTreeCollection.InternalAfterRemove
  (const ANode: TSDVTreeNode);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyAfterRemove(AContainerNode);
end;

procedure TSDVContainerTreeCollection.InternalBeforeEmpty
  (const ANode: TSDVTreeNode);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyBeforeEmpty(AContainerNode);
end;

procedure TSDVContainerTreeCollection.InternalAfterEmpty
  (const ANode: TSDVTreeNode);
var AContainerNode: TSDVContainerTreeNode;
begin
  AContainerNode := TSDVContainerTreeNode(ANode);
  Self.Container.NotifyAfterEmpty(AContainerNode);
end;

procedure TSDVContainerTreeCollection.RequestChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
begin
  InternalBeforeChangeSelected(ANode, ASelected);
  InternalConfirmedChangeSelected(ANode, ASelected);
  InternalAfterChangeSelected(ANode);
end;

procedure TSDVContainerTreeCollection.DoCreate();
begin
  inherited DoCreate();
end;

procedure TSDVContainerTreeCollection.DoDestroy();
begin
  inherited DoDestroy();
end;

(* TCustomSDVTreeContainer *)

function TCustomSDVTreeContainer.getItems(): TSDVContainerTreeCollection;
begin
  Result := FItems
  // Goal: "Items" property get method .
  // Objetivo: Metodo lectura para propiedad "Items".
end;

procedure TCustomSDVTreeContainer.setItems
  (const AValue: TSDVContainerTreeCollection);
begin
  FItems := AValue;
  // Goal: "Items" property set method .
  // Objetivo: Metodo escritura para propiedad "Items".
end;

function TCustomSDVTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVContainerTreeCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

procedure TCustomSDVTreeContainer.CreateCollection();
begin
  FItems := CreateCollectionByClass();
  FItems.Container := Self;
end;

procedure TCustomSDVTreeContainer.DestroyCollection();
begin
  FItems.DoDestroy();
end;

procedure TCustomSDVTreeContainer.NotifyBeforeChangeText
  (const ANode: TSDVContainerTreeNode; const AText: string);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyAfterChangeText
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyBeforeChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyAfterChangeSelected
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyBeforeInsert
  (const AParentNode, ANode: TSDVContainerTreeNode; const AIndex: Integer);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyAfterInsert
  (const AParentNode, ANode: TSDVContainerTreeNode; const AIndex: Integer);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyBeforeRemove
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyAfterRemove
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyBeforeEmpty
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

procedure TCustomSDVTreeContainer.NotifyAfterEmpty
  (const ANode: TSDVContainerTreeNode);
begin
  Self.DoNothing();
end;

constructor TCustomSDVTreeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateCollection();
end;

destructor TCustomSDVTreeContainer.Destroy();
begin
  DestroyCollection();
  inherited Destroy();
end;

end.

