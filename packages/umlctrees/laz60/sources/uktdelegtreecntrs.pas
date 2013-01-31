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

unit uktdelegtreecntrs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  //uktmsgctrls,
  ukttreenodes,
  ukttreecntrs,
  dummy;

(**
 ** Description:
 ** This unit contains several classes.
 **
 ** The main class is "TCustomSDVMsgTreeContainer",
 ** a non-visual component that contains a hierarchical,
 ** tree structure.
 **
 ** Additionaly, it supports the Subject part,
 ** of the Subject-Observer Pattern,
 ** and its observers maybe visual controls, or not.
 **)

 type

(* TSDVDelegateTreeNode *)

  TCustomSDVDelegateTreeContainer = class;

  TSDVDelegateTreeNode = class(TSDVContainerTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  end;

(* TSDVDelegateTreeCollection *)

  TSDVDelegateTreeCollection = class(TSDVContainerTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Public declarations *)

    procedure DoCreate(); override;
    procedure DoDestroy(); override;
  end;

(* TCustomSDVDelegateTreeContainer *)

  TCustomSDVDelegateTreeContainer = class(TCustomSDVTreeContainer)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FBeforeChangeText: TOnSDVBeforeChangeStringTreeNodeEvent;
    FAfterChangeText:  TOnSDVAfterChangeStringTreeNodeEvent;

    FBeforeChangeSelected: TOnSDVBeforeBooleanTreeNodeEvent;
    FAfterChangeSelected:  TOnSDVAfterBooleanTreeNodeEvent;

    FBeforeInsertNode: TOnSDVInsertAtTreeNodeEvent;
    FAfterInsertNode:  TOnSDVInsertAtTreeNodeEvent;

    FBeforeRemoveNode: TOnSDVTreeContainerNodeEvent;
    FAfterRemoveNode:  TOnSDVTreeContainerNodeEvent;

    FBeforeEmptyNode:  TOnSDVTreeContainerNodeEvent;
    FAfterEmptyNode:   TOnSDVTreeContainerNodeEvent;
  protected
    (* Protected declarations *)

    procedure DelegateBeforeChangeText
      (const ANode: TSDVContainerTreeNode;
       const AValue: string); (* nonvirtual; *)
    procedure DelegateAfterChangeText
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)

    procedure DelegateBeforeChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const AValue: Boolean); (* nonvirtual; *)
    procedure DelegateAfterChangeSelected
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)

    procedure DelegateBeforeInsertNode
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); (* nonvirtual; *)
    procedure DelegateAfterInsertNode
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); (* nonvirtual; *)

    procedure DelegateBeforeRemoveNode
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)
    procedure DelegateAfterRemoveNode
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)

    procedure DelegateBeforeEmptyNode
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)
    procedure DelegateAfterEmptyNode
      (const ANode: TSDVContainerTreeNode); (* nonvirtual; *)
  public
    (* Friend Protected declarations *)

    procedure NotifyBeforeChangeText
      (const ANode: TSDVContainerTreeNode;
       const AText: string); override;
    procedure NotifyAfterChangeText
      (const ANode: TSDVContainerTreeNode); override;

    procedure NotifyBeforeChangeSelected
      (const ANode: TSDVContainerTreeNode;
       const ASelected: Boolean); override;
    procedure NotifyAfterChangeSelected
      (const ANode: TSDVContainerTreeNode); override;

    procedure NotifyBeforeInsert
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); override;
    procedure NotifyAfterInsert
      (const AParentNode, ANode: TSDVContainerTreeNode;
       const AIndex: Integer); override;

    procedure NotifyBeforeRemove
      (const ANode: TSDVContainerTreeNode); override;
    procedure NotifyAfterRemove
      (const ANode: TSDVContainerTreeNode); override;

    procedure NotifyBeforeEmpty
      (const ANode: TSDVContainerTreeNode); override;
    procedure NotifyAfterEmpty
      (const ANode: TSDVContainerTreeNode); override;
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVContainerTreeCollection; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
  public
    (* Public declarations *)

    (* Unpublished declarations *)

    property BeforeChangeText: TOnSDVBeforeChangeStringTreeNodeEvent
      read FBeforeChangeText write FBeforeChangeText;
    property AfterChangeText: TOnSDVAfterChangeStringTreeNodeEvent
      read FAfterChangeText write FAfterChangeText;

    property BeforeChangeSelected: TOnSDVBeforeBooleanTreeNodeEvent
      read FBeforeChangeSelected write FBeforeChangeSelected;
    property AfterChangeSelected: TOnSDVAfterBooleanTreeNodeEvent
      read FAfterChangeSelected write FAfterChangeSelected;

    property BeforeInsertNode: TOnSDVInsertAtTreeNodeEvent
      read FBeforeInsertNode write FBeforeInsertNode;
    property AfterInsertNode: TOnSDVInsertAtTreeNodeEvent
      read FAfterInsertNode write FAfterInsertNode;

    property BeforeEmptyNode: TOnSDVTreeContainerNodeEvent
      read FBeforeEmptyNode write FBeforeEmptyNode;
    property AfterEmptyNode: TOnSDVTreeContainerNodeEvent
      read FAfterEmptyNode write FAfterEmptyNode;

    property BeforeRemoveNode: TOnSDVTreeContainerNodeEvent
      read FBeforeRemoveNode write FBeforeRemoveNode;
    property AfterRemoveNode: TOnSDVTreeContainerNodeEvent
      read FAfterRemoveNode write FAfterRemoveNode;
  end;

  (* TSDVDelegateTreeContainer *)

    TSDVDelegateTreeContainer = class(TCustomSDVDelegateTreeContainer)
    published
      (* Published declarations *)

      (* TCustomSDVDelegateTreeContainer: *)

      property BeforeChangeText;
      property AfterChangeText;

      property BeforeChangeSelected;
      property AfterChangeSelected;

      property BeforeEmptyNode;
      property AfterEmptyNode;

      property BeforeRemoveNode;
      property AfterRemoveNode;
    end;

implementation

procedure TSDVDelegateTreeNode.DoCreate();
begin
  inherited DoCreate();
end;

procedure TSDVDelegateTreeNode.DoDestroy();
begin
  inherited DoDestroy();
end;

(* TSDVDelegateTreeNode *)

(* TSDVDelegateTreeCollection *)

function TSDVDelegateTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVDelegateTreeNode.Create();
  Result.DoCreate();
end;

procedure TSDVDelegateTreeCollection.DoCreate();
begin
  inherited DoCreate();
end;

procedure TSDVDelegateTreeCollection.DoDestroy();
begin
  inherited DoDestroy();
end;

(* TCustomSDVDelegateTreeContainer *)

procedure TCustomSDVDelegateTreeContainer.DelegateBeforeChangeText
  (const ANode: TSDVContainerTreeNode; const AValue: string);
begin
  if (FBeforeChangeText <> nil) then
  begin
    FBeforeChangeText(ANode, AValue);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateAfterChangeText
  (const ANode: TSDVContainerTreeNode);
begin
  if (FAfterChangeText <> nil) then
  begin
    FAfterChangeText(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateBeforeChangeSelected
  (const ANode: TSDVContainerTreeNode; const AValue: Boolean);
begin
  if (FBeforeChangeSelected <> nil) then
  begin
    FBeforeChangeSelected(ANode, AValue);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateAfterChangeSelected
  (const ANode: TSDVContainerTreeNode);
begin
  if (FAfterChangeSelected <> nil) then
  begin
    FAfterChangeSelected(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateBeforeInsertNode
  (const AParentNode, ANode: TSDVContainerTreeNode;
   const AIndex: Integer);
begin
  if (FBeforeInsertNode <> nil) then
  begin
    FBeforeInsertNode(AParentNode, ANode, AIndex);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateAfterInsertNode
  (const AParentNode, ANode: TSDVContainerTreeNode;
   const AIndex: Integer);
begin
  if (FAfterInsertNode <> nil) then
  begin
    FAfterInsertNode(AParentNode, ANode, AIndex);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateBeforeRemoveNode
  (const ANode: TSDVContainerTreeNode);
begin
  if (FBeforeRemoveNode <> nil) then
  begin
    FBeforeRemoveNode(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateAfterRemoveNode
  (const ANode: TSDVContainerTreeNode);
begin
  if (FAfterRemoveNode <> nil) then
  begin
    FAfterRemoveNode(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateBeforeEmptyNode
  (const ANode: TSDVContainerTreeNode);
begin
  if (FBeforeEmptyNode <> nil) then
  begin
    FBeforeEmptyNode(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.DelegateAfterEmptyNode
  (const ANode: TSDVContainerTreeNode);
begin
  if (FAfterEmptyNode <> nil) then
  begin
    FAfterEmptyNode(ANode);
  end;
end;

procedure TCustomSDVDelegateTreeContainer.NotifyBeforeChangeText
  (const ANode: TSDVContainerTreeNode; const AText: string);
begin
  DelegateBeforeChangeText(ANode, AText);
end;

procedure TCustomSDVDelegateTreeContainer.NotifyAfterChangeText
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateAfterChangeText(ANode);
end;

procedure TCustomSDVDelegateTreeContainer.NotifyBeforeChangeSelected
  (const ANode: TSDVContainerTreeNode; const ASelected: Boolean);
begin
  DelegateBeforeChangeSelected(ANode, ASelected);
end;

procedure TCustomSDVDelegateTreeContainer.NotifyAfterChangeSelected
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateAfterChangeSelected(ANode);
end;

procedure TCustomSDVDelegateTreeContainer.NotifyBeforeInsert
  (const AParentNode, ANode: TSDVContainerTreeNode;
   const AIndex: Integer);
begin
  DelegateBeforeInsertNode(AParentNode, ANode, AIndex);
  // Goal: Delegate insert operation.
  // Objetivo: Delegar la operacion de insertar.
end;

procedure TCustomSDVDelegateTreeContainer.NotifyAfterInsert
  (const AParentNode, ANode: TSDVContainerTreeNode;
   const AIndex: Integer);
begin
  DelegateAfterInsertNode(AParentNode, ANode, AIndex);
  // Goal: Delegate insert operation.
  // Objetivo: Delegar la operacion de insertar.
end;

procedure TCustomSDVDelegateTreeContainer.NotifyBeforeRemove
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateBeforeRemoveNode(ANode);
  // Goal: Delegate remove operation.
  // Objetivo: Delegar la operacion de remover.
end;

procedure TCustomSDVDelegateTreeContainer.NotifyAfterRemove
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateAfterRemoveNode(ANode);
  // Goal: Delegate remove operation.
  // Objetivo: Delegar la operacion de remover.
end;

procedure TCustomSDVDelegateTreeContainer.NotifyBeforeEmpty
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateBeforeEmptyNode(ANode);
end;

procedure TCustomSDVDelegateTreeContainer.NotifyAfterEmpty
  (const ANode: TSDVContainerTreeNode);
begin
  DelegateAfterEmptyNode(ANode);
end;

function TCustomSDVDelegateTreeContainer.CreateCollectionByClass(): TSDVContainerTreeCollection;
begin
  Result := TSDVDelegateTreeCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

constructor TCustomSDVDelegateTreeContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // clear event delegate (s)
  FBeforeChangeText := nil;
  FAfterChangeText  := nil;

  FBeforeChangeSelected := nil;
  FAfterChangeSelected  := nil;

  FBeforeInsertNode := nil;
  FAfterInsertNode  := nil;

  FBeforeEmptyNode  := nil;
  FAfterEmptyNode   := nil;

  FBeforeRemoveNode := nil;
  FAfterRemoveNode  := nil;
  // create inheretable (polimorphic) collection
  // crear coleccion heredable (polimorfica)
end;

end.

