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

unit uktmsgpaneltreeviews;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  uktguids,
  uktguidstrs,
  ukttreestates,
  ukttreenodes,
  ukttreecntrs,
  uktmsgtypes,
  uktmsgtreecntrs,
  uktcontroltreenodes,
  uktpaneltreeviews,
  dummy;

(**
 ** This unit contains several classes.
 **
 ** The main class is a descendant ot "TCustomSDVPanelTreeView",
 ** a panel that encapsulates a treeview control,
 ** to add some specific features,
 ** like the "Decorator" Software Design Pattern.
 **
 ** This control, supports the subscriber part,
 ** of the Publisher-Subscriber Design Pattern,
 ** ("Observer" Pattern),
 ** implemented with messages.
 **
 ** There is a non-visual treecontainer, that is the publisher complement.
 **)

type

(* TSDVMsgPanelTreeViewNode *)

  TSDVMsgPanelTreeViewNode = class(TSDVPanelTreeviewNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FContainerTreeNode: TSDVContainerTreeNode;
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    function getContainerTreeNode(): TSDVContainerTreeNode; virtual;

    procedure setContainerTreeNode(const AValue: TSDVContainerTreeNode); virtual;
  public
    (* Protected Friend declarations *)

    procedure InternalExplore(); override;
  public
    (* Public declarations *)

    (* Never published declarations *)

    property ContainerTreeNode: TSDVContainerTreeNode
      read FContainerTreeNode write FContainerTreeNode;
  end;

(* TSDVMsgPanelTreeViewCollection *)

  TSDVMsgPanelTreeViewCollection = class(TSDVPanelTreeviewCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Public declarations *)

    function InsertRoot(): TSDVTreeNode; override;
    procedure DropRoot(); override;
  end;

(* TCustomSDVMsgPanelTreeView *)

  TCustomSDVMsgPanelTreeView =
    class(
      TCustomSDVPanelTreeView,
      ISDVMessageClient,
      ISDVSingleServerMessageClient
      )
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FTreeContainer: TCustomSDVMsgTreeContainer;
  protected
    (* Protected declarations *)

    function getServer(): ISDVMessageServer;
    function getTreeContainer(): TCustomSDVMsgTreeContainer;

    procedure setServer(const AValue: ISDVMessageServer);
    procedure setTreeContainer(const AValue: TCustomSDVMsgTreeContainer);
  protected
    (* Protected declarations *)

    (* Message Handlers declarations *)

    procedure ServerAssignHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure ServerDeassignHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure TreeNodeBeforeInsertHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure TreeNodeAfterInsertHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure TreeNodeBeforeRemoveHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure TreeNodeAfterRemoveHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure TreeNodeBeforeChangeTextHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure TreeNodeAfterChangeTextHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure TreeNodeBeforeChangeSelectedHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure TreeNodeAfterChangeSelectedHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
  protected
    (* Protected declarations *)

    FDestRootNode: TSDVMsgPanelTreeViewNode;

    FMsgHandlerList : TSDVMessageHandlerList;
  protected
    (* Protected declarations *)

    function CreateCollectionByClass(): TSDVPanelTreeviewCollection; override;

    function MatchesNode
      (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
    function NodeOf
      (const ASourceNode: TSDVContainerTreeNode): TSDVMsgPanelTreeViewNode;

    procedure InsertRootNode
      (const AContainerRootNode: TSDVContainerTreeNode); virtual;
    procedure InsertNode
      (const AContainerParentNode: TSDVContainerTreeNode); virtual;

    procedure AssignHandlers(); virtual;

    procedure AnswerMessage
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  public
    (* Public declarations *)

    function AsComponent(): TComponent;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    property TreeContainer: TCustomSDVMsgTreeContainer
      read getTreeContainer write setTreeContainer;

    property Server: ISDVMessageServer
      read getServer write setServer;
  end;

(* TSDVMsgPanelTreeView *)

  TSDVMsgPanelTreeView = class(TCustomSDVMsgPanelTreeView)
  published
    (* published declarations *)

    (* TPanel: *)

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;

    //property ShowGrid;
    //property Text;

    (* TCustomSDVPanel: *)

    property Activated;
    property ReadOnly;

    property OnChange;

    (* TCustomSDVPanelTreeView: *)

    property AutoExpand;
    // "HideSelection" -> "HideFocus"
    property HideFocus;
    property ShowRoot;
    property Images;
    property Items;
    property States;
    property RowFocus;

    property BeforeExplore;
    property BeforeCollapse;
    property BeforeExpand;
    property BeforeEmpty;

    property OnExplore;

    property AfterExplore;
    property AfterCollapse;
    property AfterExpand;
    property AfterEmpty;

    (* TCustomSDVMsgPanelTreeView: *)

    property TreeContainer;
    property Server;
  end;

implementation

(* TSDVMsgPanelTreeViewNode *)

function TSDVMsgPanelTreeViewNode.getContainerTreeNode(): TSDVContainerTreeNode;
begin
  Result := FContainerTreeNode;
end;

procedure TSDVMsgPanelTreeViewNode.setContainerTreeNode
  (const AValue: TSDVContainerTreeNode);
begin
  FContainerTreeNode := AValue;
end;

procedure TSDVMsgPanelTreeViewNode.InternalExplore();
var AContainerParentNode, AContainerChildNode: TSDVContainerTreeNode;
    APanelChildNode: TSDVMsgPanelTreeViewNode;
    AText: string;
    EachIndex, LastIndex: Integer;
begin
  // in this step, the control node, must not have subitems

  AContainerParentNode := Self.ContainerTreeNode;

  LastIndex := (AContainerParentNode.Count() - 1);
  for EachIndex := 0 to LastIndex do
  begin
    AContainerChildNode :=
      TSDVContainerTreeNode(AContainerParentNode.List().Items[EachIndex]);

    APanelChildNode :=
      (Self.Insert() as TSDVMsgPanelTreeViewNode);
    Application.ProcessMessages();

    AText := AContainerChildNode.Text;
    APanelChildNode.Text := AText;

    APanelChildNode.ContainerTreeNode := AContainerChildNode;
  end;
end;

(* TSDVMsgPanelTreeViewCollection *)

function TSDVMsgPanelTreeViewCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVMsgPanelTreeViewNode.Create();
  Result.DoCreate();
end;

function TSDVMsgPanelTreeViewCollection.InsertRoot(): TSDVTreeNode;
begin
  Result := inherited InsertRoot();
  Application.ProcessMessages();
  // Goal: Inserts the root item to the collection.
  // Objetivo: Inserta el elemento raiz en la coleccion.
end;

procedure TSDVMsgPanelTreeViewCollection.DropRoot();
begin
  inherited DropRoot();
  Application.ProcessMessages();
end;

(* TCustomSDVMsgPanelTreeView *)

function TCustomSDVMsgPanelTreeView.getServer(): ISDVMessageServer;
begin
  Result := ISDVMessageServer(FTreeContainer);
  // Goal: "Server" property set method.
  // Objetivo: Metodo lectura propiedad "Server".
end;

function TCustomSDVMsgPanelTreeView.getTreeContainer(): TCustomSDVMsgTreeContainer;
begin
  Result := FTreeContainer
  // Goal: "TreeContainer" property get method .
  // Objetivo: Metodo lectura para propiedad "TreeContainer".
end;

procedure TCustomSDVMsgPanelTreeView.setServer(const AValue: ISDVMessageServer);
begin
  if (AValue = nil) then
  begin
    FTreeContainer := nil;
  end else
  begin
    FTreeContainer := TCustomSDVMsgTreeContainer(AValue.AsComponent());
  end;
  // Goal: "Server" property set method.
  // Objetivo: Metodo escritura propiedad "Server".
end;

procedure TCustomSDVMsgPanelTreeView.setTreeContainer
  (const AValue: TCustomSDVMsgTreeContainer);
begin
  FTreeContainer := AValue;
  // Goal: "TreeContainer" property set method .
  // Objetivo: Metodo escritura para propiedad "TreeContainer"
end;

procedure TCustomSDVMsgPanelTreeView.ServerAssignHandler
  (const AMsgRec: TSDVMessageParamsRecord);
var ASourceRootNode: TSDVContainerTreeNode;
  AText: string;
begin
  // reassign data server
  FTreeContainer := TCustomSDVMsgTreeContainer(AMsgRec.Sender);

  // clear previous items, if any
  Self.Items.DropRoot();

  // match root nodes
  ASourceRootNode := (FTreeContainer.Items.Root() as TSDVContainerTreeNode);
  if (ASourceRootNode <> nil) then
  begin
    FDestRootNode := (Self.Items.InsertRoot() as TSDVMsgPanelTreeViewNode);

    AText := ASourceRootNode.Text;
    FDestRootNode.Text := AText;

    FDestRootNode.ContainerTreeNode := ASourceRootNode;
  end;
end;

procedure TCustomSDVMsgPanelTreeView.ServerDeassignHandler
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  // clear previous items, if any
  Self.Items.DropRoot();

  // reassign data server
  FTreeContainer := nil;
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeBeforeInsertHandler
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  Self.DoNothing();
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeAfterInsertHandler
  (const AMsgRec: TSDVMessageParamsRecord);
var AContainerParentNode, AContainerNewNode: TSDVContainerTreeNode;
begin
  AContainerParentNode := (AMsgRec.Sender as TSDVContainerTreeNode);
  AContainerNewNode    := TSDVContainerTreeNode(AMsgRec.Param);
  if (AContainerNewNode <> nil) then
  begin
    if (AContainerNewNode.IsRoot()) then
    begin
      InsertRootNode(AContainerNewNode);
    end else
    begin
      InsertNode(AContainerParentNode);
    end;
  end;
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeBeforeRemoveHandler
  (const AMsgRec: TSDVMessageParamsRecord);
var AContainerNode: TSDVContainerTreeNode;
    ATreeviewNode:  TSDVMsgPanelTreeViewNode;
begin
  AContainerNode := (AMsgRec.Sender as TSDVContainerTreeNode);

  // find the node from treeview that references the given parent node,
  // from the container
  ATreeviewNode := NodeOf(AContainerNode);
  if (ATreeviewNode <> nil) then
  begin
    ATreeviewNode.Remove();
  end;
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeAfterRemoveHandler
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  Self.DoNothing();
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeBeforeChangeTextHandler
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  Self.DoNothing();
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeAfterChangeTextHandler
  (const AMsgRec: TSDVMessageParamsRecord);
var AContainerNode: TSDVContainerTreeNode;
    ATreeviewNode:  TSDVMsgPanelTreeViewNode;
var AText: string;
begin
  AContainerNode := (AMsgRec.Sender as TSDVContainerTreeNode);

  // find the node from treeview that references the given parent node,
  // from the container
  ATreeviewNode := NodeOf(AContainerNode);
  if (ATreeviewNode <> nil) then
  begin
    AText := PString(AMsgRec.Param)^;
    ATreeviewNode.Text := AText;
  end;
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeBeforeChangeSelectedHandler
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  Self.DoNothing();
end;

procedure TCustomSDVMsgPanelTreeView.TreeNodeAfterChangeSelectedHandler
  (const AMsgRec: TSDVMessageParamsRecord);
var ANode: TSDVContainerTreeNode;
    APanelNode: TSDVMsgPanelTreeViewNode;
var ASelected: Boolean;
begin
  ANode := (AMsgRec.Sender as TSDVContainerTreeNode);

  // find the node from treeview that references the given parent node,
  // from the container
  APanelNode := NodeOf(ANode);
  if (APanelNode <> nil) then
  begin
    ASelected := PBoolean(AMsgRec.Param)^;
    APanelNode.Selected := ASelected;
  end;
end;

function TCustomSDVMsgPanelTreeView.CreateCollectionByClass(): TSDVPanelTreeviewCollection;
begin
  Result := TSDVMsgPanelTreeViewCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

function TCustomSDVMsgPanelTreeView.MatchesNode
  (const ANode: TSDVTreeNode; const AParam: pointer): Boolean;
var PanelNode: TSDVMsgPanelTreeViewNode;
    ThisNode: TSDVContainerTreeNode;
begin
  PanelNode := (ANode as TSDVMsgPanelTreeViewNode);
  ThisNode  := TSDVContainerTreeNode(AParam);
  Result    := (PanelNode.ContainerTreeNode = ThisNode);
end;

function TCustomSDVMsgPanelTreeView.NodeOf
  (const ASourceNode: TSDVContainerTreeNode): TSDVMsgPanelTreeViewNode;
var ARootNode: TSDVMsgPanelTreeViewNode;
begin
  Result := nil;

  ARootNode := TSDVMsgPanelTreeViewNode(Self.Items.Root());
  if (ARootNode <> nil) then
  begin
    Result :=
      TSDVMsgPanelTreeViewNode(ARootNode.FirstThat
        (@MatchesNode, ASourceNode));
  end;
end;

procedure TCustomSDVMsgPanelTreeView.InsertRootNode
  (const AContainerRootNode: TSDVContainerTreeNode);
var ATreeviewRootNode: TSDVMsgPanelTreeViewNode;
    AText: string;
begin
  // remove previous root if any
  Self.Items.DropRoot();

  // match root nodes
  ATreeviewRootNode := (Self.Items.InsertRoot() as TSDVMsgPanelTreeViewNode);

  AText := AContainerRootNode.Text;
  ATreeviewRootNode.Text := AText;

  ATreeviewRootNode.ContainerTreeNode := AContainerRootNode;
end;

procedure TCustomSDVMsgPanelTreeView.InsertNode
  (const AContainerParentNode: TSDVContainerTreeNode);
var ATreeviewParentNode: TSDVMsgPanelTreeViewNode;
begin
  // find the node from treeview that references the given parent node,
  // from the container
  ATreeviewParentNode := NodeOf(AContainerParentNode);
  if (ATreeviewParentNode <> nil) then
  begin
    // remove all subitems
    ATreeviewParentNode.Empty();

    // refresh subitems
    //ATreeviewParentNode.InternalExplore();

    //ATreeviewParentNode.Hide();

    ATreeviewParentNode.UpdateExpand();
  end;
end;

procedure TCustomSDVMsgPanelTreeView.AssignHandlers();
var AMsgID: TGUID;
begin
  uktguidstrs.DoubleStrToGUID
    (msgServerAssign, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}ServerAssignHandler);

  uktguidstrs.DoubleStrToGUID
    (msgServerDeassign, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}ServerDeassignHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeInsert, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeBeforeInsertHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterInsert, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeAfterInsertHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeRemove, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeBeforeRemoveHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterRemove, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeAfterRemoveHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeChangeText, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeBeforeChangeTextHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeText, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeAfterChangeTextHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeBeforeChangeSelected, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeBeforeChangeSelectedHandler);

  uktguidstrs.DoubleStrToGUID
    (msgTreeNodeAfterChangeSelected, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}TreeNodeAfterChangeSelectedHandler);
end;

procedure TCustomSDVMsgPanelTreeView.AnswerMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var AHandler: TSDVMsgEventHandler;
begin
  AHandler := FMsgHandlerList.HandlerOf(AMsgRec.Message);
  if (AHandler <> nil) then
  begin
    AHandler(AMsgRec);
  end;
end;

procedure TCustomSDVMsgPanelTreeView.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (FTreeContainer <> nil) then
  begin
    if ((Operation = opRemove) and (AComponent = FTreeContainer)) then
    begin
      FTreeContainer := nil;
    end;
  end;
  // Goal: To detect & update when associated components,
  // have been removed.

  // Objetivo: Detectar y actualizar cuando,
  // los componentes asociados se han removido.
end;

function TCustomSDVMsgPanelTreeView.AsComponent(): TComponent;
begin
  Result := Self;
end;

constructor TCustomSDVMsgPanelTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMsgHandlerList := TSDVMessageHandlerList.Create();

  AssignHandlers();
end;

destructor TCustomSDVMsgPanelTreeView.Destroy();
begin
  FMsgHandlerList.Free();
  FMsgHandlerList := nil;
  inherited Destroy();
end;

end.

