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

unit uktpaneltreeviews;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  {$IFDEF Windows}
  COMCtrls,
  {$ENDIF}
  ImgList,
  Graphics,
  Forms,
  Dialogs,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  ukttreestates,
  ukttreenodes,
  uktcontroltreenodes,
  uktpanels,
  uktnormpanels,
  //uktpublictreeviews,
  uktstatetreeviews,
  dummy;

(**
 ** This unit contains several classes.
 **
 ** The main class is "TCustomSDVPanelTreeView",
 ** a panel that encapsulates a treeview control,
 ** to add some specific features,
 ** like the "Decorator" Software Design Pattern.
 **
 ** However, this is a temporal fix,
 ** the goal at long term, is to built,
 ** a new treeview control class from scratch.
 **)

type

(* TInternalTreeNode *)

  TInternalTreeNode = COMCtrls.TTreenode;

(* TSDVPanelTreeViewNode *)

  TCustomSDVPanelTreeView = class;

  TSDVPanelTreeviewNode = class(TSDVControlTreeNode)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FInternalTreeview: TCustomSDVPanelTreeView;
    FInternalTreenode: TInternalTreeNode;
  protected
    (* Protected declarations *)

    (* DO NOT become published declarations *)

    procedure setText(const AValue: string); override;
    procedure setFocused(const AValue: Boolean); override;
    procedure setSelected(const AValue: Boolean); override;
    procedure setState(const AValue: TTreeStates); override;
  protected
    (* Protected declarations *)

    procedure ConfirmedChangeText(const AText: string);override;
  public
    (* Protected Friend declarations *)

    procedure InternalTreenodeDeassign();
    procedure InternalTreenodeAssign
      (const WantShowNode: Boolean);

    procedure ConfirmedInsertRoot(); override;
    procedure ConfirmedInsert(var ANode: TSDVTreeNode); override;
    procedure ConfirmedInsertAt
      (const AIndex: Integer; var ANode: TSDVTreeNode); override;

    procedure ConfirmedRemove(); override;

    procedure UpdateText();
    procedure UpdateImageIndex(); override;
    procedure UpdateStateImageIndex(); virtual;
  public
    (* Protected Friend declarations *)

    procedure InternalExplore(); override;
    procedure InternalCollapse(); override;
    procedure InternalExpand(); override;
    procedure InternalEmpty(); override;
    procedure InternalHide(); override;
  public
    (* Protected Friend declarations *)

    (* Never published declarations *)

    property InternalTreenode: TInternalTreeNode
      read FInternalTreenode write FInternalTreenode;
    property InternalTreeview: TCustomSDVPanelTreeView
      read FInternalTreeview write FInternalTreeview;
  public
    (* Public declarations *)
  end;

  (* TSDVPanelTreeviewCollection *)

  TSDVPanelTreeviewCollection = class(TSDVControlTreeCollection)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    function CreateNodeByClass(): TSDVTreeNode; override;
  public
    (* Protected Friend declarations *)

    FTreeview:  TCustomSDVPanelTreeView;
  public
    (* Protected Friend declarations *)

    property Treeview: TCustomSDVPanelTreeView
      read FTreeview write FTreeview;
  public
    (* Public declarations *)

    function InsertRoot(): TSDVTreeNode; override;
    procedure DropRoot(); override;
  end;

  (* TCustomSDVPanelTreeView *)

  TCustomSDVPanelTreeView = class(TCustomSDVNormalizedPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FAutoExpand:    Boolean;
    FEnabled:       Boolean;
    FHideFocus:     Boolean;
    FShowRoot:      Boolean;
    FFocusedNode:   TSDVPanelTreeviewNode;

    FItems:            TSDVPanelTreeviewCollection;
    FInternalTreeView: TCustomSDVStateTreeView;

    FOnClick:          TNotifyEvent;
    FOnFocusedChanged: TNotifyEvent;

    IgnoreClick:    Boolean;
  protected
    (* Protected declarations *)

    function getAutoExpand(): Boolean;
    function getHideFocus(): Boolean;
    function getShowRoot(): Boolean;
    function getImages(): TCustomImageList;
    function getStates(): TCustomImageList;
    function getFocusedNode(): TSDVPanelTreeviewNode;
    function getEnabled(): Boolean; override;
    function getRowFocus(): Boolean;

    procedure setAutoExpand(const AValue: Boolean);
    procedure setHideFocus(const AValue: Boolean);
    procedure setShowRoot(const AValue: Boolean);
    procedure setImages(const AValue: TCustomImageList);
    procedure setStates(const AValue: TCustomImageList);
    procedure setFocusedNode(const AValue: TSDVPanelTreeviewNode);
    procedure setEnabled(const AValue: Boolean); override;
    procedure setRowFocus(const AValue: Boolean);
  protected
    (* Protected declarations *)

    (* Never published declarations *)

    property InternalTreeView: TCustomSDVStateTreeView
      read FInternalTreeView write FInternalTreeView;
  protected
    (* Protected declarations *)

    function getBeforeExplore(): TOnSDVControlTreeNodeEvent;
    function getBeforeCollapse(): TOnSDVControlTreeNodeEvent;
    function getBeforeExpand(): TOnSDVControlTreeNodeEvent;
    function getBeforeEmpty(): TOnSDVControlTreeNodeEvent;
    function getOnExplore(): TOnSDVControlTreeNodeEvent;
    function getAfterExplore(): TOnSDVControlTreeNodeEvent;
    function getAfterCollapse(): TOnSDVControlTreeNodeEvent;
    function getAfterExpand(): TOnSDVControlTreeNodeEvent;
    function getAfterEmpty(): TOnSDVControlTreeNodeEvent;

    procedure setBeforeExplore(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setBeforeCollapse(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setBeforeExpand(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setBeforeEmpty(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setOnExplore(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setAfterExplore(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setAfterCollapse(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setAfterExpand(const AValue: TOnSDVControlTreeNodeEvent);
    procedure setAfterEmpty(const AValue: TOnSDVControlTreeNodeEvent);
  protected
    (* Protected declarations *)

    procedure OnTreeviewChangingDispatcher
      (Sender: TObject; Node: TInternalTreeNode; var AllowChange: Boolean);
    procedure OnTreeviewExpandingDispatcher
      (Sender: TObject; Node: TInternalTreeNode; var AllowExpansion: Boolean);
    procedure OnTreeviewCollapsingDispatcher
      (Sender: TObject; Node: TInternalTreeNode; var AllowCollapse: Boolean);
    procedure OnTreeviewCollapsedDispatcher
      (Sender: TObject; Node: TInternalTreeNode);
    procedure OnTreeviewExpandedDispatcher
      (Sender: TObject; Node: TInternalTreeNode);
    procedure OnTreeviewClickDispatcher
      (Sender: TObject);
    procedure OnTreeviewClickStateDispatcher
      (Sender: TObject; Node: TInternalTreeNode);
    procedure OnTreeviewSelectionChangedDispatcher
      (Sender: TObject);
  protected
    (* Protected declarations *)

    function CreateTreeViewByClass(): TCustomSDVStateTreeView; virtual;

    procedure AssignInternalTreeView();

    function CreateCollectionByClass(): TSDVPanelTreeviewCollection; virtual;

    procedure FocusOffItem
      (var ANode: TSDVTreeNode; const AParam: pointer);
    procedure EnableItem
      (var ANode: TSDVTreeNode; const AParam: pointer);

    procedure ConfirmFocused(const ANode: TSDVPanelTreeviewNode);

    procedure RefreshRootItem(); virtual;

    function CreateCollection(): TSDVPanelTreeviewCollection; virtual;
    procedure DestroyCollection(); virtual;

    procedure CreateImages(); virtual;
    procedure DestroyImages(); virtual;

    procedure DelegateOnClick();
    procedure DelegateOnFocusedChanged();
  public
    (* Protected Friend declarations *)

    procedure TreeviewRootAssign();
    procedure TreeviewRootDeassign();

    function InsertInternalTreeNode
      (const AParent: TInternalTreeNode): TInternalTreeNode;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    procedure ActivateFirst(); override;

    (* Never published declarations *)

    property Items: TSDVPanelTreeviewCollection
      read FItems write FItems;
    property FocusedNode: TSDVPanelTreeviewNode
      read getFocusedNode write setFocusedNode;

    (* Unpublished declarations *)

    property AutoExpand: Boolean
      read getAutoExpand write setAutoExpand;
    property HideFocus: Boolean
      read getHideFocus write setHideFocus;
    property ShowRoot: Boolean
      read getShowRoot write setShowRoot;
    property Images: TCustomImageList
      read getImages write setImages;
    property States: TCustomImageList
      read getStates write setStates;
    property RowFocus: Boolean
      read getRowFocus write setRowFocus;
    property UseDockManager default true;

    property BeforeExplore: TOnSDVControlTreeNodeEvent
      read getBeforeExplore write setBeforeExplore;
    property BeforeCollapse: TOnSDVControlTreeNodeEvent
      read getBeforeCollapse write setBeforeCollapse;
    property BeforeExpand: TOnSDVControlTreeNodeEvent
      read getBeforeExpand write setBeforeExpand;
    property BeforeEmpty: TOnSDVControlTreeNodeEvent
      read getBeforeEmpty write setBeforeEmpty;

    property OnExplore: TOnSDVControlTreeNodeEvent
      read getOnExplore write setOnExplore;

    property AfterExplore: TOnSDVControlTreeNodeEvent
      read getAfterExplore write setAfterExplore;
    property AfterCollapse: TOnSDVControlTreeNodeEvent
      read getAfterCollapse write setAfterCollapse;
    property AfterExpand: TOnSDVControlTreeNodeEvent
      read getAfterExpand write setAfterExpand;
    property AfterEmpty: TOnSDVControlTreeNodeEvent
      read getAfterEmpty write setAfterEmpty;

    property OnClick: TNotifyEvent
      read FOnClick write FOnClick;
    property OnFocusedChanged: TNotifyEvent
      read FOnFocusedChanged write FOnFocusedChanged;
  end;

(* TSDVPanelTreeView *)

  TSDVPanelTreeView = class(TCustomSDVPanelTreeView)
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

    property OnFocusedChanged;
  end;

  function SharedButtonImages(): TImageList;

implementation

{$IFDEF DELPHI}
{$R 'sdvpaneltreeviewsres.dcr'}
{$ENDIF}

// singleton variable to store shared images for buttons,
// of treeview control.
// Previous version created a variable, per, object.
// that was not memory friendly.
var FSharedButtonImages: TImageList;

function SharedButtonImages(): TImageList;
begin
  Result := FSharedButtonImages;
end;

(* TSDVPanelTreeviewNode *)

procedure TSDVPanelTreeviewNode.setText(const AValue: string);
begin
  if (FText <> AValue) then
  begin
    FText := AValue;
    if (InternalTreenode <> nil) then
    begin
      ConfirmedChangeText(AValue);
    end;
  end;
end;

procedure TSDVPanelTreeviewNode.setFocused(const AValue: Boolean);
begin
  if (FFocused <> AValue) then
  begin
    FFocused := AValue;
    if (AValue) then
    begin
      InternalTreeview.ConfirmFocused(Self);
    end;
  end;
end;

procedure TSDVPanelTreeviewNode.setSelected(const AValue: Boolean);
begin
  inherited setSelected(AValue);
  ConfirmedChangeText(Self.FText);
end;

procedure TSDVPanelTreeviewNode.setState(const AValue: TTreeStates);
begin
  if (FInternalState <> AValue) then
  begin
    FInternalState := AValue;
    if (InternalTreenode <> nil) then
    begin
      case (FInternalState) of
        ukttreestates.tsEmpty:     Self.Empty();
        ukttreestates.tsCollapsed: Self.Collapse();
        ukttreestates.tsExpanded:  Self.Expand();
        ukttreestates.tsUnknown:   Self.Explore();

        // "exploring" cannot be assigned directly by user
        ukttreestates.tsExploring: Self.DoNothing();
        else                       Self.DoNothing();
      end;
    end;
    Application.ProcessMessages();
  end;
end;

procedure TSDVPanelTreeviewNode.ConfirmedChangeText(const AText: string);
var NewText: string;
begin
  InternalTreenode.Text := '';

  if (Self.Selected) then
  begin
    NewText := '[X] ' + AText;
  end else
  begin
    NewText := '[ ] ' + AText;
  end;

  InternalTreenode.Text := NewText;
  Application.ProcessMessages();
end;

procedure TSDVPanelTreeviewNode.InternalTreenodeDeassign();
var CanRemove: Boolean;
    ParentNode: TSDVPanelTreeviewNode;
    InternalChildNode: TInternalTreeNode;
begin
  CanRemove :=
    ( (1 = 1)
      and (Self.InternalTreeview <> nil)
      and (Self.InternalTreenode <> nil)
    );

  if (CanRemove) then
  begin
    // obtain reference to node
    InternalChildNode := Self.InternalTreenode;
    // remove it
    //InternalChildNode.Delete();
    InternalChildNode.Free();

    // update link
    Self.InternalTreenode := nil;
  end;
end;

procedure TSDVPanelTreeviewNode.InternalTreenodeAssign
  (const WantShowNode: Boolean);
var CanShow, IsActive: Boolean;
    ParentNode: TSDVPanelTreeviewNode;
    InternalParentNode, InternalChildNode: TInternalTreeNode;
    ACollection: TSDVPanelTreeviewCollection;
begin
  // note: "parent" should be assigned previously
  ACollection := (InternalCollection as TSDVPanelTreeviewCollection);

  Self.InternalTreeview := ACollection.Treeview;

  // assign link to internal native treeview
  // asignar liga a vistaarbol interno nativo

  // clear reference to internal node
  Self.InternalTreenode := nil;

  // obtain external parent node
  ParentNode  := (Self.InternalParent as TSDVPanelTreeviewNode);

  //IsActive := ACollection.Activated;
  IsActive := FInternalTreeview.Activated;

  CanShow  := (WantShowNode and IsActive);
  if (CanShow) then
  begin
    // obtain internal parent node from external parent node
    InternalParentNode := nil;
    if (ParentNode <> nil) then
    begin
      InternalParentNode := ParentNode.InternalTreenode;
    end;

    // generate new internal node, and add to internal parent node
    InternalChildNode :=
      InternalTreeview.InsertInternalTreeNode(InternalParentNode);
    // its a visual control, let's wait for process,
    // otherwise there is a bug
    Application.ProcessMessages();

    // link new internal node to external node
    InternalChildNode.Data := Self;
    // link new external node to internal node
    Self.InternalTreenode  := InternalChildNode;

    // copy caption
    InternalChildNode.Text := Self.Text;
    Application.ProcessMessages();

    // confirm change
    Self.UpdateStateImageIndex();
  end;
end;

procedure TSDVPanelTreeviewNode.ConfirmedInsertRoot();
begin
  inherited ConfirmedInsertRoot();
  InternalTreenodeAssign(true);
end;

procedure TSDVPanelTreeviewNode.ConfirmedInsert(var ANode: TSDVTreeNode);
var APanelNode: TSDVPanelTreeviewNode;
begin
  inherited ConfirmedInsert((* var *) ANode);
  APanelNode := (ANode as TSDVPanelTreeviewNode);
  APanelNode.InternalTreenodeAssign(true);
end;

procedure TSDVPanelTreeviewNode.ConfirmedInsertAt
  (const AIndex: Integer; var ANode: TSDVTreeNode);
var APanelNode: TSDVPanelTreeviewNode;
begin
  inherited ConfirmedInsertAt((* const *) AIndex, (* var *) ANode);
  APanelNode := (ANode as TSDVPanelTreeviewNode);
  APanelNode.InternalTreenodeAssign(true);
end;

procedure TSDVPanelTreeviewNode.ConfirmedRemove();
begin
  InternalTreenodeDeassign();
  inherited ConfirmedRemove();
end;

procedure TSDVPanelTreeviewNode.UpdateText();
var S: string;
begin
  if (InternalTreenode <> nil) then
  begin
    S := Self.Text;
    Self.Text := '';
    Self.Text := S;
  end;
end;

procedure TSDVPanelTreeviewNode.UpdateImageIndex();
begin
  if (InternalTreenode <> nil) then
  begin
    if (FEnabled) then
    begin
      InternalTreenode.ImageIndex    := FImageIndex;
      InternalTreenode.SelectedIndex := FFocusedIndex;
    end else
    begin
      InternalTreenode.ImageIndex    := FDisabledIndex;
      InternalTreenode.SelectedIndex := FKeepIndex;
    end;
  end;
end;

procedure TSDVPanelTreeviewNode.UpdateStateImageIndex();
begin
  if (InternalTreenode <> nil) then
  begin
    InternalTreenode.StateIndex := StateToIndex(FInternalState);
    Application.ProcessMessages();
  end;
end;

procedure TSDVPanelTreeviewNode.InternalExplore();
begin
  if (HasItems()) then
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsExpanded;

    // update treeview visually
    if (InternalTreenode <> nil) then
    begin
      InternalTreenode.Expand(false);
      Application.ProcessMessages();
    end;
  end else
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsEmpty;

    // update treeview visually
    if (InternalTreenode <> nil) then
    begin
      InternalTreenode.Collapse(false);
      Application.ProcessMessages();
    end;
  end;
  // change image
  UpdateStateImageIndex();
end;

procedure TSDVPanelTreeviewNode.InternalCollapse();
begin
  if (HasItems()) then
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsCollapsed;
  end else
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsEmpty;
  end;

  // in any case, always collapse node
  if (InternalTreenode <> nil) then
  begin
    InternalTreenode.Collapse(false);
    Application.ProcessMessages();
  end;

  // change image
  UpdateStateImageIndex();
end;

procedure TSDVPanelTreeviewNode.InternalExpand();
begin
  if (HasItems()) then
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsExpanded;

    // update treeview visually
    if (InternalTreenode <> nil) then
    begin
      InternalTreenode.Expand(false);
      Application.ProcessMessages();
    end;
  end else
  begin
    // change state marker value, without doing anything else
    FInternalState := ukttreestates.tsEmpty;

    // update treeview visually
    if (InternalTreenode <> nil) then
    begin
      InternalTreenode.Collapse(false);
      Application.ProcessMessages();
    end;
  end;

  // change image
  UpdateStateImageIndex();
end;

procedure TSDVPanelTreeviewNode.InternalEmpty();

  procedure DeleteChildren();
  begin
    InternalTreenode.DeleteChildren();
    Application.ProcessMessages();
  end;

begin
  if (InternalTreeview.Items.InternalRoot = Self) then
  begin
    if (InternalTreeview.ShowRoot) then
    begin
      DeleteChildren()
    end else
    begin
      InternalTreeview.InternalTreeView.Items.Clear();
      Application.ProcessMessages();
    end;
  end else
  begin
    DeleteChildren();
  end;

  // remove all nodes from list
  InternalList.Empty();

  // change state marker value, without doing anything else
  FInternalState := ukttreestates.tsEmpty;

  // update treeview visually
  UpdateStateImageIndex();
end;

procedure TSDVPanelTreeviewNode.InternalHide();
begin
  // removes visual subitems,
  // NOT data subitems
  InternalTreenode.DeleteChildren();

  // change state marker value, without doing anything else
  FInternalState := ukttreestates.tsUnknown;

  // update treeview visually
  UpdateStateImageIndex();
  // Goal: Makes subitems not visible,
  // NOT the node itself.
end;

(* TSDVPanelTreeviewCollection *)

function TSDVPanelTreeviewCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVPanelTreeviewNode.Create();
  Result.DoCreate();
end;

function TSDVPanelTreeviewCollection.InsertRoot(): TSDVTreeNode;
begin
  Result := nil;
  if (not FBeenIterated) then
  begin
    // check previous existing root node
    Self.DestroyRoot();

    Self.CreateRoot();
    Self.Treeview.TreeviewRootAssign();

    Result := InternalRoot;
  end;
  // Goal: Inserts the root item to the collection.
  // Objetivo: Inserta el elemento raiz en la coleccion.
end;

procedure TSDVPanelTreeviewCollection.DropRoot();
begin
  if (not FBeenIterated) then
  begin
    // check previous existing root node
    Self.DestroyRoot();

    Self.Treeview.TreeviewRootDeassign();
  end;
end;

(* TCustomSDVPanelTreeView *)

function TCustomSDVPanelTreeView.getAutoExpand(): Boolean;
begin
  Result := FAutoExpand;
end;

function TCustomSDVPanelTreeView.getHideFocus(): Boolean;
begin
  Result := FHideFocus;
end;

function TCustomSDVPanelTreeView.getShowRoot(): Boolean;
begin
  Result := FShowRoot;
end;

function TCustomSDVPanelTreeView.getImages(): TCustomImageList;
begin
  if (FInternalTreeView <> nil)
    then Result := FInternalTreeView.Images
    else Result := nil;
end;

function TCustomSDVPanelTreeView.getStates(): TCustomImageList;
begin
  if (FInternalTreeView <> nil) then
  begin
    Result := FInternalTreeView.StateImages
  end else
  begin
    Result := nil;
  end;
end;

function TCustomSDVPanelTreeView.getFocusedNode(): TSDVPanelTreeviewNode;
begin
  Result := FFocusedNode;
end;

function TCustomSDVPanelTreeView.getEnabled(): Boolean;
begin
  Result := FEnabled;
end;

function TCustomSDVPanelTreeView.getRowFocus(): Boolean;
begin
  if (FInternalTreeView <> nil) then
  begin
    Result := FInternalTreeView.RowSelect
  end else
  begin
    Result := false;
  end;
end;

procedure TCustomSDVPanelTreeView.setAutoExpand(const AValue: Boolean);
begin
  FAutoExpand := AValue;
end;

procedure TCustomSDVPanelTreeView.setHideFocus(const AValue: Boolean);
begin
  FHideFocus := AValue;

  if (Self.InternalTreeView <> nil) then
  begin
    Self.InternalTreeView.HideSelection := AValue;
  end;
end;

procedure TCustomSDVPanelTreeView.setShowRoot(const AValue: Boolean);
begin
  FShowRoot := AValue;

  (*
  if (FShowRoot) then
  begin
    RefreshRootItem(Items.InternalRoot);
  end;
  *)
end;

procedure TCustomSDVPanelTreeView.setImages(const AValue: TCustomImageList);
begin
  if (FInternalTreeView <> nil) then
  begin
    FInternalTreeView.Images := AValue;
  end;
end;

procedure TCustomSDVPanelTreeView.setStates(const AValue: TCustomImageList);
begin
  if (FInternalTreeView <> nil) then
  begin
    FInternalTreeView.StateImages := AValue;
  end;
end;

procedure TCustomSDVPanelTreeView.setFocusedNode
  (const AValue: TSDVPanelTreeviewNode);
var CanSelect: Boolean;
begin
  //Items.InternalList.ForEach(@FocusOffItem, nil);
  // deseleccionar todos los nodos
  // unselect all nodes

  FFocusedNode := AValue;

  CanSelect :=
    ( (1 = 1)
      and (FInternalTreeView <> nil)
      and (AValue <> nil)
      and (AValue.InternalTreenode <> nil)
    );
  if (CanSelect) then
  begin
    ConfirmFocused(AValue);
  end else
  begin
    FInternalTreeView.Selected := nil;
  end;
end;

procedure TCustomSDVPanelTreeView.setEnabled(const AValue: Boolean);
begin
  if (FEnabled <> AValue) then
  begin
    FEnabled := AValue;
    Items.Root().ForEachForward(@EnableItem, nil);
  end;
end;

procedure TCustomSDVPanelTreeView.setRowFocus(const AValue: Boolean);
begin
  if (FInternalTreeView <> nil) then
  begin
    FInternalTreeView.RowSelect := AValue;
  end;
end;

function TCustomSDVPanelTreeView.getBeforeExplore(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.BeforeExplore;
end;

function TCustomSDVPanelTreeView.getBeforeCollapse(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.BeforeCollapse;
end;

function TCustomSDVPanelTreeView.getBeforeExpand(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.BeforeExpand;
end;

function TCustomSDVPanelTreeView.getBeforeEmpty(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.BeforeEmpty;
end;

function TCustomSDVPanelTreeView.getOnExplore(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.OnExplore;
end;

function TCustomSDVPanelTreeView.getAfterExplore(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.AfterExplore;
end;

function TCustomSDVPanelTreeView.getAfterCollapse(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.AfterCollapse;
end;

function TCustomSDVPanelTreeView.getAfterExpand(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.AfterExpand;
end;

function TCustomSDVPanelTreeView.getAfterEmpty(): TOnSDVControlTreeNodeEvent;
begin
  Result := Items.AfterEmpty;
end;

procedure TCustomSDVPanelTreeView.setBeforeExplore
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.BeforeExplore := AValue;
end;

procedure TCustomSDVPanelTreeView.setBeforeCollapse
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.BeforeCollapse := AValue;
end;

procedure TCustomSDVPanelTreeView.setBeforeExpand
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.BeforeExpand := AValue;
end;

procedure TCustomSDVPanelTreeView.setBeforeEmpty
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.BeforeEmpty := AValue;
end;

procedure TCustomSDVPanelTreeView.setOnExplore
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.OnExplore := AValue;
end;

procedure TCustomSDVPanelTreeView.setAfterExplore
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.AfterExplore := AValue;
end;

procedure TCustomSDVPanelTreeView.setAfterCollapse
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.AfterCollapse := AValue;
end;

procedure TCustomSDVPanelTreeView.setAfterExpand
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.AfterExpand := AValue;
end;

procedure TCustomSDVPanelTreeView.setAfterEmpty
  (const AValue: TOnSDVControlTreeNodeEvent);
begin
  Items.AfterEmpty := AValue;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewChangingDispatcher
 (Sender: TObject; Node: TInternalTreeNode; var AllowChange: Boolean);
var ENode: TSDVPanelTreeviewNode;
begin
  AllowChange := FEnabled;
  if (AllowChange) then
  begin
    ENode := TSDVPanelTreeviewNode(Node.Data);
    AllowChange := ENode.Enabled;
  end;
  // Goal: Allow change of node when both tree and node are enabled.

  // Objetivo: Permitir cambio de nodo cuando ambos arbol y nodo
  // estan habilitados.
end;

procedure TCustomSDVPanelTreeView.OnTreeviewExpandingDispatcher
 (Sender: TObject; Node: TInternalTreeNode; var AllowExpansion: Boolean);
var ENode: TSDVPanelTreeviewNode;
begin
  AllowExpansion := FEnabled;
  if (AllowExpansion) then
  begin
    ENode := TSDVPanelTreeviewNode(Node.Data);
    if (ENode.State <> tsExpanded)
      then Items.DelegateBeforeExpand(ENode);
  end;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewCollapsingDispatcher
  (Sender: TObject; Node: TInternalTreeNode; var AllowCollapse: Boolean);
var ENode: TSDVPanelTreeviewNode;
begin
  AllowCollapse := FEnabled;
  if (AllowCollapse) then
  begin
    ENode := TSDVPanelTreeviewNode(Node.Data);
    if (ENode.State <> tsCollapsed)
      then Items.DelegateBeforeCollapse(ENode);
  end;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewCollapsedDispatcher
  (Sender: TObject; Node: TInternalTreeNode);
var ENode: TSDVPanelTreeviewNode;
begin
  ENode := TSDVPanelTreeviewNode(Node.Data);
  if (ENode.State <> tsCollapsed) then
  begin
    Items.DelegateAfterCollapse(ENode);
  end;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewExpandedDispatcher
  (Sender: TObject; Node: TInternalTreeNode);
var ENode: TSDVPanelTreeviewNode;
begin
  ENode := TSDVPanelTreeviewNode(Node.Data);
  if (ENode.State <> tsExpanded) then
  begin
    Items.DelegateAfterExpand(ENode);
  end;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewClickDispatcher(Sender: TObject);
var ANode: TInternalTreeNode; ENode: TSDVPanelTreeviewNode;
begin
  if (not IgnoreClick) then
  begin
    ANode := InternalTreeView.Selected;
    if (ANode <> nil) then
    begin
      ENode := TSDVPanelTreeviewNode(ANode.Data);
      FocusedNode := ENode;
      DelegateOnClick();
    end else
    begin
      FocusedNode := (Items.InternalRoot as TSDVPanelTreeviewNode);
    end;
  end;
  IgnoreClick := false;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewClickStateDispatcher
  (Sender: TObject; Node: TInternalTreeNode);
var ENode: TSDVPanelTreeviewNode;
begin
  IgnoreClick := true;
  if (Enabled) then
  begin
    ENode := TSDVPanelTreeviewNode(Node.Data);
    case (ENode.State) of
      tsEmpty:     (*Nothing*);
      tsCollapsed: ENode.Expand();
      tsExpanded:  ENode.Collapse();
      tsUnknown:   ENode.Explore();
      else ;
    end;
  end;
end;

procedure TCustomSDVPanelTreeView.OnTreeviewSelectionChangedDispatcher
  (Sender: TObject);
var ANode: TInternalTreeNode; ENode: TSDVPanelTreeviewNode;
begin
  ANode := InternalTreeView.Selected;
  if (ANode <> nil) then
  begin
    ENode := TSDVPanelTreeviewNode(ANode.Data);
    //FocusedNode := ENode;
    DelegateOnFocusedChanged();
  end else
  begin
    //FocusedNode := (Items.InternalRoot as TSDVPanelTreeviewNode);
  end;
end;

function TCustomSDVPanelTreeView.CreateTreeViewByClass(): TCustomSDVStateTreeView;
begin
  Result := TSDVStateTreeView.Create(Self);
end;

procedure TCustomSDVPanelTreeView.AssignInternalTreeView();
var ASize: Integer;
begin
  InternalTreeView := CreateTreeViewByClass();

  InternalTreeView.Parent := Self;
  InternalTreeView.Align  := alClient;
  InternalTreeView.ReadOnly    := true;
  InternalTreeView.HideSelection := Self.HideFocus;
  InternalTreeView.RowSelect   := true;
  InternalTreeView.ShowButtons := false;

  ASize := InternalTreeView.Font.Size;
  Self.Font.Size := ASize;

  InternalTreeView.OnClickStateImage :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewClickStateDispatcher;
  InternalTreeView.OnChanging :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewChangingDispatcher;
  InternalTreeView.OnExpanding :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewExpandingDispatcher;
  InternalTreeView.OnCollapsing :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewCollapsingDispatcher;
  InternalTreeView.OnExpanded   :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewExpandedDispatcher;
  InternalTreeView.OnCollapsed  :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewCollapsedDispatcher;
  InternalTreeView.OnClick :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewClickDispatcher;

  InternalTreeView.OnSelectionChanged :=
    {$IFNDEF DELPHI}@{$ENDIF}OnTreeviewSelectionChangedDispatcher;
end;

function TCustomSDVPanelTreeView.CreateCollectionByClass(): TSDVPanelTreeviewCollection;
begin
  Result := TSDVPanelTreeviewCollection.Create();
  Result.DoCreate();
  // Goal: Create inheretable (polimorphic) collection.
  // Objetivo: Crear coleccion heredable (polimorfica).
end;

procedure TCustomSDVPanelTreeView.FocusOffItem
  (var ANode: TSDVTreeNode; const AParam: pointer);
var ThisNode: TSDVPanelTreeviewNode;
begin
  ThisNode := TSDVPanelTreeviewNode(ANode);
  ThisNode.Focused := false;
end;

procedure TCustomSDVPanelTreeView.EnableItem
  (var ANode: TSDVTreeNode; const AParam: pointer);
var ThisNode: TSDVPanelTreeviewNode; InternalTreenode: TInternalTreeNode;
begin
  if (not Enabled) then
  begin
    ThisNode := TSDVPanelTreeviewNode(ANode);
    InternalTreenode := ThisNode.InternalTreenode;
    if (InternalTreenode <> nil) then
    begin
      InternalTreenode.ImageIndex    := ThisNode.DisabledIndex;
      InternalTreenode.SelectedIndex := ThisNode.KeepIndex
    end;
  end else
  begin
    ThisNode.UpdateImageIndex();
  end;
end;

procedure TCustomSDVPanelTreeView.ConfirmFocused
  (const ANode: TSDVPanelTreeviewNode);
begin
  FInternalTreeView.Selected := ANode.InternalTreenode;
  Application.ProcessMessages();
end;

procedure TCustomSDVPanelTreeView.RefreshRootItem();
var ARootNode: TSDVPanelTreeviewNode;
begin
  ARootNode := (Items.InternalRoot as TSDVPanelTreeviewNode);
  if (ARootNode <> nil) then
  begin
    ARootNode.InternalTreenodeDeassign();
    ARootNode.InternalTreenodeAssign(true);

    ARootNode.UpdateText();
    ARootNode.UpdateImageIndex();
    ARootNode.UpdateStateImageIndex();
  end;
end;

function TCustomSDVPanelTreeView.CreateCollection(): TSDVPanelTreeViewCollection;
begin
  Result := CreateCollectionByClass();
  Result.Treeview := Self;
  // Goal: Create inheretable (polimorphic) collection
  // and perform addtional actions based in current InternalTreeView class.

  // Objetivo: Crear coleccion heredable (polimorfica)
  // y realizar acciones adicionales basadas en la clase nodoarbol actual.
end;

procedure TCustomSDVPanelTreeView.DestroyCollection();
begin
  Items.Free();
  Items := nil;
  // Goal: Destroy inheretable (polimorphic) collection
  // and perform addtional actions based in current InternalTreeView class.

  // Objetivo: Destruir coleccion heredable (polimorfica)
  // y realizar acciones adicionales basadas en la clase nodoarbol actual.
end;

procedure TCustomSDVPanelTreeView.CreateImages();
begin
  States := SharedButtonImages;
  // make connection with "FSharedButtonImages"
  // hacer conexion con "FSharedButtonImages"
end;

procedure TCustomSDVPanelTreeView.DestroyImages();
begin
  States := nil;
  // Break connection with "FSharedButtonImages"
  // Romper conexion con "FSharedButtonImages"
end;

procedure TCustomSDVPanelTreeView.DelegateOnClick();
begin
  if (FOnClick <> nil) then
  begin
    FOnClick(FocusedNode);
  end;
end;

procedure TCustomSDVPanelTreeView.DelegateOnFocusedChanged();
begin
  if (FOnFocusedChanged <> nil) then
  begin
    FOnFocusedChanged(FocusedNode);
  end;
end;

procedure TCustomSDVPanelTreeView.TreeviewRootAssign();
var ARootNode: TSDVPanelTreeviewNode;
    ACollection: TSDVPanelTreeviewCollection;
begin
  ACollection := (Self.Items as TSDVPanelTreeviewCollection);
  ARootNode   := (ACollection.InternalRoot as TSDVPanelTreeviewNode);

  RefreshRootItem();

  FocusedNode  := ARootNode;
  // select root node as default
  // selecionar nodo raiz por default
end;

procedure TCustomSDVPanelTreeView.TreeviewRootDeassign();
begin
  // eliminar nodo raiz interno de control "InternalTreeView"
  Self.InternalTreeView.Items.Clear();
end;

function TCustomSDVPanelTreeView.InsertInternalTreeNode
  (const AParent: TInternalTreeNode): TInternalTreeNode;
begin
  Result := FInternalTreeView.Items.AddChild(AParent, '');
end;

constructor TCustomSDVPanelTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BevelInner  := bvNone;
  BevelOuter  := bvNone;
  FEnabled    := true;
  IgnoreClick := false;
  Height      := 97;
  Width       := 121;

  AssignInternalTreeView();

  Self.FItems := CreateCollection();
  // create inheretable (polimorphic) collection
  // crear coleccion heredable (polimorfica)

  CreateImages();
end;

destructor TCustomSDVPanelTreeView.Destroy();
begin
  DestroyImages();
  DestroyCollection();

  inherited Destroy();
end;

procedure TCustomSDVPanelTreeView.ActivateFirst();
begin
  //Items.Activated := true;
  RefreshRootItem();
  FocusedNode := (Items.InternalRoot as TSDVPanelTreeviewNode);
end;

(* Internal functions *)

procedure UnitConstructor;
begin
  FSharedButtonImages := TImageList.Create(nil);
  FSharedButtonImages.Height := 16;
  FSharedButtonImages.Width  := 16;
  // default size without checked images
  // tamaño default sin imagenes casillas

  FSharedButtonImages.AddLazarusResource('BTNIGNORE', clTeal);
  // Load null image, treeview bug ?
  // Cargar imagen nula, bug vistaarbol ?

  FSharedButtonImages.AddLazarusResource('BTNEMPTY', clTeal);
  FSharedButtonImages.AddLazarusResource('BTNCOLLAPSED', clTeal);
  FSharedButtonImages.AddLazarusResource('BTNEXPANDED', clTeal);
  FSharedButtonImages.AddLazarusResource('BTNEXPLORING', clTeal);
  FSharedButtonImages.AddLazarusResource('BTNUNKNOWN', clTeal);
  // Goal: Create image containers for checkboxes.
  // Objetivo: Crear contenedor de imagenes para cajamarcas.
end;

procedure UnitDestructor;
begin
  FSharedButtonImages.Free;
  // Goal: Destroy image containers for checkboxes.
  // Objetivo: Destruir contenedor de imagenes para cajamarcas.
end;

initialization
  {$IFDEF FPC}
  {$I 'uktpaneltreeviewsres.lrs'}
  {$ENDIF}
  UnitConstructor;
finalization
  UnitDestructor;
end.
