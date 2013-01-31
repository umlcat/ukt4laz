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

unit uktcontroltreenodes;

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

 (* TOnSDVControlTreeNodeEvent *)

   TSDVControlTreeNode = class;

   TOnSDVControlTreeNodeCanEvent =
     function (const ANode: TSDVControlTreeNode): Boolean of object;
   TOnSDVControlTreeNodeEvent =
     procedure (const ANode: TSDVControlTreeNode) of object;

 (* TSDVControlTreeNode *)

   TSDVControlTreeNode = class(TSDVTextTreeNode)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     // properties related to visual controls
     FEnabled:  Boolean;

     // there can be several, maybe hidden, randomly selected nodes,
     // but, only one visible focused node
     FFocused:   Boolean;
     FSelected:  Boolean;

     FVisible:  Boolean;

     FImageIndex:    Integer; // item Enabled and not Focused
     FFocusedIndex:  Integer; // item Enabled and Focused
     FDisabledIndex: Integer; // item disabled and not Focused
     FKeepIndex:     Integer; // item disabled and Focused

     FInternalState: TTreeStates;
   protected
     (* Protected declarations *)

     (* accessors declarations *)

     function getEnabled(): Boolean; virtual;

     // there can be several, maybe hidden, randomly selected nodes,
     // but, only one visible focused node
     function getFocused(): Boolean; virtual;
     function getSelected(): Boolean; virtual;

     function getState(): TTreeStates;

     procedure setEnabled(const AValue: Boolean); virtual;

     // there can be several, maybe hidden, randomly selected nodes,
     // but, only one visible focused node
     procedure setFocused(const AValue: Boolean); virtual;
     procedure setSelected(const AValue: Boolean); virtual;

     procedure setState(const AValue: TTreeStates); virtual;

     function getImageIndex: Integer;
     function getFocusedIndex: Integer; virtual;
     function getDisabledIndex: Integer;
     function getKeepIndex: Integer;

     procedure setImageIndex(const AValue: Integer);
     procedure setFocusedIndex(const AValue: Integer);
     procedure setDisabledIndex(const AValue: Integer);
     procedure setKeepIndex(const AValue: Integer);

     function getInternalState(): TTreeStates; virtual;
     procedure setInternalState(const AValue: TTreeStates); virtual;
   public
     (* Protected Friend declarations *)

     procedure InternalExplore(); virtual;
     procedure InternalCollapse(); virtual;
     procedure InternalExpand(); virtual;
     procedure InternalEmpty(); virtual;
     procedure InternalHide(); virtual;
   protected
     (* Protected declarations *)

     procedure UpdateImageIndex(); virtual;
   public
     (* Protected Friend declarations *)

     (* DO NOT become published declarations *)

     // "InternalState" property does not refresh visually,
     // the state, of the node, "State" property does
     property InternalState: TTreeStates
       read getInternalState write setInternalState;
   public
     (* Public declarations *)

     procedure DoCreate(); override;
   public
     (* Public declarations *)

     procedure Explore(); (* nonvirtual; *)
     procedure Collapse(); (* nonvirtual; *)
     procedure Expand(); (* nonvirtual; *)
     procedure Empty(); (* nonvirtual; *)

     procedure UpdateExpand(); (* nonvirtual; *)

     procedure Hide(); (* nonvirtual; *)
   published
     (* Published declarations *)

     property Enabled: Boolean
       read getEnabled write setEnabled;

     // there can be several, maybe hidden, randomly selected nodes,
     // but, only one visible focused node
     property Focused: Boolean
       read getFocused write setFocused;
     property Selected: Boolean
       read getSelected write setSelected;

     // "InternalState" property does not refresh visually,
     // the state, of the node, "State" property does
     property State: TTreeStates
       read getState write setState;

     property ImageIndex: Integer
       read getImageIndex write setImageIndex;
     property SelectedIndex: Integer
       read getFocusedIndex write setFocusedIndex;
     property DisabledIndex: Integer
       read getDisabledIndex write setDisabledIndex;
     property KeepIndex: Integer
       read getKeepIndex write setKeepIndex;
   end;

 (* TSDVControlTreeCollection *)

   TSDVControlTreeCollection = class(TSDVTextTreeCollection)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)

     function CreateNodeByClass(): TSDVTreeNode; override;

     procedure InternalConfirmedInsert
       (const AParentNode: TSDVTreeNode; var ANewNode: TSDVTreeNode); override;
     procedure InternalConfirmedInsertAt
       (const AParentNode: TSDVTreeNode;
        const AIndex: Integer; var ANewNode: TSDVTreeNode); override;
   protected
     (* Protected declarations *)

     FBeforeExplore:  TOnSDVControlTreeNodeEvent;
     FBeforeCollapse: TOnSDVControlTreeNodeEvent;
     FBeforeExpand:   TOnSDVControlTreeNodeEvent;
     FBeforeEmpty:    TOnSDVControlTreeNodeEvent;

     FOnExplore:      TOnSDVControlTreeNodeEvent;

     FAfterExplore:   TOnSDVControlTreeNodeEvent;
     FAfterCollapse:  TOnSDVControlTreeNodeEvent;
     FAfterExpand:    TOnSDVControlTreeNodeEvent;
     FAfterEmpty:     TOnSDVControlTreeNodeEvent;
   public
     (* Protected Friend declarations *)

     procedure DelegateBeforeExplore(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateBeforeCollapse(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateBeforeExpand(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateBeforeEmpty(const ANode: TSDVControlTreeNode); (*nonvirtual;*)

     procedure DelegateOnExplore(const ANode: TSDVControlTreeNode); (*nonvirtual;*)

     procedure DelegateAfterExplore(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateAfterCollapse(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateAfterExpand(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
     procedure DelegateAfterEmpty(const ANode: TSDVControlTreeNode); (*nonvirtual;*)
   public
     (* Public declarations *)

     property BeforeExplore: TOnSDVControlTreeNodeEvent
       read FBeforeExplore write FBeforeExplore;
     property BeforeCollapse: TOnSDVControlTreeNodeEvent
       read FBeforeCollapse write FBeforeCollapse;
     property BeforeExpand: TOnSDVControlTreeNodeEvent
       read FBeforeExpand write FBeforeExpand;
     property BeforeEmpty: TOnSDVControlTreeNodeEvent
       read FBeforeEmpty write FBeforeEmpty;

     property OnExplore: TOnSDVControlTreeNodeEvent
       read FOnExplore write FOnExplore;

     property AfterExplore: TOnSDVControlTreeNodeEvent
       read FAfterExplore write FAfterExplore;
     property AfterCollapse: TOnSDVControlTreeNodeEvent
       read FAfterCollapse write FAfterCollapse;
     property AfterExpand: TOnSDVControlTreeNodeEvent
       read FAfterExpand write FAfterExpand;
     property AfterEmpty: TOnSDVControlTreeNodeEvent
       read FAfterEmpty write FAfterEmpty;
   end;

implementation

(* TSDVControlTreeNode *)

function TSDVControlTreeNode.getEnabled(): Boolean;
begin
  Result := FEnabled;
end;

function TSDVControlTreeNode.getFocused: Boolean;
begin
  Result := FFocused;
end;

function TSDVControlTreeNode.getSelected(): Boolean;
begin
  Result := FSelected;
end;

function TSDVControlTreeNode.getState(): TTreeStates;
begin
  Result := FInternalState;
end;

procedure TSDVControlTreeNode.setEnabled(const AValue: Boolean);
begin
  if (FEnabled <> AValue) then
  begin
    FEnabled := AValue;
  end;
end;

procedure TSDVControlTreeNode.setFocused(const AValue: Boolean);
begin
  if (FFocused <> AValue) then
  begin
    FFocused := AValue;
  end;
end;

procedure TSDVControlTreeNode.setSelected(const AValue: Boolean);
begin
  // @to-do: change state image ???
  if (FSelected <> AValue) then
  begin
    FSelected := AValue;
  end;
end;

procedure TSDVControlTreeNode.setState(const AValue: TTreeStates);
begin
  if (FInternalState <> AValue) then
  begin
    FInternalState := AValue;
  end;
end;

function TSDVControlTreeNode.getImageIndex(): Integer;
begin
  Result := FImageIndex;
end;

function TSDVControlTreeNode.getFocusedIndex(): Integer;
begin
  Result := FFocusedIndex;
end;

function TSDVControlTreeNode.getDisabledIndex(): Integer;
begin
  Result := FDisabledIndex;
end;

function TSDVControlTreeNode.getKeepIndex(): Integer;
begin
  Result := FKeepIndex;
end;

procedure TSDVControlTreeNode.setImageIndex(const AValue: Integer);
begin
  FImageIndex := AValue;
  UpdateImageIndex();
end;

procedure TSDVControlTreeNode.setFocusedIndex(const AValue: Integer);
begin
  FFocusedIndex := AValue;
  UpdateImageIndex();
end;

procedure TSDVControlTreeNode.setDisabledIndex(const AValue: Integer);
begin
  FDisabledIndex := AValue;
  UpdateImageIndex();
end;

procedure TSDVControlTreeNode.setKeepIndex(const AValue: Integer);
begin
  FKeepIndex := AValue;
  UpdateImageIndex();
end;

function TSDVControlTreeNode.getInternalState(): TTreeStates;
begin
  Result := FInternalState;
end;

procedure TSDVControlTreeNode.setInternalState(const AValue: TTreeStates);
begin
  if (FInternalState <> AValue) then
  begin
    FInternalState := AValue;
  end;
end;

procedure TSDVControlTreeNode.InternalExplore();
begin
  if (HasItems()) then
  begin
    InternalState := ukttreestates.tsExpanded;
  end else
  begin
    InternalState := ukttreestates.tsEmpty;
  end;
end;

procedure TSDVControlTreeNode.InternalCollapse();
begin
  if (HasItems()) then
  begin
    InternalState := ukttreestates.tsCollapsed;
  end else
  begin
    InternalState := ukttreestates.tsEmpty;
  end;
end;

procedure TSDVControlTreeNode.InternalExpand();
begin
  if (HasItems()) then
  begin
    // change state & related state image
    InternalState := ukttreestates.tsExpanded;
  end else
  begin
    // change state & related state image
    InternalState := ukttreestates.tsEmpty;
  end;
end;

procedure TSDVControlTreeNode.InternalEmpty();
begin
  InternalList.Empty();
  InternalState := ukttreestates.tsEmpty;
end;

procedure TSDVControlTreeNode.InternalHide();
begin
  InternalState := ukttreestates.tsUnknown;
  // Goal: Makes subitems not visible,
  // NOT the node itself.
end;

procedure TSDVControlTreeNode.UpdateImageIndex;
begin
  //
end;

procedure TSDVControlTreeNode.DoCreate();
begin
  inherited DoCreate();

  FInternalState := ukttreestates.tsUnknown;
  FVisible := false;
  FEnabled := true;
end;

procedure TSDVControlTreeNode.Explore();
var ACollection: TSDVControlTreeCollection;
begin
  if (FInternalState = ukttreestates.tsUnknown) then
  begin
    InternalState := ukttreestates.tsExploring;

    ACollection
      := (InternalCollection  as TSDVControlTreeCollection);

    ACollection.DelegateBeforeExplore(Self);

    InternalExplore();
    ACollection.DelegateOnExplore(Self);

    ACollection.DelegateAfterExplore(Self);

    Self.Expand();
  end;
end;

procedure TSDVControlTreeNode.Collapse();
var ACollection: TSDVControlTreeCollection;
begin
  if (FInternalState = ukttreestates.tsExpanded) then
  begin
    ACollection
      := (InternalCollection  as TSDVControlTreeCollection);

    ACollection.DelegateBeforeCollapse(Self);
    InternalCollapse();
    ACollection.DelegateAfterCollapse(Self);
  end;
end;

procedure TSDVControlTreeNode.Expand();
var ACollection: TSDVControlTreeCollection;
    CanExpand: Boolean;
begin
  CanExpand :=
    ( (FInternalState = ukttreestates.tsExploring)
      or (FInternalState = ukttreestates.tsCollapsed)
    );
  if (CanExpand) then
  begin
    ACollection
      := (InternalCollection  as TSDVControlTreeCollection);

    ACollection.DelegateBeforeExpand(Self);
    InternalExpand();
    ACollection.DelegateAfterExpand(Self);
  end;
end;

procedure TSDVControlTreeNode.Empty();
var ACollection: TSDVControlTreeCollection;
begin
  if (FInternalState <> ukttreestates.tsEmpty) then
  begin
    ACollection
      := (InternalCollection  as TSDVControlTreeCollection);

    ACollection.DelegateBeforeEmpty(Self);
    InternalEmpty();
    ACollection.DelegateAfterEmpty(Self);
  end;
end;

procedure TSDVControlTreeNode.UpdateExpand();
begin
  // hide visible child nodes from node,
  // if any, and its expanded,
  // otherwise, do nothing
  //if (Self.State = ukttreestates.tsExpanded) then
  //begin
    //Self.Collapse();
  //end;
  Self.Hide();

  // cannot "expand" without a "explore",
  // but, "explore" calls "expand"
  Self.Explore();

  // show again, all nodes, including new node
  Self.Expand();
end;

procedure TSDVControlTreeNode.Hide();
var ACollection: TSDVControlTreeCollection;
begin
  InternalHide();
  // Goal: Changes a node state to "unknown",
  // and hides any visible subitems.
end;

(* TSDVControlTreeCollection *)

function TSDVControlTreeCollection.CreateNodeByClass(): TSDVTreeNode;
begin
  Result := TSDVControlTreeNode.Create();
  Result.DoCreate();
end;

procedure TSDVControlTreeCollection.InternalConfirmedInsert
  (const AParentNode: TSDVTreeNode; var ANewNode: TSDVTreeNode);
var APanelParentNode, APanelNewNode: TSDVControlTreeNode;
begin
  inherited InternalConfirmedInsert(AParentNode, ANewNode);

  // generate initial caption
  APanelParentNode   := (AParentNode as TSDVControlTreeNode);
  APanelNewNode      := (ANewNode as TSDVControlTreeNode);
  APanelNewNode.Text := Self.DefaultText();

  // update parent node state
  if (APanelParentNode <> nil) then
  begin
    case (APanelParentNode.InternalState) of
      ukttreestates.tsEmpty:     APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      ukttreestates.tsCollapsed: APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      ukttreestates.tsExpanded:  APanelParentNode.InternalState := ukttreestates.tsExpanded;
      ukttreestates.tsUnknown:   APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      else ;
    end;
  end;
end;

procedure TSDVControlTreeCollection.InternalConfirmedInsertAt
  (const AParentNode: TSDVTreeNode; const AIndex: Integer;
   var ANewNode: TSDVTreeNode);
var APanelParentNode, APanelNewNode: TSDVControlTreeNode;
begin
  inherited InternalConfirmedInsertAt(AParentNode, AIndex, ANewNode);

  // generate initial caption
  APanelParentNode   := (AParentNode as TSDVControlTreeNode);
  APanelNewNode      := (ANewNode as TSDVControlTreeNode);
  APanelNewNode.Text := Self.DefaultText();

  // update parent node state
  if (APanelParentNode <> nil) then
  begin
    case (APanelParentNode.InternalState) of
      ukttreestates.tsEmpty:     APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      ukttreestates.tsCollapsed: APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      ukttreestates.tsExpanded:  APanelParentNode.InternalState := ukttreestates.tsExpanded;
      ukttreestates.tsUnknown:   APanelParentNode.InternalState := ukttreestates.tsCollapsed;
      else ;
    end;
  end;
end;

procedure TSDVControlTreeCollection.DelegateBeforeExplore
  (const ANode: TSDVControlTreeNode);
begin
  if (FBeforeExplore <> nil) then
  begin
    FBeforeExplore(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateBeforeCollapse
  (const ANode: TSDVControlTreeNode);
begin
  if (FBeforeCollapse <> nil)then
  begin
    FBeforeCollapse(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateBeforeExpand
  (const ANode: TSDVControlTreeNode);
begin
  if (FBeforeExpand <> nil) then
  begin
    FBeforeExpand(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateBeforeEmpty
  (const ANode: TSDVControlTreeNode);
begin
  if (FBeforeEmpty <> nil) then
  begin
    FBeforeEmpty(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateOnExplore
  (const ANode: TSDVControlTreeNode);
begin
  if (FOnExplore <> nil) then
  begin
    FOnExplore(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateAfterExplore
  (const ANode: TSDVControlTreeNode);
begin
  if (FAfterExplore <> nil) then
  begin
    FAfterExplore(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateAfterCollapse
  (const ANode: TSDVControlTreeNode);
begin
  if (FAfterCollapse <> nil) then
  begin
    FAfterCollapse(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateAfterExpand
  (const ANode: TSDVControlTreeNode);
begin
  if (FAfterExpand <> nil) then
  begin
    FAfterExpand(ANode);
  end;
end;

procedure TSDVControlTreeCollection.DelegateAfterEmpty
  (const ANode: TSDVControlTreeNode);
begin
  if (FAfterEmpty <> nil) then
  begin
    FAfterEmpty(ANode);
  end;
end;

end.

