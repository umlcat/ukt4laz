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

unit uktstatetreeviews;

interface
uses
  //Windows,
  //Messages,
  SysUtils,
  Classes,
  Controls,
  {$IFDEF MSWINDOWS}
  CommCtrl,
  COMCtrls,
  {$ENDIF}
  Forms,
  ExtCtrls,
  //ImgList,
  Dialogs, Graphics,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktpublictreeviews,
  dummy;

type

// Pointer to TTKeyDown message structure

  PTVKeyDown  = ^TTVKeyDown;

(* TOnCanNodeEvent *)

  TOnCanNodeEvent =
    function(Sender: TObject; ANode: TTreeNode): Boolean of object;

(* TOnNodeEvent *)

  TOnNodeEvent =
    procedure(Sender: TObject; ANode: TTreeNode) of object;

(* TCustomSDVStateTreeView *)

  TCustomSDVStateTreeView = class(TCustomPublicTreeView)
  private
    (* Private declarations *)

    FOnCanStateImage: TOnCanNodeEvent;
    FOnClickStateImage: TOnNodeEvent;

    function HasStateImage(ANode: TTreeNode): Boolean;
    //procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    (* Protected declarations *)

    procedure PerformStateImageClicked(ANode: TTreeNode); virtual;
    procedure DetectStateImage(ANode: TTreeNode; Pt: TPoint); overload; virtual;
  public
    (* Public declarations *)

    function  DelegateOnCanStateImage(ANode: TTreeNode): Boolean; virtual;
    procedure DelegateOnClickStateImage(ANode: TTreeNode); virtual;

    property OnCanStateImage: TOnCanNodeEvent
      read FOnCanStateImage write FOnCanStateImage;
    property OnClickStateImage: TOnNodeEvent
      read FOnClickStateImage write FOnClickStateImage;
  end;

(* TSDVStateTreeView *)

  TSDVStateTreeView = class(TCustomSDVStateTreeView)
  published
    (* Published declarations *)

    (* TCustomPanel: *)

    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    //property ChangeDelay;
    property Color;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property Items;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop;
    property ToolTips;
    property Visible;

    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsing;
    property OnCollapsed;
    property OnCompare;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    (* TCustomSDVStateTreeView: *)

    property OnCanStateImage;
    property OnClickStateImage;
  end;

implementation

(* TCustomSDVStateTreeView *)

function TCustomSDVStateTreeView.HasStateImage(ANode: TTreeNode): Boolean;
begin
  Result := (1 = 1)
    and (StateImages <> nil)
    and (ANode <> nil)
    and (ANode.StateIndex > 0);
  // Goal: Detects where a node has an state image.
  // Objetivo: Detectar cuando un nodo tiene una imagen de estado.
end;

(*
procedure TCustomSDVStateTreeView.CNNotify(var Message: TWMNotify);
var ATreeNode: TTreeNode; MousePos: TPoint;
begin
  inherited;
  with Message do
    case NMHdr^.code of
      NM_CLICK:
        begin
          GetCursorPos(MousePos);
          MousePos := ScreenToClient(MousePos);
          ATreeNode := GetNodeAt(MousePos.X, MousePos.Y);

          if (HasStateImage(ATreeNode))
            then DetectStateImage(ATreeNode,MousePos);
        end;
      TVN_KEYDOWN:
        if ((PTVKeyDown(NMHdr)^.wVKey = 32) and HasStateImage(Selected))
          then PerformStateImageClicked(Selected);
    end;
   // Goal: Detect the "MouseClick" & "KeyDown" events in order to modify
   // the associated checbox state for each item.

   // Objetivo: Detectar los eventos "MouseClick" y "KeyDown" con el fin
   // de modificar el estado del checkbox asociado de cada elemento.
end;
*)

procedure TCustomSDVStateTreeView.PerformStateImageClicked(ANode: TTreeNode);
begin
  if (DelegateOnCanStateImage(ANode)) then
  begin
    DelegateOnClickStateImage(ANode);
  end;
  // Goal: Delegate event handler after click state image.

  // Objetivo: Delegar manejador de evento despues
  // de cliquear la imagen de estado.
end;

procedure TCustomSDVStateTreeView.DetectStateImage(ANode: TTreeNode; Pt: TPoint);
//var ARect: TRect; AWidth: Integer;
begin
  (*
  TreeView_GetItemRect(Handle,Node.ItemId,ARect,true);

  if (Assigned(Images) and (Node.ImageIndex >= 0))
    then AWidth := 19
    else AWidth := 0;

  if ((Pt.y >= (ARect.Top + 3))            and (Pt.y <= (ARect.Top + 13)) and
      (Pt.x >= (ARect.Left - AWidth - 14)) and (Pt.x <= (ARect.Left - AWidth - 4)))
     then PerformStateImageClicked(Node);
     *)

  // Goal: Check if the mouse pointer click into stateimage area
  // and change the state.

  // Objetivo: Verificar si el puntero del mouse hace click dentro
  // del area de la imagen y cambiar el estado.
end;

function TCustomSDVStateTreeView.DelegateOnCanStateImage(ANode: TTreeNode): Boolean;
begin
  if (FOnCanStateImage <> nil)
    then Result := OnCanStateImage(Self, ANode)
    else Result := true;
  // Goal: To execute the "OnCanStateImage" event, true by default.
  // Objetivo: Ejecutar el evento "OnCanStateImage", VERDADERO por default.
end;

procedure TCustomSDVStateTreeView.DelegateOnClickStateImage(ANode: TTreeNode);
begin
  if (FOnClickStateImage <> nil) then
  begin
    FOnClickStateImage(Self, ANode);
  end;
   // Goal: To execute the "OnClickStateImage" event.
  // Objetivo: Ejecutar el evento "OnClickStateImage".
end;

end.
