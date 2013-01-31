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

unit ukttreeviews;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktactivatedcontrols,
  uktpublictreeviews,
  dummy;

// Objetivo: Los controles "TCustomSDXXX" se proveen con el fin
// de reemplazar algunas propiedades "estaticas" comunes por
// propiedades "dinamicas", ademas de la propiedad "Activated"
// que indica si la forma que contiene el control ha sido
// activada al menos una vez.

// Goal: "TCustomSDXXX" controls are provided in order to
// replace common some "static" properties for "dynamic" properties,
// also includes the "Activated" property that indicates if the
// form has been activated unleast once.

(**
 ** The "TCustomSDVTreeView" control class is a descendant,
 ** of the "TCustomPublicTreeView" control class,
 ** redeclare that properties (and events).
 **
 ** Supports, also, the "ISDVControl" interface.
 **)

type

 (* TCustomInternalTreeView *)

   TCustomInternalTreeView = class(TCustomPublicTreeView)
   private
     (* Private declarations *)

     function getInternalFont: TFont; virtual;
     function getInternalText: string;
     function getInternalEnabled: Boolean;
     function getInternalReadOnly: Boolean;
     function getInternalVisible: Boolean;

     procedure setInternalFont(const Value: TFont); virtual;
     procedure setInternalText(const Value: string);
     procedure setInternalEnabled(const Value: Boolean);
     procedure setInternalReadOnly(const Value: Boolean);
     procedure setInternalVisible(const Value: Boolean);
   protected
     (* Protected declarations *)

     property InternalFont: TFont
       read getInternalFont write setInternalFont;
     property InternalText: string
       read getInternalText write setInternalText;
     property InternalEnabled: Boolean
       read getInternalEnabled write setInternalEnabled;
     property InternalReadOnly: Boolean
       read getInternalReadOnly write setInternalReadOnly;
     property InternalVisible: Boolean
       read getInternalVisible write setInternalVisible;
   public
     (* Public declarations *)
   end;

 (* TCustomSDVTreeView *)

   TCustomSDVTreeView = class(TCustomInternalTreeView, ISDVActivatedControl)
   private
     (* Private declarations *)

     FActivated: Boolean;

     function getActivated: Boolean;
     procedure setActivated(const Value: Boolean);
   protected
     (* Protected declarations *)

     FReadOnly: Boolean;

     function getText: string; reintroduce; virtual;
     function getEnabled: Boolean; reintroduce; virtual;
     function getReadOnly: Boolean; reintroduce; virtual;
     function getVisible: Boolean; reintroduce; virtual;
     function getFont: TFont; reintroduce; virtual;

     procedure setText(const Value: string); reintroduce; virtual;
     procedure setEnabled(const Value: Boolean); reintroduce; virtual;
     procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
     procedure setVisible(const Value: Boolean); reintroduce; virtual;
     procedure setFont(const Value: TFont); reintroduce; virtual;

     procedure ActivateFirst(); virtual;
     procedure DeactivateLast(); virtual;
   public
     (* Public declarations *)

     property Activated: Boolean
       read getActivated write setActivated;
     property Font: TFont
       read getFont write setFont;
     property Text: string
       read getText write setText;
     property Enabled: Boolean
       read getEnabled write setEnabled;
     property ReadOnly: Boolean
       read getReadOnly write setReadOnly;
   end;

 (* TSDVTreeView *)

   TSDVTreeView = class(TCustomSDVTreeView)
   published
     (* Published declarations *)

     (* TCustomSDVTreeView: *)

     property Align;
     property Anchors;
     property AutoExpand;
     property BorderSpacing;
     //property BiDiMode;
     property BackgroundColor;
     property BorderStyle;
     property BorderWidth;
     {$IFDEF DELPHI}
     property Ctl3D;
     {$ENDIF}
     property Color;
     property Constraints;
     property DefaultItemHeight;
     property DragKind;
     property DragCursor;
     property DragMode;
     property Enabled;
     property ExpandSignType;
     property Font;
     property HideSelection;
     property HotTrack;
     property Images;
     property Indent;
     property MultiSelect;
     property MultiSelectStyle;
     //property ParentBiDiMode;
     {$IFDEF DELPHI}
     property ParentCtl3D;
     {$ENDIF}
     property ParentColor;
     property ParentFont;
     property ParentShowHint;
     property PopupMenu;
     property ReadOnly;
     property RightClickSelect;
     property RowSelect;
     property ScrollBars;
     property SelectionColor;
     property ShowButtons;
     property ShowHint;
     property ShowLines;
     property ShowRoot;
     property SortType;
     property StateImages;
     property TabOrder;
     property TabStop;
     property Tag;
     property ToolTips;
     property Visible;

     property OnAddition;
     property OnAdvancedCustomDraw;
     property OnAdvancedCustomDrawItem;
     property OnChange;
     property OnChanging;
     property OnClick;
     property OnCollapsed;
     property OnCollapsing;
     property OnCompare;
     property OnContextPopup;
     property OnCreateNodeClass;
     property OnCustomCreateItem;
     property OnCustomDraw;
     property OnCustomDrawItem;
     property OnDblClick;
     property OnDeletion;
     property OnDragDrop;
     property OnDragOver;
     property OnEdited;
     property OnEditing;
     property OnEditingEnd;
     //property OnEndDock;
     property OnEndDrag;
     property OnEnter;
     property OnExit;
     property OnExpanded;
     property OnExpanding;
     property OnGetImageIndex;
     property OnGetSelectedIndex;
     property OnKeyDown;
     property OnKeyPress;
     property OnKeyUp;
     property OnMouseDown;
     property OnMouseEnter;
     property OnMouseLeave;
     property OnMouseMove;
     property OnMouseUp;
     property OnSelectionChanged;
     property OnShowHint;
     //property OnStartDock;
     property OnStartDrag;
     property OnUTF8KeyPress;
     property Options;
     property Items;
     property TreeLineColor;
     property TreeLinePenStyle;
     property ExpandSignColor;

     (* TCustomPublicTreeView: *)

     property Activated;
     property Text;
   end;

implementation

(* TCustomInternalTreeView *)

function TCustomInternalTreeview.getInternalFont: TFont;
begin
  Result := inherited Font;
  // Goal: "InternalFont" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalFont".
end;

function TCustomInternalTreeview.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalTreeview.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalTreeview.getInternalReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
  // Goal: "InternalReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalReadOnly".
end;

function TCustomInternalTreeview.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

procedure TCustomInternalTreeView.setInternalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  // Goal: "InternalFont" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalFont".
end;

procedure TCustomInternalTreeview.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalTreeview.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalTreeview.setInternalReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  // Goal: "InternalReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalReadOnly".
end;

procedure TCustomInternalTreeview.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

(* TCustomSDVTreeView *)

function TCustomSDVTreeView.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVTreeView.setActivated(const Value: Boolean);
begin
  if (FActivated <> Value) then
  begin
    FActivated := Value;
    if (Value)
      then ActivateFirst()
      else DeActivateLast();
  end;
  // Goal: "Activated" property set method.
  // Objetivo: Metodo escritura para propiedad "Activated".
end;

function TCustomSDVTreeview.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVTreeview.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVTreeview.getReadOnly: Boolean;
begin
  Result := FReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVTreeview.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVTreeview.getFont: TFont;
begin
  Result := InternalFont;
  // Goal: "Font" property get method.
  // Objetivo: Metodo lectura para propiedad "Font".
end;

procedure TCustomSDVTreeview.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVTreeview.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVTreeview.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVTreeview.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVTreeview.setFont(const Value: TFont);
begin
  InternalFont := Value;
  // Goal: "Font" property set method.
  // Objetivo: Metodo escritura para propiedad "Font".
end;

procedure TCustomSDVTreeview.ActivateFirst();
begin
  // Goal: Perform an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVTreeview.DeactivateLast();
begin
  // Goal: Perform an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

end.

