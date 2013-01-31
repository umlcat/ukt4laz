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

unit uktmaskeds;

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls, 
  //StdCtrls,
  Forms, 
  MaskEdit,
(*.ENDIF*)
  SysUtils, Classes,
  uktactivatedcontrols,
  //vcluktCtrls;
  dummy;

type

(* TCustomInternalMaskEdit *)

  TCustomInternalMaskEdit = class(TCustomMaskEdit)
  private
    (* Private declarations *)

    function getInternalText: string;
    function getInternalEnabled: Boolean;
    function getInternalReadOnly: Boolean;
    function getInternalVisible: Boolean;

    procedure setInternalText(const Value: string);
    procedure setInternalEnabled(const Value: Boolean);
    procedure setInternalReadOnly(const Value: Boolean);
    procedure setInternalVisible(const Value: Boolean);
  protected
    (* Protected declarations *)

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

(* TCustomSDVMaskEdit *)

  TCustomSDVMaskEdit = class(TCustomInternalMaskEdit, ISDVActivatedControl)
  private
    (* Private declarations *)

    FActivated: Boolean;

    function getActivated: Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    (* Protected declarations *)

    function getText: string; reintroduce; virtual;
    function getEnabled: Boolean; reintroduce; virtual;
    function getReadOnly: Boolean; reintroduce; virtual;
    function getVisible: Boolean; reintroduce; virtual;

    procedure setText(const Value: string); reintroduce; virtual;
    procedure setEnabled(const Value: Boolean); reintroduce; virtual;
    procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
    procedure setVisible(const Value: Boolean); reintroduce; virtual;

    procedure ActivateFirst; virtual;
    procedure DeactivateLast; virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    property Activated: Boolean
      read getActivated write setActivated;
    property Text: string
      read getText write setText;
    property Enabled: Boolean
      read getEnabled write setEnabled;
    property ReadOnly: Boolean
      read getReadOnly write setReadOnly;
  end;

(* TSDVMaskEdit *)

  TSDVMaskEdit = class(TCustomSDVMaskEdit)
  published
    (* published declarations *)

    (* TCustomMaskEdit: *)

    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property EditMask;
    property Text;
    property SpaceChar;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

(* TCustomInternalMaskEdit *)

function TCustomInternalMaskEdit.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalMaskEdit.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalMaskEdit.getInternalReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
  // Goal: "InternalReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalReadOnly".
end;

function TCustomInternalMaskEdit.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

procedure TCustomInternalMaskEdit.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalMaskEdit.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalMaskEdit.setInternalReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  // Goal: "InternalReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalReadOnly".
end;

procedure TCustomInternalMaskEdit.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

(* TCustomSDVMaskEdit *)

function TCustomSDVMaskEdit.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVMaskEdit.setActivated(const Value: Boolean);
begin
  if (FActivated <> Value) then
  begin
    FActivated := Value;
    if (Value)
      then ActivateFirst
      else DeActivateLast;
  end;
  // Goal: "Activated" property set method.
  // Objetivo: Metodo escritura para propiedad "Activated".
end;

function TCustomSDVMaskEdit.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVMaskEdit.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVMaskEdit.getReadOnly: Boolean;
begin
  Result := InternalReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVMaskEdit.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

procedure TCustomSDVMaskEdit.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVMaskEdit.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVMaskEdit.setReadOnly(const Value: Boolean);
begin
  InternalReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVMaskEdit.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVMaskEdit.ActivateFirst;
begin
  // Goal: Perform an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVMaskEdit.DeactivateLast;
begin
  // Goal: Perform an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;
end;

end.
