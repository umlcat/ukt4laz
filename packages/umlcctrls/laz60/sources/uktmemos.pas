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

unit uktmemos;

interface
uses
{.IFDEF MSWINDOWS}
  //Windows,
  //Messages,
  Graphics,
  Controls, 
  StdCtrls, 
  Forms,
{.ENDIF}
  SysUtils, Classes,
  uktactivatedcontrols,
  //vcluktCtrls;
  dummy;

type

{ TCustomInternalMemo }

  TCustomInternalMemo = class(TCustomMemo)
  private
    { Private declarations }

    function getInternalText: string;
    function getInternalEnabled: Boolean;
    function getInternalReadOnly: Boolean;
    function getInternalVisible: Boolean;

    procedure setInternalText(const Value: string);
    procedure setInternalEnabled(const Value: Boolean);
    procedure setInternalReadOnly(const Value: Boolean);
    procedure setInternalVisible(const Value: Boolean);
  protected
    { Protected declarations }

    property InternalText: string
      read getInternalText write setInternalText;
    property InternalEnabled: Boolean
      read getInternalEnabled write setInternalEnabled;
    property InternalReadOnly: Boolean
      read getInternalReadOnly write setInternalReadOnly;
    property InternalVisible: Boolean
      read getInternalVisible write setInternalVisible;
  public
    { Public declarations }
  end;

{ TCustomSDVMemo }

  TCustomSDVMemo = class(TCustomInternalMemo, ISDVActivatedControl)
  private
    { Private declarations }

    FActivated: Boolean;

    function getActivated: Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    { Protected declarations }

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
    { Public declarations }

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

{ TSDVMemo }

  TSDVMemo = class(TCustomSDVMemo)
  published
    (* published declarations *)

    (* TCustomMemo: *)

    property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
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
    property HideSelection;
    property Lines;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
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
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDrag;
    property OnUTF8KeyPress;

    (* TCustomSDVMemo: *)
  end;

implementation

{ TCustomInternalMemo }

function TCustomInternalMemo.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalMemo.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalMemo.getInternalReadOnly: Boolean;
begin
  Result := inherited ReadOnly;
  // Goal: "InternalReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalReadOnly".
end;

function TCustomInternalMemo.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

procedure TCustomInternalMemo.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalMemo.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalMemo.setInternalReadOnly(const Value: Boolean);
begin
  inherited ReadOnly := Value;
  // Goal: "InternalReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalReadOnly".
end;

procedure TCustomInternalMemo.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

{ TCustomSDVMemo }

function TCustomSDVMemo.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVMemo.setActivated(const Value: Boolean);
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

function TCustomSDVMemo.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVMemo.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVMemo.getReadOnly: Boolean;
begin
  Result := InternalReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVMemo.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

procedure TCustomSDVMemo.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVMemo.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVMemo.setReadOnly(const Value: Boolean);
begin
  InternalReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVMemo.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVMemo.ActivateFirst;
begin
  // Goal: Perform an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVMemo.DeactivateLast;
begin
  // Goal: Perform an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;
end;

end.
