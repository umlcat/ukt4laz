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

unit uktctrls;

interface
uses
  SysUtils, Classes,
{.IFDEF MSWINDOWS}
  Windows,
  //Messages,
  Graphics,
  Controls,
  //StdCtrls,
  Forms,
  ExtCtrls,
  //MaskEds,
{.ENDIF}
  uktactivatedcontrols,
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

const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);

type

{ TCustomInternalControl }

  TCustomInternalControl = class(TCustomControl)
  private
    { Private declarations }

    function getInternalAlign: TAlign;
    function getInternalText: string;
    function getInternalEnabled: Boolean;
    function getInternalVisible: Boolean;
    function getInternalFont: TFont;

    procedure setInternalAlign(const Value: TAlign);
    procedure setInternalText(const Value: string);
    procedure setInternalEnabled(const Value: Boolean);
    procedure setInternalVisible(const Value: Boolean);
    procedure setInternalFont(const Value: TFont);
  protected
    { Protected declarations }

    property InternalAlign: TAlign
      read getInternalAlign write setInternalAlign;
    property InternalText: string
      read getInternalText write setInternalText;
    property InternalEnabled: Boolean
      read getInternalEnabled write setInternalEnabled;
    property InternalVisible: Boolean
      read getInternalVisible write setInternalVisible;
    property InternalFont: TFont
      read getInternalFont write setInternalFont;

    function InternalCanvas: TCanvas;
  public
    { Public declarations }
  end;

{ TCustomSDVControl }

  TCustomSDVControl = class(TCustomInternalControl, ISDVActivatedControl)
  private
    { Private declarations }

    FActivated: Boolean;

    function getActivated: Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    { Protected declarations }

    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;

    function getAlign: TAlign; reintroduce; virtual;
    function getText: string; reintroduce; virtual;
    function getEnabled: Boolean; reintroduce; virtual;
    function getReadOnly: Boolean; reintroduce; virtual;
    function getVisible: Boolean; reintroduce; virtual;
    function getFont: TFont; reintroduce; virtual;

    procedure setAlign(const Value: TAlign); reintroduce; virtual;
    procedure setText(const Value: string); reintroduce; virtual;
    procedure setEnabled(const Value: Boolean); reintroduce; virtual;
    procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
    procedure setVisible(const Value: Boolean); reintroduce; virtual;
    procedure setFont(const Value: TFont); reintroduce; virtual;

    procedure DelegateOnChange;

    procedure ActivateFirst; virtual;
    procedure DeActivateLast; virtual;
    procedure Change; virtual;

    function ParentForm: TCustomForm;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Unpublished declarations }

    property Activated: Boolean
      read getActivated write setActivated;
    property Align: TAlign
      read getAlign write setAlign;
    property Text: string
      read getText write setText;
    property Enabled: Boolean
      read getEnabled write setEnabled;
    property ReadOnly: Boolean
      read getReadOnly write setReadOnly;
    property Font: TFont
      read getFont write setFont;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;
  end;

  function FormByControl((*in/bycopy*) Control: TControl): TCustomForm;
  function FormByComponent((*in/bycopy*) Component: TComponent): TCustomForm;

implementation

function FormByControl((*in/bycopy*) Control: TControl): TCustomForm;
var Found: Boolean;
begin
  Result := nil; Found := false;
  while (Assigned(Control) and (not Found)) do
  begin
    Found := (Control is TCustomForm);

    if (not Found)
      then Control := Control.Parent;
  end;
  if (Found)
    then Result := (Control as TCustomForm);
end;

function FormByComponent((*in/bycopy*) Component: TComponent): TCustomForm;
var Found: Boolean;
begin
  Result := nil; Found := false;
  while (Assigned(Component) and (not Found)) do
  begin
    Found := (Component is TCustomForm);

    if (not Found)
      then Component := Component.Owner;
  end;
  if (Found)
    then Result := (Component as TCustomForm);
end;

{ TCustomInternalControl }

function TCustomInternalControl.getInternalAlign: TAlign;
begin
  Result := inherited Align;
  // Goal: "InternalAlign" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalAlign".
end;

function TCustomInternalControl.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalControl.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalControl.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

function TCustomInternalControl.getInternalFont: TFont;
begin
  Result := inherited Font;
  // Goal: "InternalFont" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalFont".
end;

procedure TCustomInternalControl.setInternalAlign(const Value: TAlign);
begin
  inherited Align := Value;
  // Goal: "InternalAlign" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalAlign".
end;

procedure TCustomInternalControl.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalControl.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalControl.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

procedure TCustomInternalControl.setInternalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  // Goal: "InternalFont" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalFont".
end;

function TCustomInternalControl.InternalCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

{ TCustomSDVControl }

function TCustomSDVControl.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVControl.setActivated(const Value: Boolean);
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

function TCustomSDVControl.getAlign: TAlign;
begin
  Result := InternalAlign;
  // Goal: "Align" property get method.
  // Objetivo: Metodo lectura para propiedad "Align".
end;

function TCustomSDVControl.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVControl.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVControl.getReadOnly: Boolean;
begin
  Result := FReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVControl.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVControl.getFont: TFont;
begin
  Result := InternalFont;
  // Goal: "Font" property get method.
  // Objetivo: Metodo lectura para propiedad "Font".
end;

procedure TCustomSDVControl.setAlign(const Value: TAlign);
begin
  InternalAlign := Value;
  // Goal: "Align" property set method.
  // Objetivo: Metodo escritura para propiedad "Align".
end;

procedure TCustomSDVControl.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVControl.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVControl.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVControl.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVControl.setFont(const Value: TFont);
begin
  InternalFont := Value;
  // Goal: "Font" property set method.
  // Objetivo: Metodo escritura para propiedad "Font".
end;

procedure TCustomSDVControl.DelegateOnChange;
begin
  if (Assigned(FOnChange))
    then FOnChange(Self);
end;

procedure TCustomSDVControl.Change;
begin
  inherited Changed;
  DelegateOnChange;
end;

procedure TCustomSDVControl.ActivateFirst;
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVControl.DeActivateLast;
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

function TCustomSDVControl.ParentForm: TCustomForm;
begin
  Result := FormByControl(Self);
  // Objetivo: Obtener la forma en la que esta contenido este el componente.
  // Goal: Obtain the form where this component is contained.
end;

constructor TCustomSDVControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;

  FOnChange  := nil;
end;

destructor TCustomSDVControl.Destroy;
begin
  FOnChange  := nil;
  FActivated := false;
  inherited;
end;

end.


