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

unit uktforms;

{$mode objfpc}{$H+}

// Objetivo: Los controles "TCustomSDXXX" se proveen con el fin
// de reemplazar algunas propiedades "estaticas" comunes por
// propiedades "dinamicas", ademas de la propiedad "Activated"
// que indica si la forma que contiene el control ha sido
// activada al menos una vez.

// Goal: "TCustomSDXXX" controls are provided in order to
// replace common some "static" properties for "dynamic" properties,
// also includes the "Activated" property that indicates if the
// form has been activated unleast once.

interface

uses
  Classes, SysUtils,
  Controls, ExtCtrls, Graphics, Forms;

type

{ TSDVCustomInternalForm }

  ///<summary>
  ///Makes an alias for some common properties,
  ///and their respective non-virtual getters & setters functions,
  ///</summary>
  TSDVCustomInternalForm = class(TCustomForm)
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

{ TCustomSDVForm }

  ///<summary>
  ///Replaces properties with non-virtual getters & setters functions,
  ///with properties with virtual getters & setters functions.
  ///</summary>
  TCustomSDVForm = class(TSDVCustomInternalForm)
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

    procedure ActivateFirst; dynamic;
    procedure DeActivateLast; dynamic;
    procedure Change; dynamic;
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

implementation

{ TSDVCustomInternalForm }

function TSDVCustomInternalForm.getInternalAlign: TAlign;
begin
  Result := inherited Align;
  // Goal: "InternalAlign" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalAlign".
end;

function TSDVCustomInternalForm.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TSDVCustomInternalForm.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TSDVCustomInternalForm.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

function TSDVCustomInternalForm.getInternalFont: TFont;
begin
  Result := inherited Font;
  // Goal: "InternalFont" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalFont".
end;

procedure TSDVCustomInternalForm.setInternalAlign(const Value: TAlign);
begin
  inherited Align := Value;
  // Goal: "InternalAlign" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalAlign".
end;

procedure TSDVCustomInternalForm.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TSDVCustomInternalForm.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TSDVCustomInternalForm.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

procedure TSDVCustomInternalForm.setInternalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  // Goal: "InternalFont" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalFont".
end;

function TSDVCustomInternalForm.InternalCanvas: TCanvas;
begin
  Result := inherited Canvas;
end;

{ TCustomSDVForm }

function TCustomSDVForm.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVForm.setActivated(const Value: Boolean);
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

function TCustomSDVForm.getAlign: TAlign;
begin
  Result := InternalAlign;
  // Goal: "Align" property get method.
  // Objetivo: Metodo lectura para propiedad "Align".
end;

function TCustomSDVForm.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVForm.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVForm.getReadOnly: Boolean;
begin
  Result := FReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVForm.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVForm.getFont: TFont;
begin
  Result := InternalFont;
  // Goal: "Font" property get method.
  // Objetivo: Metodo lectura para propiedad "Font".
end;

procedure TCustomSDVForm.setAlign(const Value: TAlign);
begin
  InternalAlign := Value;
  // Goal: "Align" property set method.
  // Objetivo: Metodo escritura para propiedad "Align".
end;

procedure TCustomSDVForm.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVForm.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVForm.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVForm.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVForm.setFont(const Value: TFont);
begin
  InternalFont := Value;
  // Goal: "Font" property set method.
  // Objetivo: Metodo escritura para propiedad "Font".
end;

procedure TCustomSDVForm.DelegateOnChange;
begin
  if (Assigned(FOnChange))
    then FOnChange(Self);
end;

procedure TCustomSDVForm.Change;
begin
  inherited Changed;
  DelegateOnChange;
end;

procedure TCustomSDVForm.ActivateFirst;
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVForm.DeActivateLast;
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;

  FOnChange  := nil;
end;

destructor TCustomSDVForm.Destroy;
begin
  FOnChange  := nil;
  FActivated := false;
  inherited;
end;

end.

