unit uktbitbtns;

{$mode delphi}

interface

uses
  Classes, SysUtils,
  Graphics,
  Controls, StdCtrls,
  Buttons,
  uktactivatedcontrols,
  dummy;

type

(* TCustomInternalBitBtn *)

  TCustomInternalBitBtn = class(TCustomBitBtn)
  private
    (* Private declarations *)

    function getInternalAlign: TAlign;
    function getInternalCaption: string;
    function getInternalText: string;
    function getInternalEnabled: Boolean;
    function getInternalVisible: Boolean;
    function getInternalFont: TFont;

    procedure setInternalAlign(const Value: TAlign);
    procedure setInternalCaption(const Value: string);
    procedure setInternalText(const Value: string);
    procedure setInternalEnabled(const Value: Boolean);
    procedure setInternalVisible(const Value: Boolean);
    procedure setInternalFont(const Value: TFont);
  protected
    (* Protected declarations *)

    property InternalAlign: TAlign
      read getInternalAlign write setInternalAlign;
    property InternalText: string
      read getInternalText write setInternalText;
    property InternalCaption: string
      read getInternalCaption write setInternalCaption;
    property InternalEnabled: Boolean
      read getInternalEnabled write setInternalEnabled;
    property InternalVisible: Boolean
      read getInternalVisible write setInternalVisible;
    property InternalFont: TFont
      read getInternalFont write setInternalFont;
  public
    (* Public declarations *)
  end;

(* TCustomSDVBitBtn *)

  TCustomSDVBitBtn = class(TCustomInternalBitBtn, ISDVActivatedControl)
  private
    (* Private declarations *)

    FActivated: Boolean;

    function getActivated(): Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    (* Protected declarations *)

    (* properties declarations *)

    FReadOnly: Boolean;
  protected
    (* Protected declarations *)

    (* accesors declarations *)

    function getAlign: TAlign; reintroduce; virtual;
    function getCaption: string; reintroduce; virtual;
    function getText: string; reintroduce; virtual;
    function getEnabled: Boolean; reintroduce; virtual;
    function getReadOnly: Boolean; reintroduce; virtual;
    function getVisible: Boolean; reintroduce; virtual;
    function getFont: TFont; reintroduce; virtual;

    procedure setAlign(const Value: TAlign); reintroduce; virtual;
    procedure setCaption(const Value: string); reintroduce; virtual;
    procedure setText(const Value: string); reintroduce; virtual;
    procedure setEnabled(const Value: Boolean); reintroduce; virtual;
    procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
    procedure setVisible(const Value: Boolean); reintroduce; virtual;
    procedure setFont(const Value: TFont); reintroduce; virtual;
  protected
    (* Protected declarations *)

    procedure ActivateFirst(); virtual;
    procedure DeactivateLast(); virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

(* TSDVBitBtn *)

  TSDVBitBtn = class(TCustomSDVBitBtn)
  published
    (* Published declarations *)

    (* TCustomSDVBitBtn: *)

    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Default;
    property DefaultCaption;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphShowMode;
    property Kind;
    property Layout;
    property Margin;
    property ModalResult;
    property NumGlyphs;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
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
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

implementation

function TCustomInternalBitBtn.getInternalAlign(): TAlign;
begin
  Result := inherited Align;
  // Goal: "InternalAlign" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalAlign".
end;

function TCustomInternalBitBtn.getInternalCaption(): string;
begin
  Result := inherited Caption;
  // Goal: "InternalCaption" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalCaption".
end;

function TCustomInternalBitBtn.getInternalText(): string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalBitBtn.getInternalEnabled(): Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalBitBtn.getInternalVisible(): Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

function TCustomInternalBitBtn.getInternalFont(): TFont;
begin
  Result := inherited Font;
  // Goal: "InternalFont" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalFont".
end;

procedure TCustomInternalBitBtn.setInternalAlign(const Value: TAlign);
begin
  inherited Align := Value;
  // Goal: "InternalAlign" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalAlign".
end;

procedure TCustomInternalBitBtn.setInternalCaption(const Value: string);
begin
  inherited Caption := Value;
  // Goal: "InternalCaption" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalCaption".
end;

procedure TCustomInternalBitBtn.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalBitBtn.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalBitBtn.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

procedure TCustomInternalBitBtn.setInternalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  // Goal: "InternalFont" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalFont".
end;

(* TCustomSDVBitBtn *)

function TCustomSDVBitBtn.getActivated(): Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVBitBtn.setActivated(const Value: Boolean);
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

function TCustomSDVBitBtn.getAlign(): TAlign;
begin
  Result := InternalAlign;
  // Goal: "Align" property get method.
  // Objetivo: Metodo lectura para propiedad "Align".
end;

function TCustomSDVBitBtn.getCaption(): string;
begin
  Result := InternalCaption;
  // Goal: "Caption" property get method.
  // Objetivo: Metodo lectura para propiedad "Caption".
end;

function TCustomSDVBitBtn.getText(): string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVBitBtn.getEnabled(): Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVBitBtn.getReadOnly(): Boolean;
begin
  Result := FReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVBitBtn.getVisible(): Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVBitBtn.getFont(): TFont;
begin
  Result := InternalFont;
  // Goal: "Font" property get method.
  // Objetivo: Metodo lectura para propiedad "Font".
end;

procedure TCustomSDVBitBtn.setAlign(const Value: TAlign);
begin
  InternalAlign := Value;
  // Goal: "Align" property set method.
  // Objetivo: Metodo escritura para propiedad "Align".
end;

procedure TCustomSDVBitBtn.setCaption(const Value: string);
begin
  InternalCaption := Value;
  // Goal: "Caption" property set method.
  // Objetivo: Metodo escritura para propiedad "Caption".
end;

procedure TCustomSDVBitBtn.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVBitBtn.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVBitBtn.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVBitBtn.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVBitBtn.setFont(const Value: TFont);
begin
  InternalFont := Value;
  // Goal: "Font" property set method.
  // Objetivo: Metodo escritura para propiedad "Font".
end;

procedure TCustomSDVBitBtn.ActivateFirst();
begin
  // Goal: Perform an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVBitBtn.DeactivateLast();
begin
  // Goal: Perform an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;
end;

destructor TCustomSDVBitBtn.Destroy();
begin
  FActivated := false;
  inherited;
end;


end.

