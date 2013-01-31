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

unit uktpanels;

interface
uses
  SysUtils, Classes,
(*.IFDEF MSWINDOWS*)
  Windows,
  //Messages,
  Graphics,
  Controls,
  //StdCtrls,
  Forms,
  ExtCtrls,
  //Mask,
(*.ENDIF*)
  uktactivatedcontrols,
  dummy;

(**
 ** Description:
 ** This unit declares controls,
 ** that allows some common properties,
 ** to have its accesorts replaced with vitual accesors.
 **)

type

(* TCustomInternalPanel *)

  TCustomInternalPanel = class(TCustomPanel)
  private
    (* Private declarations *)

    function getInternalAlign(): TAlign;
    function getInternalCaption(): string;
    function getInternalText(): string;
    function getInternalEnabled(): Boolean;
    function getInternalVisible(): Boolean;
    function getInternalFont(): TFont;

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

    function InternalCanvas(): TCanvas;
  public
    (* Public declarations *)
  end;

(* TCustomSDVPanel *)

  TCustomSDVPanel = class(TCustomInternalPanel, ISDVActivatedControl)
  private
    (* Private declarations *)

    FActivated: Boolean;

    function getActivated(): Boolean;
    procedure setActivated(const Value: Boolean);
  protected
    (* Protected declarations *)

    (* properties declarations *)

    FShowGrid: Boolean;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
  protected
    (* Protected declarations *)

    (* accesors declarations *)

    function getAlign(): TAlign; reintroduce; virtual;
    function getCaption(): string; reintroduce; virtual;
    function getText(): string; reintroduce; virtual;
    function getEnabled(): Boolean; reintroduce; virtual;
    function getReadOnly(): Boolean; reintroduce; virtual;
    function getVisible(): Boolean; reintroduce; virtual;
    function getFont(): TFont; reintroduce; virtual;

    procedure setAlign(const Value: TAlign); reintroduce; virtual;
    procedure setCaption(const Value: string); reintroduce; virtual;
    procedure setText(const Value: string); reintroduce; virtual;
    procedure setEnabled(const Value: Boolean); reintroduce; virtual;
    procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
    procedure setVisible(const Value: Boolean); reintroduce; virtual;
    procedure setFont(const Value: TFont); reintroduce; virtual;
  protected
    (* Protected declarations *)

    procedure DelegateOnChange();

    procedure ActivateFirst(); virtual;
    procedure DeactivateLast(); virtual;

    procedure Change(); virtual;

    procedure PaintGrid();
    procedure PaintWindow(DC: HDC); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* Public declarations *)

    procedure ClearControls(); virtual;
  public
    (* Public declarations *)

(*.IFDEF MSWINDOWS*)
    property DockManager;
    property UseDockManager default TRUE;
(*.ENDIF*)

    property BevelInner;
    property BevelOuter;
    property BevelWidth;

    property Activated: Boolean
      read getActivated write setActivated;
    property Align: TAlign
      read getAlign write setAlign;
    property Caption: string
      read getCaption write setCaption;
    property Color;
    property Text: string
      read getText write setText;
    property Enabled: Boolean
      read getEnabled write setEnabled;
    property ReadOnly: Boolean
      read getReadOnly write setReadOnly;
    property Font: TFont
      read getFont write setFont;
    property ShowGrid: Boolean
      read FShowGrid write FShowGrid;

    property OnChange: TNotifyEvent
      read FOnChange write FOnChange;
  end;

(* TSDVPanel *)

  TSDVPanel = class(TCustomSDVPanel)
  published
    (* Published declarations *)

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

    //property DockManager;

    (* TCustomSDVPanel: *)

    property Activated;
    property ReadOnly;

    property OnChange;
  end;

implementation

(* TCustomInternalPanel *)

function TCustomInternalPanel.getInternalAlign(): TAlign;
begin
  Result := inherited Align;
  // Goal: "InternalAlign" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalAlign".
end;

function TCustomInternalPanel.getInternalCaption(): string;
begin
  Result := inherited Caption;
  // Goal: "InternalCaption" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalCaption".
end;

function TCustomInternalPanel.getInternalText(): string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalPanel.getInternalEnabled(): Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalPanel.getInternalVisible(): Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

function TCustomInternalPanel.getInternalFont(): TFont;
begin
  Result := inherited Font;
  // Goal: "InternalFont" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalFont".
end;

procedure TCustomInternalPanel.setInternalAlign(const Value: TAlign);
begin
  inherited Align := Value;
  // Goal: "InternalAlign" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalAlign".
end;

procedure TCustomInternalPanel.setInternalCaption(const Value: string);
begin
  inherited Caption := Value;
  // Goal: "InternalCaption" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalCaption".
end;

procedure TCustomInternalPanel.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalPanel.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalPanel.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

procedure TCustomInternalPanel.setInternalFont(const Value: TFont);
begin
  inherited Font.Assign(Value);
  // Goal: "InternalFont" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalFont".
end;

function TCustomInternalPanel.InternalCanvas(): TCanvas;
begin
  Result := inherited Canvas;
end;

(* TCustomSDVPanel *)

function TCustomSDVPanel.getActivated(): Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVPanel.setActivated(const Value: Boolean);
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

function TCustomSDVPanel.getAlign(): TAlign;
begin
  Result := InternalAlign;
  // Goal: "Align" property get method.
  // Objetivo: Metodo lectura para propiedad "Align".
end;

function TCustomSDVPanel.getCaption(): string;
begin
  Result := InternalCaption;
  // Goal: "Caption" property get method.
  // Objetivo: Metodo lectura para propiedad "Caption".
end;

function TCustomSDVPanel.getText(): string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVPanel.getEnabled(): Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVPanel.getReadOnly(): Boolean;
begin
  Result := FReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVPanel.getVisible(): Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVPanel.getFont(): TFont;
begin
  Result := InternalFont;
  // Goal: "Font" property get method.
  // Objetivo: Metodo lectura para propiedad "Font".
end;

procedure TCustomSDVPanel.setAlign(const Value: TAlign);
begin
  InternalAlign := Value;
  // Goal: "Align" property set method.
  // Objetivo: Metodo escritura para propiedad "Align".
end;

procedure TCustomSDVPanel.setCaption(const Value: string);
begin
  InternalCaption := Value;
  // Goal: "Caption" property set method.
  // Objetivo: Metodo escritura para propiedad "Caption".
end;

procedure TCustomSDVPanel.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVPanel.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVPanel.setReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVPanel.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVPanel.setFont(const Value: TFont);
begin
  InternalFont := Value;
  // Goal: "Font" property set method.
  // Objetivo: Metodo escritura para propiedad "Font".
end;

procedure TCustomSDVPanel.DelegateOnChange();
begin
  if (Assigned(FOnChange))
    then FOnChange(Self);
end;

procedure TCustomSDVPanel.ActivateFirst();
begin
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVPanel.DeactivateLast();
begin
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

procedure TCustomSDVPanel.Change();
begin
  inherited Changed();
  DelegateOnChange();
end;

procedure TCustomSDVPanel.PaintGrid();
const FGridSpace = 8;
var lBitmap: TBitmap;
  liLeft: Integer;
  liTop: Integer;
  liWidth: Integer;
  liHeight: Integer;
begin
  lBitmap := TBitmap.Create();
  try
    lBitmap.Height := Height;
    lBitmap.Width  := Width;
    lBitmap.Canvas.Pen.Color   := clBlack;
    lBitmap.Canvas.Brush.Color := clGray;

    liTop := 0;
    liHeight := Height;
    liWidth  := Width;

    while (liTop <= liHeight) do
    begin
      liLeft := 0;

      while (liLeft <= liWidth) do
      begin
        lBitmap.Canvas.MoveTo(liLeft, liTop);
        lBitmap.Canvas.LineTo(liLeft, liTop + 1);

        Inc(liLeft, FGridSpace);
      end; // draw dots left to right

      Inc(liTop, FGridSpace);
    end; // draw dots top to bottom

    Canvas.CopyRect(Canvas.ClipRect, lBitmap.Canvas, Canvas.ClipRect);
  finally
    lBitmap.Free();
  end;
end;

procedure TCustomSDVPanel.PaintWindow(DC: HDC);
begin
  if (FShowGrid) then
  begin
    Canvas.Lock();
    try
      Canvas.Handle := DC;
      try
        PaintGrid();
//        Paint();
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock();
    end;
  end else inherited PaintWindow(DC);
end;

constructor TCustomSDVPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;
//  FShowGrid  := true;
  FShowGrid  := false;
end;

destructor TCustomSDVPanel.Destroy;
begin
  FActivated := false;
  inherited;
end;

procedure TCustomSDVPanel.ClearControls();
var EachIndex, LastIndex: Integer; EachControl: TControl;
begin
  LastIndex := (ControlCount - 1);
  for EachIndex := LastIndex downto 0 do
  begin
    EachControl := Controls[EachIndex];
    Self.RemoveControl(EachControl);
  end;
end;


end.
