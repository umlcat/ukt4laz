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

unit uktcomboboxes;

(*$R-,T-,H+,X+*)

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls, StdCtrls, Forms,
(*.ENDIF*)
  SysUtils, Classes,
  uktKeyConsts, uktDays, uktMonths, uktTimes,
  uktConfigs, uktLangs, uktenumstrs,
  uktactivatedcontrols,
  //uktCtrls;
  dummy;

  // Objetivo: Los controles "TCustomInternalXXX" se proveen con el fin
  // de reemplazar algunas propiedades "estaticas" comunes por
  // propiedades "dinamicas" en classes descendientes.

  // Goal: "TCustomInternalXXX" controls are provided in order to
  // replace common some "static" properties for "dynamic" properties
  // in descendant classes.

const
  DefaultTimeFormat = 'h:mm am/pm';

type

(* TCustomInternalCombobox *)

  TCustomInternalCombobox = class(TCustomCombobox)
  private
    (* Private declarations *)

    function getInternalText: string;
    function getInternalEnabled: Boolean;
    function getInternalReadOnly: Boolean;
    function getInternalVisible: Boolean;
    function getInternalItems: TStrings;

    procedure setInternalText(const Value: string);
    procedure setInternalEnabled(const Value: Boolean);
    procedure setInternalReadOnly(const Value: Boolean);
    procedure setInternalVisible(const Value: Boolean);
    procedure setInternalItems(const Value: TStrings);
  protected
    (* Protected declarations *)

    FReadOnly: Boolean;

    property InternalText: string
      read getInternalText write setInternalText;
    property InternalEnabled: Boolean
      read getInternalEnabled write setInternalEnabled;
    property InternalReadOnly: Boolean
      read getInternalReadOnly write setInternalReadOnly;
    property InternalVisible: Boolean
      read getInternalVisible write setInternalVisible;
    property InternalItems: TStrings
      read getInternalItems write setInternalItems;
  public
    (* Public declarations *)
  end;

(* TCustomSDVCombobox *)

  TCustomSDVCombobox = class(TCustomInternalCombobox, ISDVActivatedControl)
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
    function getItems: TStrings; reintroduce; virtual;

    procedure setText(const Value: string); reintroduce; virtual;
    procedure setEnabled(const Value: Boolean); reintroduce; virtual;
    procedure setReadOnly(const Value: Boolean); reintroduce; virtual;
    procedure setVisible(const Value: Boolean); reintroduce; virtual;
    procedure setItems(const Value: TStrings); reintroduce; virtual;

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

(* TSDVComboStates *)

  TSDVComboStates = (sdcbMouse, sdcbArrowKey, sdcbAnyKey);

(* TCustomSDVPreloadCombobox *)

  TCustomSDVPreloadCombobox = class(TCustomSDVCombobox)
  private
    (* Private declarations *)

    FComboState: TSDVComboStates;
    // Accion que genero un evento "OnChange"
    // Action that generate an "OnChange" event
  protected
    (* Protected declarations *)

    FPreLoad: Boolean;

    function getPreLoad: Boolean; virtual;

    procedure setPreLoad(const Value: Boolean); virtual;

    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure ActivateFirst; override;
    procedure DeactivateLast; override;

    procedure LoadDefaultValue; virtual;
    procedure LoadItems; virtual;
    procedure UnLoadItems; virtual;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Selected: Pointer;
    (*Returns object of selected string*)
    (*Regresa objeto de cadena seleccionada*)

    function Locate: Integer;

    (* Never Published declarations *)

    property ComboState: TSDVComboStates
        read FComboState write FComboState default sdcbMouse;
    property PreLoad: Boolean
      read getPreLoad write setPreLoad;
  end;

(* TCustomSDVEnumCombobox *)

  TCustomSDVEnumCombobox = class(TCustomSDVPreloadCombobox)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FEnumToStrStyle: TEnumToStrStyle;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    property EnumToStrStyle: TEnumToStrStyle
      read FEnumToStrStyle write FEnumToStrStyle;
  end;

(* TCustomSDVDayCombobox *)

  TCustomSDVDayCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FMinDay, FMaxDay, FDay: TDay;
    FDelta: Integer;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getMinDay: TDay;
    function getMaxDay: TDay;
    function getDay: TDay;

    procedure setMinDay(const Value: TDay);
    procedure setMaxDay(const Value: TDay);
    procedure setDay(const Value: TDay);

    function FormatDay(Value: TDay): string;

    procedure LoadItems; override;
    procedure UnLoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property MinDay: TDay
      read getMinDay write setMinDay;
    property MaxDay: TDay
      read getMaxDay write setMaxDay;
    property Day: TDay
      read getDay write setDay;
    property Delta: Integer
      read FDelta write FDelta;
  end;

(* TCustomSDVMonthCombobox *)

  TCustomSDVMonthCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FMinMonth, FMaxMonth, FMonth: TMonth;
    FDelta: Integer;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getMinMonth: TMonth;
    function getMaxMonth: TMonth;
    function getMonth: TMonth;

    procedure setMinMonth(const Value: TMonth);
    procedure setMaxMonth(const Value: TMonth);
    procedure setMonth(const Value: TMonth);

    function FormatMonth(Value: TMonth): string;

    procedure LoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property MinMonth: TMonth
      read getMinMonth write setMinMonth;
    property MaxMonth: TMonth
      read getMaxMonth write setMaxMonth;
    property Month: TMonth
      read getMonth write setMonth;
    property Delta: Integer
      read FDelta write FDelta;
  end;

(* TCustomSDVTimeCombobox *)

  TCustomSDVTimeCombobox = class(TCustomSDVPreloadCombobox)
  private
    (* Private declarations *)

    FMinTime, FMaxTime, FTime: TTime;
    FDelta: Integer;
    // Number of seconds
    // Numero de segundos
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getMinTime: TTime;
    function getMaxTime: TTime;
    function getTime: TTime;

    procedure setMinTime(const Value: TTime);
    procedure setMaxTime(const Value: TTime);
    procedure setTime(const Value: TTime);

    function FormatTime(Value: TTime): string;

    procedure LoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property MinTime: TTime
      read getMinTime write setMinTime;
    property MaxTime: TTime
      read getMaxTime write setMaxTime;
    property Time: TTime
      read getTime write setTime;
    property Delta: Integer
      read FDelta write FDelta;
  end;

(* TCustomSDVLanguageCombobox *)

  TCustomSDVLanguageCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FLanguage: TSDVLanguage;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getLanguage: TSDVLanguage;

    procedure setLanguage(const Value: TSDVLanguage);

    function FormatLanguage(Value: TSDVLanguage): string;

    procedure LoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property Language: TSDVLanguage
      read getLanguage write setLanguage;
  end;

(* TCustomSDVConfigCombobox *)

  TCustomSDVConfigCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FConfig: TConfig;
    FDelta: Integer;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getConfig: TConfig;
    procedure setConfig(const Value: TConfig);

    procedure LoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property Config: TConfig
      read getConfig write setConfig;
    property Delta: Integer
      read FDelta write FDelta;
  end;

(* TSDVCombobox *)

  TSDVCombobox = class(TCustomSDVCombobox)
  published
    (* Published declarations *)

  (*.IFDEF MSWINDOWS*)
      property BiDiMode;
      {$IFDEF DELPHI}
      property Ctl3D;
      {$ENDIF}
      property DragCursor;
      property DragKind;
      {$IFDEF DELPHI}
      property ImeMode;
      property ImeName;
      {$ENDIF}
      property ParentBiDiMode;
      {$IFDEF DELPHI}
      property ParentCtl3D;
      {$ENDIF}

      property OnEndDock;
      property OnStartDock;
  (*.ENDIF*)

      property Style; (*Must be published before Items*)
      property Anchors;
      property Color;
      property Constraints;
      property DragMode;
      property DropDownCount;
      property Enabled;
      property Font;
      property ItemHeight;
  //    property Items;       (*Controlled by class itself*)
      property MaxLength;
      property ParentColor;
      property ParentFont;
      property ParentShowHint;
      property PopupMenu;
      property ShowHint;
      property Sorted;
      property TabOrder;
      property TabStop;
      property Text;
      property Visible;

      property OnChange;
      property OnClick;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnDrawItem;
      property OnDropDown;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMeasureItem;
      property OnStartDrag;
  end;

(* TSDVDayCombobox *)

  TSDVDayCombobox = class(TCustomSDVDayCombobox)
  published
    (* Published declarations *)

(*.IFDEF MSWINDOWS*)
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    property Style; (*Must be published before Items*)
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
//    property Items;       (*Controlled by class itself*)
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;

    (* TCustomSDVPreloadCombobox: *)

    property Delta;

    (* TCustomSDVEnumCombobox: *)

    property EnumToStrStyle;

    (* TCustomSDVDayCombobox: *)

    property MinDay;
    property MaxDay;
    property Day;
  end;

(* TSDVMonthCombobox *)

  TSDVMonthCombobox = class(TCustomSDVMonthCombobox)
  published
    (* Published declarations *)

(*.IFDEF MSWINDOWS*)
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    property Style; (*Must be published before Items*)
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
//    property Items;       (*Controlled by class itself*)
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;

    (* TCustomSDVPreloadCombobox: *)

    property Delta;

    (* TCustomSDVEnumCombobox: *)

    property EnumToStrStyle;

    (* TCustomSDVMonthCombobox: *)

    property MinMonth;
    property MaxMonth;
    property Month;
  end;

(* TSDVTimeCombobox *)

  TSDVTimeCombobox = class(TCustomSDVTimeCombobox)
  published
    (* Published declarations *)

(*.IFDEF MSWINDOWS*)
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

//    property Style; (*Must be published before Items*)
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
//    property Items;       (*Controlled by class itself*)
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;

    (* TCustomSDVPreloadCombobox: *)

    property Delta;

    (* TCustomSDVTimeCombobox: *)

    property MinTime;
    property MaxTime;
    property Time;
  end;

(* TSDVLanguageCombobox *)

  TSDVLanguageCombobox = class(TCustomSDVLanguageCombobox)
  published
    (* Published declarations *)

(*.IFDEF MSWINDOWS*)
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

//    property Style; (*Must be published before Items*)
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
//    property Items;       (*Controlled by class itself*)
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;

    (* TCustomSDVPreloadCombobox: *)

    (* TCustomSDVEnumCombobox: *)

    property EnumToStrStyle;

    (* TCustomSDVLanguageCombobox: *)

    property Language;
  end;

(* TSDVConfigCombobox *)

  TSDVConfigCombobox = class(TCustomSDVConfigCombobox)
  published
    (* Published declarations *)

 (*.IFDEF MSWINDOWS*)
    property BiDiMode;
    {$IFDEF DELPHI}
    property Ctl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ParentBiDiMode;

    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

//    property Style; // must be published before Items
    property Anchors;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
//    property Items;       // controlled by class itself
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;

    (* TCustomSDVPreloadCombobox: *)

    property Delta;

    (* TCustomSDVEnumCombobox: *)

    property EnumToStrStyle;

    (* TCustomSDVConfigCombobox: *)

    property Config;
  end;

implementation

(* TCustomInternalCombobox *)

function TCustomInternalCombobox.getInternalText: string;
begin
  Result := inherited Text;
  // Goal: "InternalText" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalText".
end;

function TCustomInternalCombobox.getInternalEnabled: Boolean;
begin
  Result := inherited Enabled;
  // Goal: "InternalEnabled" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalEnabled".
end;

function TCustomInternalCombobox.getInternalReadOnly: Boolean;
begin
  Result := FReadOnly;
  // Goal: "InternalReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalReadOnly".
end;

function TCustomInternalCombobox.getInternalVisible: Boolean;
begin
  Result := inherited Visible;
  // Goal: "InternalVisible" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalVisible".
end;

function TCustomInternalCombobox.getInternalItems: TStrings;
begin
  Result := inherited Items;
  // Goal: "InternalItems" property get method.
  // Objetivo: Metodo lectura para propiedad "InternalItems".
end;

procedure TCustomInternalCombobox.setInternalText(const Value: string);
begin
  inherited Text := Value;
  // Goal: "InternalText" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalText".
end;

procedure TCustomInternalCombobox.setInternalEnabled(const Value: Boolean);
begin
  inherited Enabled := Value;
  // Goal: "InternalEnabled" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalEnabled".
end;

procedure TCustomInternalCombobox.setInternalReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  // Goal: "InternalReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalReadOnly".
end;

procedure TCustomInternalCombobox.setInternalVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  // Goal: "InternalVisible" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalVisible".
end;

procedure TCustomInternalCombobox.setInternalItems(const Value: TStrings);
begin
  inherited Items := Value;
  // Goal: "InternalItems" property set method.
  // Objetivo: Metodo escritura para propiedad "InternalItems".
end;

(* TCustomSDVCombobox *)

function TCustomSDVCombobox.getActivated: Boolean;
begin
  Result := FActivated;
  // Goal: "Activated" property get method.
  // Objetivo: Metodo lectura para propiedad "Activated".
end;

procedure TCustomSDVCombobox.setActivated(const Value: Boolean);
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

function TCustomSDVCombobox.getText: string;
begin
  Result := InternalText;
  // Goal: "Text" property get method.
  // Objetivo: Metodo lectura para propiedad "Text".
end;

function TCustomSDVCombobox.getEnabled: Boolean;
begin
  Result := InternalEnabled;
  // Goal: "Enabled" property get method.
  // Objetivo: Metodo lectura para propiedad "Enabled".
end;

function TCustomSDVCombobox.getReadOnly: Boolean;
begin
  Result := InternalReadOnly;
  // Goal: "ReadOnly" property get method.
  // Objetivo: Metodo lectura para propiedad "ReadOnly".
end;

function TCustomSDVCombobox.getVisible: Boolean;
begin
  Result := InternalVisible;
  // Goal: "Visible" property get method.
  // Objetivo: Metodo lectura para propiedad "Visible".
end;

function TCustomSDVCombobox.getItems: TStrings;
begin
  Result := InternalItems;
  // Goal: "Items" property get method.
  // Objetivo: Metodo lectura para propiedad "Items".
end;

procedure TCustomSDVCombobox.setText(const Value: string);
begin
  InternalText := Value;
  // Goal: "Text" property set method.
  // Objetivo: Metodo escritura para propiedad "Text".
end;

procedure TCustomSDVCombobox.setEnabled(const Value: Boolean);
begin
  InternalEnabled := Value;
  // Goal: "Enabled" property set method.
  // Objetivo: Metodo escritura para propiedad "Enabled".
end;

procedure TCustomSDVCombobox.setReadOnly(const Value: Boolean);
begin
  InternalReadOnly := Value;
  // Goal: "ReadOnly" property set method.
  // Objetivo: Metodo escritura para propiedad "ReadOnly".
end;

procedure TCustomSDVCombobox.setVisible(const Value: Boolean);
begin
  InternalVisible := Value;
  // Goal: "Visible" property set method.
  // Objetivo: Metodo escritura para propiedad "Visible".
end;

procedure TCustomSDVCombobox.setItems(const Value: TStrings);
begin
  InternalItems := Value;
  // Goal: "Items" property set method.
  // Objetivo: Metodo escritura para propiedad "Items".
end;

procedure TCustomSDVCombobox.ActivateFirst;
begin
  // Goal: Perform an specific action when the control is activated *)
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido *)
  // activado la primera vez.
end;

procedure TCustomSDVCombobox.DeactivateLast;
begin
  // Goal: Perform an specific action when the control is dectivated *)
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido *)
  // deactivado por ultima vez.
end;

constructor TCustomSDVCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivated := false;
end;

(* TCustomSDVPreloadCombobox *)

function TCustomSDVPreloadCombobox.getPreLoad: Boolean;
begin
  Result := FPreLoad;
  // Goal: "PreLoad" property get method.
  // Objetivo: Metodo lectura propiedad "PreLoad".
end;

procedure TCustomSDVPreloadCombobox.setPreLoad(const Value: Boolean);
begin
  FPreLoad := Value;
  // Goal: "PreLoad" property get method.
  // Objetivo: Metodo lectura propiedad "PreLoad".
end;

procedure TCustomSDVPreloadCombobox.Change;
begin
  if (ComboState < sdcbAnyKey)
    then inherited Change;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

procedure TCustomSDVPreloadCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Style = csDropDownList)
    then ComboState := sdcbArrowKey else
  begin
    if ((Key = VK_Down) or (Key = VK_Up))
      then ComboState := sdcbArrowKey
    else ComboState := sdcbAnyKey;
  end;
  // Filter keys that causes selection to change
  // Filtrar teclas que causan que cambie la seleccion

  inherited;
  // Goal:  To detect a pressed key.
  // Objetivo: Detectar una tecla presionada.
end;

procedure TCustomSDVPreloadCombobox.ActivateFirst;
begin
  LoadItems;
  // Goal: Performa an specific action when the control is activated *)
  // by the first time.
  // Objetivo: Realizar una accion especifica cuando el control ha sido *)
  // activado la primera vez.
end;

procedure TCustomSDVPreloadCombobox.DeactivateLast;
begin
  UnLoadItems;
  // Goal: Performa an specific action when the control is dectivated *)
  // by the last time.
  // Objetivo: Realizar una accion especifica cuando el control ha sido *)
  // deactivado por ultima vez.
end;

constructor TCustomSDVPreloadCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreLoad := false;
  FComboState := sdcbMouse;
  Style := csDropDownList;
  // Goal: To prepare the time listbox.
  // Objetivo: Preparar la cajalista para tiempo.
end;

destructor TCustomSDVPreloadCombobox.Destroy;
begin
  FComboState := sdcbMouse;
  FPreLoad := false;
  inherited Destroy;
  // Goal: To prepare the time listbox.
  // Objetivo: Preparar la cajalista para tiempo.
end;

function TCustomSDVPreloadCombobox.Selected: Pointer;
begin
  if ((ItemIndex > -1) and (ItemIndex <= Items.Count))
    then Result := Items.Objects[ItemIndex]
    else Result := nil;
  // Goal: Returns object of selected string.
  // Objetivo: Regresa objeto de cadena seleccionada.
end;

function TCustomSDVPreloadCombobox.Locate: Integer;
var i: Integer; Found: Boolean;
begin
  I := 0; Found  := false;
  while (I <= Items.Count) and not Found do
  begin
    Found  := (Text >= Items[i]);
    Inc(I);
  end;
  Result := I;
  // Objetivo: Buscar el elemento que coincida con el texto.
  // Goal: Locate the item that matches thet text.
end;

procedure TCustomSDVPreloadCombobox.LoadDefaultValue;
begin
//
end;

procedure TCustomSDVPreloadCombobox.LoadItems;
begin
//
end;

procedure TCustomSDVPreloadCombobox.UnLoadItems;
begin
  Items.Clear;
end;

(* TCustomSDVEnumCombobox *)

constructor TCustomSDVEnumCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnumToStrStyle := tsDefault;
  // Goal: To prepare the enumerated listbox.
  // Objetivo: Preparar la cajalista para enumerado.
end;

(* TCustomSDVDayCombobox *)

procedure TCustomSDVDayCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value := Text;
    Day   := StrToDay(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVDayCombobox.getMinDay: TDay;
begin
  Result := FMinDay;
  // Goal: "MinDay" property get method.
  // Objetivo: Metodo lectura propiedad "MinDay".
end;

function TCustomSDVDayCombobox.getMaxDay: TDay;
begin
  Result := FMaxDay;
  // Goal: "MaxDay" property get method.
  // Objetivo: Metodo lectura propiedad "MaxDay".
end;

function TCustomSDVDayCombobox.getDay: TDay;
begin
  Result := FDay;
  // Goal: "Day" property get method.
  // Objetivo: Metodo lectura propiedad "Day".
end;

procedure TCustomSDVDayCombobox.setMinDay(const Value: TDay);
var NewValue: TDay;
begin
  NewValue := SafeDay(Value);
  if (FMinDay <> NewValue)  then
  begin
    FMinDay := NewValue;
    if (FMinDay > FDay)
      then Day := NewValue;
  end;
  // Goal: "MinDay" property get method.
  // Objetivo: Metodo lectura propiedad "MinDay".
end;

procedure TCustomSDVDayCombobox.setMaxDay(const Value: TDay);
var NewValue: TDay;
begin
  NewValue := SafeDay(Value);
  if (FMaxDay <> NewValue)  then
  begin
    FMaxDay := NewValue;
    if (FMaxDay < FDay)
      then Day := NewValue;
  end;
  // Goal: "MaxDay" property get method.
  // Objetivo: Metodo lectura propiedad "MaxDay".
end;

procedure TCustomSDVDayCombobox.setDay(const Value: TDay);
var NewValue: TDay;
begin
  NewValue := SafeDay(Value);
  if (FDay <> NewValue) then
  begin
    FDay := NewValue;
    Text := DayToStr(NewValue);
  end;
  // Goal: "Day" property get method.
  // Objetivo: Metodo lectura propiedad "Day".
end;

constructor TCustomSDVDayCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinDay := Low(TDay);
  FMaxDay := High(TDay);
  FDay    := Low(TDay);
  FDelta  := 1;
//  Text    := FormatDay(FDay);
  // Goal: To prepare the Day listbox.
  // Objetivo: Preparar la cajalista para mes.
end;

function TCustomSDVDayCombobox.FormatDay(Value: TDay): string;
begin
  Result := DayToLongText(Value);
  // Objetivo: formatea un valor "mes" segun propiedades.
end;

procedure TCustomSDVDayCombobox.LoadItems;
var Value: TDay;
begin
  if (Delta > 0) then
  begin
    Value := MinDay;
    while (Value <= MaxDay) do
    begin
      Items.Add(FormatDay(Value));
      Inc(Value, Delta);
    end;
  end;
  ItemIndex := 0;
end;

procedure TCustomSDVDayCombobox.UnLoadItems;
begin
  Items.Clear;
end;

(* TCustomSDVMonthCombobox *)

procedure TCustomSDVMonthCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value := Text;
    Month := StrToMonth(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVMonthCombobox.getMinMonth: TMonth;
begin
  Result := FMinMonth;
  // Goal: "MinMonth" property get method.
  // Objetivo: Metodo lectura propiedad "MinMonth".
end;

function TCustomSDVMonthCombobox.getMaxMonth: TMonth;
begin
  Result := FMaxMonth;
  // Goal: "MaxMonth" property get method.
  // Objetivo: Metodo lectura propiedad "MaxMonth".
end;

function TCustomSDVMonthCombobox.getMonth: TMonth;
begin
  Result := FMonth;
  // Goal: "Month" property get method.
  // Objetivo: Metodo lectura propiedad "Month".
end;

procedure TCustomSDVMonthCombobox.setMinMonth(const Value: TMonth);
var NewValue: TMonth;
begin
  NewValue := SafeMonth(Value);
  if (FMinMonth <> NewValue)  then
  begin
    FMinMonth := NewValue;
    if (FMinMonth > FMonth)
      then Month := NewValue;
  end;
  // Goal: "MinMonth" property get method.
  // Objetivo: Metodo lectura propiedad "MinMonth".
end;

procedure TCustomSDVMonthCombobox.setMaxMonth(const Value: TMonth);
var NewValue: TMonth;
begin
  NewValue := SafeMonth(Value);
  if (FMaxMonth <> NewValue)  then
  begin
    FMaxMonth := NewValue;
    if (FMaxMonth < FMonth)
      then Month := NewValue;
  end;
  // Goal: "MaxMonth" property get method.
  // Objetivo: Metodo lectura propiedad "MaxMonth".
end;

procedure TCustomSDVMonthCombobox.setMonth(const Value: TMonth);
var NewValue: TMonth;
begin
  NewValue := SafeMonth(Value);
  if (FMonth <> NewValue) then
  begin
    FMonth := NewValue;
    Text   := MonthToStr(NewValue);
  end;
  // Goal: "Month" property get method.
  // Objetivo: Metodo lectura propiedad "Month".
end;

constructor TCustomSDVMonthCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinMonth := Low(TMonth);
  FMaxMonth := High(TMonth);
  FMonth    := Low(TMonth);
  FDelta := 1;
//  Text   := FormatMonth(FMonth);
  // Goal: To prepare the Month listbox.
  // Objetivo: Preparar la cajalista para mes.
end;

function TCustomSDVMonthCombobox.FormatMonth(Value: TMonth): string;
begin
  Result := MonthToLongText(Value);
  // Objetivo: formatea un valor "mes" segun propiedades.
end;

procedure TCustomSDVMonthCombobox.LoadItems;
var Value: TMonth;
begin
  if (Delta > 0) then
  begin
    Value := MinMonth;
    while (Value <= MaxMonth) do
    begin
      Items.Add(FormatMonth(Value));
      Inc(Value, Delta);
    end;
  end;
  ItemIndex := 0;
end;

(* TCustomSDVTimeCombobox *)

procedure TCustomSDVTimeCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value := Text;
    Time  := StrToTimeDef(Value, NoTime);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVTimeCombobox.getMinTime: TTime;
begin
  Result := FMinTime;
  // Goal: "MinTime" property get method.
  // Objetivo: Metodo lectura propiedad "MinTime".
end;

function TCustomSDVTimeCombobox.getMaxTime: TTime;
begin
  Result := FMaxTime;
  // Goal: "MaxTime" property get method.
  // Objetivo: Metodo lectura propiedad "MaxTime".
end;

function TCustomSDVTimeCombobox.getTime: TTime;
begin
  Result := FTime;
  // Goal: "Time" property get method.
  // Objetivo: Metodo lectura propiedad "Time".
end;

procedure TCustomSDVTimeCombobox.setMinTime(const Value: TTime);
begin
  FMinTime := Value;
  if (Greater(FMinTime, FTime))
    then Time := Value;
  // Goal: "MinTime" property get method.
  // Objetivo: Metodo lectura propiedad "MinTime".
end;

procedure TCustomSDVTimeCombobox.setMaxTime(const Value: TTime);
begin
  FMaxTime := Value;
  if (Lesser(FMaxTime, FTime))
    then Time := Value;
  // Goal: "MaxTime" property get method.
  // Objetivo: Metodo lectura propiedad "MaxTime".
end;

procedure TCustomSDVTimeCombobox.setTime(const Value: TTime);
begin
  if (Different(FTime, Value)) then
  begin
    FTime := Value;
    Text  := FormatTime(Value);
  end;
  // Goal: "Time" property get method.
  // Objetivo: Metodo lectura propiedad "Time".
end;

constructor TCustomSDVTimeCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FMinTime := SysUtils.Time;
//  FMaxTime := SysUtils.Time;
//  FTime    := SysUtils.Time;
  FDelta := 60;
  Text   := FormatTime(FTime);
  // Goal: To prepare the time listbox.
  // Objetivo: Preparar la cajalista para tiempo.
end;

function TCustomSDVTimeCombobox.FormatTime(Value: TTime): string;
begin
  SysUtils.DateTimeToString(Result, DefaultTimeFormat, TimeToDateTime(Value));
  // Goal: Formats a time value upon properties.
  // Objetivo: formatea un valor tiempo segun propiedades.
end;

procedure TCustomSDVTimeCombobox.LoadItems;
var Value: TTime; CanContinue: Boolean;
begin
  if (Delta > 0) then
  begin
    Value := MinTime; CanContinue := true;
    while LesserEqual(Value, MaxTime) and CanContinue do
    begin
      Items.Add(FormatTime(Value));
      CanContinue := CanIncMin(Value, Delta);
    end;
  end;
  ItemIndex := 0;
  // Goal: To load the items into the combobox.
  // Objetivo: Cargar los elementos de la cajacombo.
end;

(* TCustomSDVLanguageCombobox *)

procedure TCustomSDVLanguageCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value    := Text;
    Language := StrToLang(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVLanguageCombobox.getLanguage: TSDVLanguage;
begin
  Result := FLanguage;
  // Goal: "Language" property get method.
  // Objetivo: Metodo lectura propiedad "Language".
end;

procedure TCustomSDVLanguageCombobox.setLanguage(const Value: TSDVLanguage);
var NewValue: TSDVLanguage;
begin
  NewValue := SafeLang(Value);
  if (FLanguage <> Value) then
  begin
    FLanguage := NewValue;
    Text      := LangToStr(NewValue);
  end;
  // Goal: "Language" property get method.
  // Objetivo: Metodo lectura propiedad "Language".
end;

function TCustomSDVLanguageCombobox.FormatLanguage(Value: TSDVLanguage): string;
begin
  Result := LangToText(Value);
  // Objetivo: formatea un valor "Language" segun propiedades.
end;

procedure TCustomSDVLanguageCombobox.LoadItems;
var I: TSDVLanguage;
begin
  for i := Low(TSDVLanguage) to High(TSDVLanguage) do
    Items.Add(FormatLanguage(i));
  ItemIndex := 0;
end;

constructor TCustomSDVLanguageCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLanguage := Low(TSDVLanguage);
  Text      := FormatLanguage(FLanguage);
  // Goal: To prepare the listbox.
  // Objetivo: Preparar la cajalista.
end;

(* TCustomSDVConfigCombobox *)

procedure TCustomSDVConfigCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value := Text;
    Config := StrToConfig(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVConfigCombobox.getConfig: TConfig;
begin
  Result := FConfig;
  // Goal: "Config" property get method.
  // Objetivo: Metodo lectura propiedad "Config".
end;

procedure TCustomSDVConfigCombobox.setConfig(const Value: TConfig);
var NewValue: TConfig;
begin
  NewValue := SafeConfig(Value);
  if (FConfig <> Value) then
  begin
    FConfig := NewValue;
    Text    := ConfigToStr(NewValue);
  end;
  // Goal: "Config" property get method.
  // Objetivo: Metodo lectura propiedad "Config".
end;

constructor TCustomSDVConfigCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfig := cfgNone;
  FDelta  := 1;
  Text    := ConfigToStr(FConfig);
  // Goal: To prepare the Config listbox.
  // Objetivo: Preparar la cajalista para mes.
end;

procedure TCustomSDVConfigCombobox.LoadItems;
var Value: TConfig;
begin
  if (Delta > 0) then
  begin
    Value := Low(TConfig);
    while (Value <= High(TConfig)) do
    begin
      Items.Add(ConfigToStr(Value));
      Inc(Value, Delta);
    end;
  end;
  ItemIndex := 0;
end;

end.
