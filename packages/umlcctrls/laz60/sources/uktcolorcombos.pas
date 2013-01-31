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

unit uktcolorcombos;

interface
uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages,
  Graphics, Controls, StdCtrls, Forms,
(*.ENDIF*)
  SysUtils, Classes,
  //uktDates, uktTimes, uktDays, uktMonths, uktConfigs, uktLangs,
  uktdoscolors, uktwin16colors,
  uktcomponents,
  uktctrls,
  uktcomboboxes,
  dummy;

type

(* TCustomSDVDOSColorCombobox *)

  TCustomSDVDOSColorCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FMinDOSColor, FMaxDOSColor, FDOSColor: doscolor;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getMinDOSColor: doscolor;
    function getMaxDOSColor: doscolor;
    function getDOSColor: doscolor;

    procedure setMinDOSColor(const Value: doscolor);
    procedure setMaxDOSColor(const Value: doscolor);
    procedure setDOSColor(const Value: doscolor);

    function FormatDOSColor(Value: doscolor): string;

    procedure LoadItems(); override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property MinDOSColor: doscolor
      read getMinDOSColor write setMinDOSColor;
    property MaxDOSColor: doscolor
      read getMaxDOSColor write setMaxDOSColor;
    property DOSColor: doscolor
      read getDOSColor write setDOSColor;
  end;

(* TCustomSDVWin16ColorCombobox *)

  TCustomSDVWin16ColorCombobox = class(TCustomSDVEnumCombobox)
  private
    (* Private declarations *)

    FMinWin16Color, FMaxWin16Color, FWin16Color: Win16Color;
  protected
    (* Protected declarations *)

    procedure Change; override;

    function getMinWin16Color: Win16Color;
    function getMaxWin16Color: Win16Color;
    function getWin16Color: Win16Color;

    procedure setMinWin16Color(const Value: Win16Color);
    procedure setMaxWin16Color(const Value: Win16Color);
    procedure setWin16Color(const Value: Win16Color);

    function FormatWin16Color(Value: Win16Color): string;

    procedure LoadItems; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;

    (* UnPublished declarations *)

    property MinWin16Color: Win16Color
      read getMinWin16Color write setMinWin16Color;
    property MaxWin16Color: Win16Color
      read getMaxWin16Color write setMaxWin16Color;
    property Win16Color: Win16Color
      read getWin16Color write setWin16Color;
  end;

(* TSDVDOSColorCombobox *)

  TSDVDOSColorCombobox = class(TCustomSDVDOSColorCombobox)
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

    (* TCustomSDVEnumCombobox: *)

    property EnumToStrStyle;
    
    (* TCustomSDVDOSColorCombobox: *)

    property MinDOSColor;
    property MaxDOSColor;
    property DOSColor;
  end;

(* TSDVWin16ColorCombobox *)

  TSDVWin16ColorCombobox = class(TCustomSDVWin16ColorCombobox)
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
//    property Items;     // controlled by class itself
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

    (* TCustomSDVWin16ColorCombobox: *)

    property MinWin16Color;
    property MaxWin16Color;
    property Win16Color;
  end;

implementation

(* TCustomSDVDOSColorCombobox *)

procedure TCustomSDVDOSColorCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value    := Text;
    DOSColor := StrToDOSColor(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVDOSColorCombobox.getMinDOSColor: doscolor;
begin
  Result := FMinDOSColor;
  // Goal: "MinDOSColor" property get method.
  // Objetivo: Metodo lectura propiedad "MinDOSColor".
end;

function TCustomSDVDOSColorCombobox.getMaxDOSColor: doscolor;
begin
  Result := FMaxDOSColor;
  // Goal: "MaxDOSColor" property get method.
  // Objetivo: Metodo lectura propiedad "MaxDOSColor".
end;

function TCustomSDVDOSColorCombobox.getDOSColor: doscolor;
begin
  Result := FDOSColor;
  // Goal: "Color" property get method.
  // Objetivo: Metodo lectura propiedad "Color".
end;

procedure TCustomSDVDOSColorCombobox.setMinDOSColor(const Value: doscolor);
begin
  FMinDOSColor := Value;
  if (FMinDOSColor > FDOSColor)
    then DOSColor := Value;
  // Goal: "MinDOSColor" property get method.
  // Objetivo: Metodo lectura propiedad "MinDOSColor".
end;

procedure TCustomSDVDOSColorCombobox.setMaxDOSColor(const Value: doscolor);
begin
  FMaxDOSColor := Value;
  if (FMaxDOSColor < FDOSColor)
    then DOSColor := Value;
  // Goal: "MaxDOSColor" property get method.
  // Objetivo: Metodo lectura propiedad "MaxDOSColor".
end;

procedure TCustomSDVDOSColorCombobox.setDOSColor(const Value: doscolor);
begin
  if (FDOSColor <> Value) then
  begin
    FDOSColor := Value;
    Text      := DOSColorToStr(Value);
  end;
  // Goal: "DOSColor" property get method.
  // Objetivo: Metodo lectura propiedad "DOSColor".
end;

constructor TCustomSDVDOSColorCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinDOSColor := Low(doscolor);
  FMaxDOSColor := High(doscolor);
  FDOSColor    := Low(doscolor);
  Text         := FormatDOSColor(FDOSColor);
  // Goal: To prepare the listbox.
  // Objetivo: Preparar la cajalista.
end;

function TCustomSDVDOSColorCombobox.FormatDOSColor(Value: doscolor): string;
begin
  Result := DOSColorToText(Value);
  // Objetivo: formatea un valor "mes" segun propiedades.
end;

procedure TCustomSDVDOSColorCombobox.LoadItems();
var Value: uktdoscolors.doscolor;
begin
  Value := MinDOSColor;
  while (Value <= MaxDOSColor) do
  begin
    Items.Add(FormatDOSColor(Value));
    Inc(Value);
  end;
  ItemIndex := 0;
end;

(* TCustomSDVWin16ColorCombobox *)

procedure TCustomSDVWin16ColorCombobox.Change;
var Value: string;
begin
  if (ComboState < sdcbAnyKey) then
  begin
    Value    := Text;
    Win16Color := StrToWin16Color(Value);
    inherited Change;
  end;
  // Objetivo: Activar el manejador del evento "OnChange".
  // Goal: Activate the "OnChange" event handler.
end;

function TCustomSDVWin16ColorCombobox.getMinWin16Color: Win16Color;
begin
  Result := FMinWin16Color;
  // Goal: "MinWin16Color" property get method.
  // Objetivo: Metodo lectura propiedad "MinWin16Color".
end;

function TCustomSDVWin16ColorCombobox.getMaxWin16Color: Win16Color;
begin
  Result := FMaxWin16Color;
  // Goal: "MaxWin16Color" property get method.
  // Objetivo: Metodo lectura propiedad "MaxWin16Color".
end;

function TCustomSDVWin16ColorCombobox.getWin16Color: Win16Color;
begin
  Result := FWin16Color;
  // Goal: "Color" property get method.
  // Objetivo: Metodo lectura propiedad "Color".
end;

procedure TCustomSDVWin16ColorCombobox.setMinWin16Color(const Value: Win16Color);
begin
  FMinWin16Color := Value;
  if (FMinWin16Color > FWin16Color)
    then Win16Color := Value;
  // Goal: "MinWin16Color" property get method.
  // Objetivo: Metodo lectura propiedad "MinWin16Color".
end;

procedure TCustomSDVWin16ColorCombobox.setMaxWin16Color(const Value: Win16Color);
begin
  FMaxWin16Color := Value;
  if (FMaxWin16Color < FWin16Color)
    then Win16Color := Value;
  // Goal: "MaxWin16Color" property get method.
  // Objetivo: Metodo lectura propiedad "MaxWin16Color".
end;

procedure TCustomSDVWin16ColorCombobox.setWin16Color(const Value: Win16Color);
begin
  if (FWin16Color <> Value) then
  begin
    FWin16Color := Value;
    Text      := Win16ColorToStr(Value);
  end;
  // Goal: "Win16Color" property get method.
  // Objetivo: Metodo lectura propiedad "Win16Color".
end;

constructor TCustomSDVWin16ColorCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinWin16Color := Low(Win16Color);
  FMaxWin16Color := High(Win16Color);
  FWin16Color    := Low(Win16Color);
  Text         := FormatWin16Color(FWin16Color);
  // Goal: To prepare the listbox.
  // Objetivo: Preparar la cajalista.
end;

function TCustomSDVWin16ColorCombobox.FormatWin16Color(Value: Win16Color): string;
begin
  Result := Win16ColorToText(Value);
  // Objetivo: formatea un valor "mes" segun propiedades.
end;

procedure TCustomSDVWin16ColorCombobox.LoadItems;
var Value: uktwin16colors.Win16Color;
begin
  Value := MinWin16Color;
  while (Value <= MaxWin16Color) do
  begin
    Items.Add(FormatWin16Color(Value));
    Inc(Value);
  end;
  ItemIndex := 0;
end;


end.
