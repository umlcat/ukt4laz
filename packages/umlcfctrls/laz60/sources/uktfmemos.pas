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
unit uktfmemos;

interface
uses
{.IFDEF MSWINDOWS}
  Controls, Forms,
  Graphics, StdCtrls,
{.ENDIF}
  SysUtils, Classes,
  uktKeyConsts,
  uktcomponents,
  uktguidstrs,
  uktmsgtypes, uktmsgctrls,
  //uktMsgLists,
{.IFDEF MSWINDOWS}
  uktfmngrs,
  uktedits, uktmemos, uktmaskeds,
{.ENDIF}
  dummy;

type

(* TCustomSDVFocusMemo *)

  TCustomSDVFocusMemo = class(TCustomSDVMemo, ISDVMessageClient)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FAlignment: TAlignment;
    // Justificar texto
    // Justify text
    FManager: TCustomSDVFocusMngr;
    FMsgHandlerList : TSDVMessageHandlerList;
  protected
    (* Protected declarations *)

    (* Message Handlers declarations *)

    procedure HandlerServerAssign
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerServerDeassign
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure HandlerFocusMngrCharCaseChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure HandlerFocusMngrDefaultColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDefaultFontColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDefaultFontNameChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDefaultFontSizeChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure HandlerFocusMngrFocusedColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrFocusedFontColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrFocusedFontNameChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrFocusedFontSizeChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure FocusedDisabledColorChangedHandler
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDisabledFontColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDisabledFontNameChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrDisabledFontSizeChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure HandlerFocusMngrReadOnlyColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrReadOnlyFontColorChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrReadOnlyFontNameChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrReadOnlyFontSizeChanged
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure HandlerFocusMngrClear
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
    procedure HandlerFocusMngrRefresh
      (const AMsgRec: TSDVMessageParamsRecord); virtual;
  protected
    (* Protected declarations *)

    procedure DoEnter(); override;
    procedure DoExit(); override;

    function getManager(): TCustomSDVFocusMngr;
    function getEnabled(): Boolean; override;
    function getReadOnly(): Boolean; override;

    procedure setManager(AValue: TCustomSDVFocusMngr);
    procedure setEnabled(const AValue: Boolean); override;
    procedure setReadOnly(const AValue: Boolean); override;

    function AsComponent(): TComponent;

    procedure AssignHandlers(); virtual;

    procedure AnswerMessage
      (const AMsgRec: TSDVMessageParamsRecord); virtual;

    procedure setAlignment(const AValue: TAlignment); virtual;

    procedure ConfirmedChangeDefaultColor();
    procedure ConfirmedChangeDefaultFontColor();
    procedure ConfirmedChangeDefaultFontName();
    procedure ConfirmedChangeDefaultFontSize();

    procedure ConfirmedChangeFocusedColor();
    procedure ConfirmedChangeFocusedFontColor();
    procedure ConfirmedChangeFocusedFontName();
    procedure ConfirmedChangeFocusedFontSize();

    procedure ConfirmedChangeDisabledColor();
    procedure ConfirmedChangeDisabledFontColor();
    procedure ConfirmedChangeDisabledFontName();
    procedure ConfirmedChangeDisabledFontSize();

    procedure ConfirmedChangeReadOnlyColor();
    procedure ConfirmedChangeReadOnlyFontColor();
    procedure ConfirmedChangeReadOnlyFontName();
    procedure ConfirmedChangeReadOnlyFontSize();

    procedure TryChangeFocusedColor();
    procedure TryChangeFocusedFontColor();
    procedure TryChangeFocusedFontName();
    procedure TryChangeFocusedFontSize();

    procedure TryChangeDefaultColor();
    procedure TryChangeDefaultFontColor();
    procedure TryChangeDefaultFontName();
    procedure TryChangeDefaultFontSize();

    procedure TryChangeDisabledColor();
    procedure TryChangeDisabledFontColor();
    procedure TryChangeDisabledFontName();
    procedure TryChangeDisabledFontSize();

    procedure TryChangeReadOnlyColor();
    procedure TryChangeReadOnlyFontColor();
    procedure TryChangeReadOnlyFontName();
    procedure TryChangeReadOnlyFontSize();

    procedure TryChangeAutoSelect();
    procedure TryChangeCharCase();

    procedure TryClear();
    procedure TryRefresh();

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    (* Public declarations *)

    procedure KeyPress(var Key: Char); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* Never Published declarations *)

    property Alignment: TAlignment
        read FAlignment write setAlignment default taLeftJustify;
    property Manager: TCustomSDVFocusMngr
        read getManager write setManager default NIL;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

(* TSDVFocusMemo *)

  TSDVFocusMemo = class(TCustomSDVFocusMemo)
  published
    (* Published declarations *)

 {.IFDEF MSWINDOWS}
    property AutoSize;
    property BiDiMode;

    {$IFDEF DELPHI}
    property BevelEdges;
    property BevelInner;
    property BevelKind // default bkNone;
    property BevelOuter;
    property Ctl3D;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    {$ENDIF}

    property DragCursor;
    property DragKind;
    property HideSelection;
    property ParentBiDiMode;
    property PasswordChar;

    property OnEndDock;
    property OnStartDock;
{.ENDIF}

//    property AutoSelect;
//    property CharCase;
//    property Color;
//    property Font;
//    property ParentColor;
//    property ParentFont;

    {$IFDEF DELPHI}
    property ParentCtl3D;
    {$ENDIF}

    property Align;
    property Alignment;
    property Anchors;
    property BorderStyle;
    property Constraints;
    property DragMode;
    property Enabled;
    property Lines;
    property MaxLength;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
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
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    (* TCustomSDVFocusMemo: *)

    property Manager;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

implementation

(* TCustomSDVFocusMemo *)

procedure TCustomSDVFocusMemo.KeyPress(var Key: Char);
var ANextControl: TWinControl; ValidKey: Boolean; TextLenght: Word;
begin
  inherited KeyPress(Key);
  // Call TMemo.KeyPress(Key);
  // Llamar TMemo.KeyPress(Key);

  ValidKey := Word(Key) >= 32;  //Sólo cáracteres
  if (ValidKey)
    then TextLenght := Length(Text + Key) - SelLength
    else TextLenght := 0;

  //verificar si la tecla presionada es la tecla definida como "exitkey"
  if ((Manager <> nil) and Manager.AutoForward and (Key = Char(VK_ENTER))) or
     ((Manager <> nil) and Manager.AutoTab and (TextLenght = MaxLength) and
     (MaxLength > 0) and ValidKey) then
  begin
    // obtener el siguiente componente de la lista de "taborder"

    ANextControl := NextControl(GetParentForm(Self as TControl) as TWinControl,
      Self as TWinControl,TRUE,TRUE,FALSE);
    // si la lista no esta vacia ir al siguiente componente

    if (ANextControl <> nil) then
    begin
      ANextControl.SetFocus();
    end;
  end
  { Goal: To focus the next control when pressing the "ExitKey" .}
  { Objetivo: Enfocar el siguiente control al presionar la tecla "ExitKey" }
end;

constructor TCustomSDVFocusMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FAlignment := taLeftJustify;
  FManager := NIL;
  // Clean the new propierties at start
  // Limpiar las nuevas propiedades al inicio

  FMsgHandlerList := TSDVMessageHandlerList.Create();
  AssignHandlers();
  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVFocusMemo.Destroy();
begin
  FMsgHandlerList.Free();
  FMsgHandlerList := nil;

  Manager := nil;
  // Clean the new propierties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy();
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

function TCustomSDVFocusMemo.getManager(): TCustomSDVFocusMngr;
begin
  Result := FManager;
  { Goal: "Manager" property get method .}
  { Objetivo: Metodo lectura propiedad "Manager" .}
end;

function TCustomSDVFocusMemo.getEnabled(): Boolean;
begin
  Result := inherited getEnabled;
  { Goal: "Enabled" property get method .}
  { Objetivo: Metodo lectura propiedad "Enabled" .}
end;

function TCustomSDVFocusMemo.getReadOnly(): Boolean;
begin
  Result := inherited getReadOnly;
  { Goal: "ReadOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ReadOnly" .}
end;

procedure TCustomSDVFocusMemo.setManager(AValue:TCustomSDVFocusMngr);
begin
  if (FManager <> AValue) then
  begin
    if (FManager <> nil)
      then FManager.RemoveClient(Self);
    // remover cliente de administrador anterior
    // remove client from previous manager

    FManager := AValue;
    // update manager
    // actualizat administrador

    if (FManager <> nil)
      then FManager.InsertClient(Self);
    // insertar cliente en nuevo administrador
    // insert client into new manager
  end;
  { Goal: "Manager" property get method .}
  { Objetivo: Metodo lectura propiedad "Manager" .}
end;

procedure TCustomSDVFocusMemo.setEnabled(const AValue: Boolean);
begin
  inherited setEnabled(AValue);
  if (Manager <> nil) then
  begin
    if (Enabled) then
    begin
      ConfirmedChangeDefaultColor();
      ConfirmedChangeDefaultFontColor();
    end else
    begin
      ConfirmedChangeDisabledColor();
      ConfirmedChangeDisabledFontColor();
    end;
  end;
  { Goal: "Enabled" property get method .}
  { Objetivo: Metodo lectura propiedad "Enabled" .}
end;

procedure TCustomSDVFocusMemo.setReadOnly(const AValue: Boolean);
begin
  inherited setEnabled(AValue);
  if (Manager <> nil) then
  begin
    if (ReadOnly) then
    begin
      ConfirmedChangeReadOnlyColor();
      ConfirmedChangeReadOnlyFontColor();
    end else
    begin
      ConfirmedChangeDefaultColor();
      ConfirmedChangeDefaultFontColor();
    end;
  end;
  { Goal: "ReadOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ReadOnly" .}
end;

function TCustomSDVFocusMemo.AsComponent(): TComponent;
begin
  Result := Self;
end;

procedure TCustomSDVFocusMemo.AssignHandlers();
var AMsgID: TGUID;
begin
  uktguidstrs.DoubleStrToGUID
    (msgServerAssign, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerServerAssign);

  uktguidstrs.DoubleStrToGUID
    (msgServerDeassign, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerServerDeassign);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrCharCaseChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrCharCaseChanged);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDefaultColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultFontColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDefaultFontColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultFontNameChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDefaultFontNameChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultFontSizeChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDefaultFontSizeChanged);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrFocusedColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedFontColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrFocusedFontColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedFontNameChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrFocusedFontNameChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedFontSizeChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrFocusedFontSizeChanged);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}FocusedDisabledColorChangedHandler);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledFontColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDisabledFontColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledFontNameChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDisabledFontNameChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledFontSizeChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDisabledFontSizeChanged);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrReadOnlyColorChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyFontColorChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrRefresh);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyFontNameChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrReadOnlyFontNameChanged);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyFontSizeChanged, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrReadOnlyFontSizeChanged);

  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrClear, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrClear);
  uktguidstrs.DoubleStrToGUID
    (msgFocusMngrRefresh, AMsgID);
  FMsgHandlerList.Insert
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrRefresh);
end;

procedure TCustomSDVFocusMemo.AnswerMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var AHandler: TSDVMsgEventHandler;
begin
  AHandler := FMsgHandlerList.HandlerOf(AMsgRec.Message);
  if (AHandler <> nil) then
  begin
    AHandler(AMsgRec);
  end;
end;

procedure TCustomSDVFocusMemo.HandlerServerAssign
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryRefresh();
end;

procedure TCustomSDVFocusMemo.HandlerServerDeassign
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryClear();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrCharCaseChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeCharCase();
  { Goal: To change case of "TMemo" .}
  { Objetivo: Cambiar el caso de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDefaultColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultColor();
  { Goal: To change case of "TMemo" .}
  { Objetivo: Cambiar el caso de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDefaultFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontColor();
  { Goal: To change case of "TMemo" .}
  { Objetivo: Cambiar el caso de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDefaultFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontName();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDefaultFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontSize();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrFocusedColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedColor();
  { Goal: To change focused color of "TMemo" .}
  { Objetivo: Cambiar el color enfocado de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrFocusedFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontColor();
  { Goal: To change focused font of "TMemo" .}
  { Objetivo: Cambiar fuente enfocada de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrFocusedFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontName();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrFocusedFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontSize();
end;

procedure TCustomSDVFocusMemo.FocusedDisabledColorChangedHandler
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledColor();
  { Goal: To change disabled color of "TMemo" .}
  { Objetivo: Cambiar el color deshabilitado de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDisabledFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontColor();
  { Goal: To change disabled font of "TMemo" .}
  { Objetivo: Cambiar la fuente deshabilitada de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDisabledFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontName();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrDisabledFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontSize();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrReadOnlyColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyColor();
  { Goal: To change ReadOnly color of "TMemo" .}
  { Objetivo: Cambiar el color deshabilitado de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrReadOnlyFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontColor();
  { Goal: To change ReadOnly font of "TMemo" .}
  { Objetivo: Cambiar la fuente deshabilitada de "TMemo".}
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrReadOnlyFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontName();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrReadOnlyFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontSize();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrClear
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryClear();
end;

procedure TCustomSDVFocusMemo.HandlerFocusMngrRefresh
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryRefresh();
  { Goal: To refresh the same color properties of "TEdit" .}
  { Objetivo: Refrescar las propiedades con los mismos colores de "TEdit".}
end;

procedure TCustomSDVFocusMemo.DoEnter();
begin
  TryRefresh();
  // Update attributes
  // Modificar atributos

  Repaint();
  inherited DoEnter();
end;

procedure TCustomSDVFocusMemo.DoExit;
begin
  TryChangeAutoSelect();
  TryRefresh();
  TryChangeCharCase();
  // Update attributes
  // Modificar atributos

  Repaint();
  inherited DoExit();
end;

procedure TCustomSDVFocusMemo.setAlignment(const AValue: TAlignment);
begin
  if (FAlignment <> AValue) then
  begin
    FAlignment := AValue;
    Repaint();
  end;
  { Goal: To justify the component .}
  { Objetivo: Justificar el componente .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDefaultColor();
begin
  Color := Manager.DefaultColor;
  Font.Color := Manager.DefaultFontColor;
  { Goal: Change the enabled color .}
  { Objetivo: Cambiar el color al habilitar .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDefaultFontColor();
begin
  Font.Color := Manager.DefaultFontColor;
  { Goal: Change the enabled font .}
  { Objetivo: Cambiar el tipofuente habilitado .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDefaultFontName();
begin
  Font.Name := Manager.DefaultFontName;
  { Goal: Change the default font color .}
  { Objetivo: Cambiar el color del tipofuente por defacto .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDefaultFontSize();
begin
  Font.Size := Manager.DefaultFontSize;
  { Goal: Change the default font color .}
  { Objetivo: Cambiar el color del tipofuente por defacto .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeFocusedColor();
begin
  Color := Manager.FocusedColor;
  { Goal: Change the focused color .}
  { Objetivo: Cambiar el color al enfocar .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeFocusedFontColor();
begin
  Font.Color := Manager.FocusedFontColor;
  { Goal: Change the enabled font .}
  { Objetivo: Cambiar el tipofuente habilitado .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeFocusedFontName();
begin
  Font.Name := Manager.FocusedFontName;
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeFocusedFontSize();
begin
  Font.Size := Manager.FocusedFontSize;
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDisabledColor();
begin
  Color := Manager.DisabledColor;
  { Goal: Change the disabled color .}
  { Objetivo: Cambiar el color al deshabilitar .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDisabledFontColor();
begin
  Font.Color := Manager.DisabledFontColor;
  { Goal: Change the disabled font .}
  { Objetivo: Cambiar el tipofuente deshabilitado .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDisabledFontName();
begin
  Font.Name := Manager.DisabledFontName;
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeDisabledFontSize();
begin
  Font.Size := Manager.DisabledFontSize;
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeReadOnlyColor();
begin
  Color := Manager.ReadOnlyColor;
  { Goal: Change the readonly color .}
  { Objetivo: Cambiar el color de sololectura .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeReadOnlyFontColor();
begin
  Font.Color := Manager.ReadOnlyFontColor;
  { Goal: Change the readonly font .}
  { Objetivo: Cambiar el tipofuente de sololectura .}
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeReadOnlyFontName();
begin
  Font.Name := Manager.ReadOnlyFontName;
end;

procedure TCustomSDVFocusMemo.ConfirmedChangeReadOnlyFontSize();
begin
  Font.Size := Manager.ReadOnlyFontSize;
end;

procedure TCustomSDVFocusMemo.TryChangeFocusedColor();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    ConfirmedChangeFocusedColor();
  end;
  { Goal: Change the focused color .}
  { Objetivo: Cambiar el color para enfocado .}
end;

procedure TCustomSDVFocusMemo.TryChangeFocusedFontColor();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    ConfirmedChangeFocusedFontColor();
  end;
  { Goal: Change the focused font .}
  { Objetivo: Cambiar el tipofuente para enfocado .}
end;

procedure TCustomSDVFocusMemo.TryChangeFocusedFontName();
begin
  Font.Name := Manager.FocusedFontName;
end;

procedure TCustomSDVFocusMemo.TryChangeFocusedFontSize();
begin
  Font.Size := Manager.FocusedFontSize;
end;

procedure TCustomSDVFocusMemo.TryChangeDefaultColor();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    ConfirmedChangeDefaultColor();
  end;
  { Goal: Change the non focused color .}
  { Objetivo: Cambiar el color para no enfocado .}
end;

procedure TCustomSDVFocusMemo.TryChangeDefaultFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontColor();
  end;
  { Goal: Change the enabled font .}
  { Objetivo: Cambiar el tipofuente habilitado .}
end;

procedure TCustomSDVFocusMemo.TryChangeDefaultFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontName();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeDefaultFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontSize();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeDisabledColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledColor();
  end;
  { Goal: Change the Disabled color .}
  { Objetivo: Cambiar el color para enfocado .}
end;

procedure TCustomSDVFocusMemo.TryChangeDisabledFontColor;
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledFontColor();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeDisabledFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledFontName();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeDisabledFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledFontSize();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeReadOnlyColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyColor();
  end;
  { Goal: Change the ReadOnly color .}
  { Objetivo: Cambiar el color para SoloLectura .}
end;

procedure TCustomSDVFocusMemo.TryChangeReadOnlyFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontColor();
  end;
  { Goal: Change the ReadOnly font .}
  { Objetivo: Cambiar el tipofuente para SoloLectura .}
end;

procedure TCustomSDVFocusMemo.TryChangeReadOnlyFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontName();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeReadOnlyFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontSize();
  end;
end;

procedure TCustomSDVFocusMemo.TryChangeAutoSelect();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    AutoSelect := Manager.AutoSelect;
  end;
  { Goal: Update the "AutoSelect" state .}
  { Objetivo: Actualiza el estado de "AutoSelect" .}
end;

procedure TCustomSDVFocusMemo.TryChangeCharCase();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    CharCase := Manager.CharCase;
  end;
  { Goal: Update the "CharCase" state .}
  { Objetivo: Actualiza el estado de "CharCase" .}
end;

procedure TCustomSDVFocusMemo.TryClear;
begin
  Self.Font.Size  := DefaultFontSize();
  Self.Font.Name  := DefaultFontName();
  Self.Font.Color := DefaultForegroundColor();
  Self.Color      := DefaultBackgroundColor();
  Self.CharCase   := TEditCharCase.ecNormal;
  Self.AutoSelect := false;
end;

procedure TCustomSDVFocusMemo.TryRefresh();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    if (Self.Enabled) then
    begin
      if (Self.ReadOnly) then
      begin
        ConfirmedChangeReadOnlyColor();
        ConfirmedChangeReadOnlyFontColor();
      end else
      begin
        if (Self.Focused) then
        begin
          ConfirmedChangeFocusedColor();
          ConfirmedChangeFocusedFontColor();
        end else
        begin
          ConfirmedChangeDefaultColor();
          ConfirmedChangeDefaultFontColor();
        end;
      end;
    end else
    begin
      ConfirmedChangeDisabledColor();
      ConfirmedChangeDisabledFontColor();
    end;
  end;

  TryChangeAutoSelect();
  TryChangeCharCase();
  { Goal: Update properties .}
  { Objetivo: Actualizar propiedades .}
end;

procedure TCustomSDVFocusMemo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FManager <> nil) and (AComponent = Manager)
     then FManager := nil;
  { Goal: To detect & update when associated components ,}
  { have been removed .}

  { Objetivo: Detectar y actualizar cuando ,}
  { los componentes asociados se han removido .}
end;

procedure Prepare();
begin
  // ...
end;

procedure UnPrepare();
begin
  // ...
end;

initialization
  Prepare();
finalization
  Unprepare();
end.

