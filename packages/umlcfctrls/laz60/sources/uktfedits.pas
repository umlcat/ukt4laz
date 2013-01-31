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
unit uktfedits;

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

(* TCustomSDVFocusEdit *)

  TCustomSDVFocusEdit = class(TCustomSDVEdit, ISDVMessageClient)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FAlignment: TAlignment;
    // Justificar texto
    // Justify text

    FManager: TCustomSDVFocusableControlMngr;
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

    procedure HandlerFocusMngrDisabledColorChanged
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

    function getManager(): TCustomSDVFocusableControlMngr;
    function getEnabled(): Boolean; override;
    function getReadOnly(): Boolean; override;

    procedure setManager(AValue: TCustomSDVFocusableControlMngr);
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

    procedure TryChangeDefaultColor();
    procedure TryChangeDefaultFontColor();
    procedure TryChangeDefaultFontName();
    procedure TryChangeDefaultFontSize();

    procedure TryChangeFocusedColor();
    procedure TryChangeFocusedFontColor();
    procedure TryChangeFocusedFontName();
    procedure TryChangeFocusedFontSize();

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

    (* Never Published declarations *)

    property Alignment: TAlignment
      read FAlignment write setAlignment default taLeftJustify;
    property Manager: TCustomSDVFocusableControlMngr
      read getManager write setManager default NIL;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

(* TSDVFocusEdit *)

  TSDVFocusEdit = class(TCustomSDVFocusEdit)
  published
    (* Published declarations *)

 {.IFDEF MSWINDOWS}
    property BiDiMode;

    {$IFDEF DELPHI}
    property Ctl3D;
    property ImeMode;
    property ImeName;
    {$ENDIF}

    property DragCursor;
    property DragKind;
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
//    property ParentCtl3D;

      {$IFDEF DELPHI}
      property ParentCtl3D;
      property OEMConvert;
      {$ENDIF}

//    property ParentFont;

    property Anchors;
    property AutoSize;
    property BorderStyle;
    property Constraints;
    property DragMode;
    property Enabled;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
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

    property Alignment;
    property Manager;
  end;
  { Goal: To change color when focused ;}
  { Objetivo: Cambiar el color al enfocarse ;}

implementation

(* TCustomSDVFocusEdit *)

procedure TCustomSDVFocusEdit.KeyPress(var Key: Char);
var ANextControl: TWinControl; ValidKey: Boolean; TextLenght: Word;
begin
  inherited KeyPress(Key);
  // Call TEdit.KeyPress(Key);
  // Llamar TEdit.KeyPress(Key);

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

constructor TCustomSDVFocusEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FAlignment := taLeftJustify;
  FManager := NIL;
  // Clean the new properties at start
  // Limpiar las nuevas propiedades al inicio

  FMsgHandlerList := TSDVMessageHandlerList.Create();
  AssignHandlers();
  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVFocusEdit.Destroy();
begin
  FMsgHandlerList.Free();
  FMsgHandlerList := nil;

  Manager := nil;
  // Clean the new properties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy();
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

function TCustomSDVFocusEdit.getManager(): TCustomSDVFocusableControlMngr;
begin
  Result := FManager;
  { Goal: "Manager" property get method .}
  { Objetivo: Metodo lectura propiedad "Manager" .}
end;

function TCustomSDVFocusEdit.getEnabled(): Boolean;
begin
  Result := inherited getEnabled();
  { Goal: "Enabled" property get method .}
  { Objetivo: Metodo lectura propiedad "Enabled" .}
end;

function TCustomSDVFocusEdit.getReadOnly(): Boolean;
begin
  Result := inherited getReadOnly();
  { Goal: "ReadOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ReadOnly" .}
end;

procedure TCustomSDVFocusEdit.setManager(AValue:TCustomSDVFocusableControlMngr);
begin
  if (FManager <> AValue) then
  begin
    // remover cliente de administrador anterior
    // remove client from previous manager
    if (FManager <> nil) then
    begin
      FManager.RemoveClient(Self);
    end;

    FManager := AValue;
    // update manager
    // actualizat administrador

    // insertar cliente en nuevo administrador
    // insert client into new manager
    if (FManager <> nil) then
    begin
      FManager.InsertClient(Self);
    end;
  end;

  TryRefresh();
  { Goal: "Manager" property get method .}
  { Objetivo: Metodo lectura propiedad "Manager" .}
end;

procedure TCustomSDVFocusEdit.setEnabled(const AValue: Boolean);
begin
  inherited setEnabled(AValue);
  if (Manager <> nil) then
  begin
    TryRefresh();
  end;
  { Goal: "Enabled" property get method .}
  { Objetivo: Metodo lectura propiedad "Enabled" .}
end;

procedure TCustomSDVFocusEdit.setReadOnly(const AValue: Boolean);
begin
  inherited setReadOnly(AValue);
  if (Manager <> nil) then
  begin
    TryRefresh();
  end;
  { Goal: "ReadOnly" property get method .}
  { Objetivo: Metodo lectura propiedad "ReadOnly" .}
end;

function TCustomSDVFocusEdit.AsComponent(): TComponent;
begin
  Result := Self;
end;

procedure TCustomSDVFocusEdit.AssignHandlers();
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
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrDisabledColorChanged);
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
    (AMsgID, {$IFDEF FPC}@{$ENDIF}HandlerFocusMngrReadOnlyFontColorChanged);
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

procedure TCustomSDVFocusEdit.AnswerMessage
  (const AMsgRec: TSDVMessageParamsRecord);
var AHandler: TSDVMsgEventHandler;
begin
  AHandler := FMsgHandlerList.HandlerOf(AMsgRec.Message);
  if (AHandler <> nil) then
  begin
    AHandler(AMsgRec);
  end;
end;

procedure TCustomSDVFocusEdit.HandlerServerAssign
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryRefresh();
end;

procedure TCustomSDVFocusEdit.HandlerServerDeassign
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryClear();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrCharCaseChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeCharCase();
  { Goal: To change case of "TEdit" .}
  { Objetivo: Cambiar el caso de "TEdit".}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDefaultColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultColor();
  { Goal: To change background color of control .}
  { Objetivo: Cambiar el color de fondo del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDefaultFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontColor();
  { Goal: To change text color of control .}
  { Objetivo: Cambiar el color de texto del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDefaultFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontName();
  { Goal: To change text color of control .}
  { Objetivo: Cambiar el color de texto del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDefaultFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDefaultFontSize();
  { Goal: To change text color of control .}
  { Objetivo: Cambiar el color de texto del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrFocusedColorChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedColor();
  { Goal: To change focused color of control .}
  { Objetivo: Cambiar el color enfocado del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrFocusedFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontColor();
  { Goal: To change focused font of control .}
  { Objetivo: Cambiar fuente enfocada del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrFocusedFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontName();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrFocusedFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeFocusedFontSize();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDisabledColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledColor();
  { Goal: To change disabled color of control .}
  { Objetivo: Cambiar el color deshabilitado del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDisabledFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontColor();
  { Goal: To change disabled font of control .}
  { Objetivo: Cambiar la fuente deshabilitada del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDisabledFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontName();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrDisabledFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeDisabledFontSize();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrReadOnlyColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyColor();
  { Goal: To change ReadOnly color of control .}
  { Objetivo: Cambiar el color deshabilitado del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrReadOnlyFontColorChanged
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontColor();
  { Goal: To change ReadOnly font of control .}
  { Objetivo: Cambiar la fuente Solo-Lectura del control .}
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrReadOnlyFontNameChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontName();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrReadOnlyFontSizeChanged
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryChangeReadOnlyFontSize();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrClear
  (const AMsgRec: TSDVMessageParamsRecord);
begin
  TryClear();
end;

procedure TCustomSDVFocusEdit.HandlerFocusMngrRefresh
(const AMsgRec: TSDVMessageParamsRecord);
begin
  TryRefresh();
  { Goal: To refresh the same color properties of control .}
  { Objetivo: Refrescar las propiedades con los mismos colores del control .}
end;

procedure TCustomSDVFocusEdit.DoEnter();
begin
//  Self.Name := Self.Name;

  TryRefresh();
  // Update attributes
  // Modificar atributos

  Repaint();
  inherited DoEnter();
end;

procedure TCustomSDVFocusEdit.DoExit();
begin
//  Self.Name := Self.Name;

  TryChangeAutoSelect();
  TryRefresh();
  TryChangeCharCase();
  // Update attributes
  // Modificar atributos

  Repaint();
  inherited DoExit();
end;

procedure TCustomSDVFocusEdit.setAlignment(const AValue: TAlignment);
begin
  if (FAlignment <> AValue) then
  begin
    FAlignment := AValue;
    Repaint();
  end;
  { Goal: To justify the component .}
  { Objetivo: Justificar el componente .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDefaultColor();
begin
  Color := Manager.readGraphicDefaultColor();
  { Goal: Change the enabled color .}
  { Objetivo: Cambiar el color al habilitar .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDefaultFontColor();
begin
  Font.Color := Manager.readGraphicDefaultFontColor();
  { Goal: Change the default font color .}
  { Objetivo: Cambiar el color del tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDefaultFontName();
begin
  Font.Name := Manager.DefaultFontName;
  { Goal: Change the default font color .}
  { Objetivo: Cambiar el color del tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDefaultFontSize();
begin
  Font.Size := Manager.DefaultFontSize;
  { Goal: Change the default font color .}
  { Objetivo: Cambiar el color del tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeFocusedColor();
begin
  Color := Manager.readGraphicFocusedColor();
  { Goal: Change the focused color .}
  { Objetivo: Cambiar el color al enfocar .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeFocusedFontColor();
begin
  Font.Color := Manager.readGraphicFocusedFontColor();
  { Goal: Change the enabled font .}
  { Objetivo: Cambiar el tipofuente habilitado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeFocusedFontName();
begin
  Font.Name := Manager.FocusedFontName;
  { Goal: Change the focused font color .}
  { Objetivo: Cambiar el color del tipofuente enfocado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeFocusedFontSize();
begin
  Font.Size := Manager.FocusedFontSize;
  { Goal: Change the focused font color .}
  { Objetivo: Cambiar el color del tipofuente enfocado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDisabledColor();
begin
  Color := Manager.readGraphicDisabledColor();
  { Goal: Change the disabled color .}
  { Objetivo: Cambiar el color al deshabilitar .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDisabledFontColor();
begin
  Font.Color := Manager.readGraphicDisabledFontColor();
  { Goal: Change the disabled font .}
  { Objetivo: Cambiar el tipofuente deshabilitado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDisabledFontName();
begin
  Font.Name := Manager.DisabledFontName;
  { Goal: Change the disabled font color .}
  { Objetivo: Cambiar el color del tipofuente deshabilitado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeDisabledFontSize();
begin
  Font.Size := Manager.DisabledFontSize;
  { Goal: Change the disabled font color .}
  { Objetivo: Cambiar el color del tipofuente deshabilitado .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeReadOnlyColor();
begin
  Color := Manager.readGraphicReadOnlyColor();
  { Goal: Change the readonly color .}
  { Objetivo: Cambiar el color de sololectura .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeReadOnlyFontColor();
begin
  Font.Color := Manager.readGraphicReadOnlyFontColor();;
  { Goal: Change the readonly font .}
  { Objetivo: Cambiar el tipofuente de sololectura .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeReadOnlyFontName();
begin
  Font.Name := Manager.ReadOnlyFontName;
  { Goal: Change the read-only font color .}
  { Objetivo: Cambiar el color del tipofuente solo-lectura .}
end;

procedure TCustomSDVFocusEdit.ConfirmedChangeReadOnlyFontSize();
begin
  Font.Size := Manager.ReadOnlyFontSize;
  { Goal: Change the read-only font color .}
  { Objetivo: Cambiar el color del tipofuente solo-lectura .}
end;

procedure TCustomSDVFocusEdit.TryChangeDefaultColor();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    ConfirmedChangeDefaultColor();
  end;
  { Goal: Change the non focused color .}
  { Objetivo: Cambiar el color para no enfocado .}
end;

procedure TCustomSDVFocusEdit.TryChangeDefaultFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontColor();
  end;
  { Goal: Change the default font .}
  { Objetivo: Cambiar el tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.TryChangeDefaultFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontName();
  end;
  { Goal: Change the default font .}
  { Objetivo: Cambiar el tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.TryChangeDefaultFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDefaultFontSize();
  end;
  { Goal: Change the default font .}
  { Objetivo: Cambiar el tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.TryChangeFocusedColor();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    ConfirmedChangeFocusedColor();
  end;
  { Goal: Change the focused color .}
  { Objetivo: Cambiar el color para enfocado .}
end;

procedure TCustomSDVFocusEdit.TryChangeFocusedFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeFocusedFontColor();
  end;
  { Goal: Change the focus font .}
  { Objetivo: Cambiar el tipofuente enfocado .}
end;

procedure TCustomSDVFocusEdit.TryChangeFocusedFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeFocusedFontName();
  end;
  { Goal: Change the focused font .}
  { Objetivo: Cambiar el tipofuente por defacto .}
end;

procedure TCustomSDVFocusEdit.TryChangeFocusedFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeFocusedFontSize();
  end;
end;

procedure TCustomSDVFocusEdit.TryChangeDisabledColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledColor();
  end;
  { Goal: Change the Disabled color .}
  { Objetivo: Cambiar el color para enfocado .}
end;

procedure TCustomSDVFocusEdit.TryChangeDisabledFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontColor();
  end;
  { Goal: Change the disabled font .}
  { Objetivo: Cambiar el tipofuente deshabilitado .}
end;

procedure TCustomSDVFocusEdit.TryChangeDisabledFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledFontName();
  end;
end;

procedure TCustomSDVFocusEdit.TryChangeDisabledFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeDisabledFontSize();
  end;
end;

procedure TCustomSDVFocusEdit.TryChangeReadOnlyColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyColor();
  end;
  { Goal: Change the ReadOnly color .}
  { Objetivo: Cambiar el color para SoloLectura .}
end;

procedure TCustomSDVFocusEdit.TryChangeReadOnlyFontColor();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontColor();
  end;
  { Goal: Change the ReadOnly font .}
  { Objetivo: Cambiar el tipofuente para SoloLectura .}
end;

procedure TCustomSDVFocusEdit.TryChangeReadOnlyFontName();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontName();
  end;
end;

procedure TCustomSDVFocusEdit.TryChangeReadOnlyFontSize();
begin
  if ((Manager <> nil) and not Manager.Enabled) then
  begin
    ConfirmedChangeReadOnlyFontSize();
  end;
end;

procedure TCustomSDVFocusEdit.TryChangeAutoSelect();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    AutoSelect := Manager.AutoSelect;
  end;
  { Goal: Update the "AutoSelect" state .}
  { Objetivo: Actualiza el estado de "AutoSelect" .}
end;

procedure TCustomSDVFocusEdit.TryChangeCharCase();
begin
  if ((Manager <> nil) and Manager.Enabled) then
  begin
    CharCase := Manager.CharCase;
  end;
  { Goal: Update the "CharCase" state .}
  { Objetivo: Actualiza el estatus de "CharCase" .}
end;

procedure TCustomSDVFocusEdit.TryClear();
begin
  Self.Font.Size  := DefaultFontSize();
  Self.Font.Name  := DefaultFontName();
  Self.Font.Color := DefaultForegroundColor();
  Self.Color      := DefaultBackgroundColor();
  Self.CharCase   := TEditCharCase.ecNormal;
  Self.AutoSelect := false;
end;

procedure TCustomSDVFocusEdit.TryRefresh();
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

procedure TCustomSDVFocusEdit.Notification
 (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (
    (Operation = opRemove) and
    (FManager <> nil) and
    (AComponent = Manager)
    ) then
  begin
     FManager := nil;
  end;
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

