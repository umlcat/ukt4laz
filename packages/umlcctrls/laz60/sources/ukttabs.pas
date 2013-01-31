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

unit ukttabs;

interface

uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages, Graphics,
  Controls, Forms,
  ExtCtrls, Buttons,
(*.ENDIF*)
  SysUtils, Classes, Math,
  uktctrls, uktpanels,
  dummy;

(**
 ** This unit defines an auxiliary control,
 ** a button toolbar, each button, its the tab for a page,
 ** of a tab-control or page-control.
 **
 ** This control its used by the "TSDVPanelTabControl".
 **)

const
  NoIndex   = -1;
  InitIndex = 00;

type

(* TOnTabEvent *)

  TCustomSDVTabs = class;
  TOnTabEvent =
    procedure (TabControl: TCustomSDVTabs; TabIndex: Integer) of object;

  TOnMouseMoveEvent     =
    procedure (Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;

(* TCustomSDVTabs *)

  TSDVTabsBitBtn = class;
  TCustomSDVTabs = class(TCustomSDVPanel)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FTabIndex: Integer;
    // boton seleccionado
    // selected button

    FTabs: TStrings;

    FOnEnterTab: TOnTabEvent;
    FOnExitTab:  TOnTabEvent;

    ButtonHeight:     Integer;
    ButtonWidth:      Integer;

    MinBtnSize:       TPoint;

    FFirstIndex: Integer;
    // primer boton visible
    // first visible button

    FOnMouseMove:     TOnMouseMoveEvent;

//    Buttons: array[TButtonOption] of TSDVTabsBitBtn;
    Buttons: TList;

    function getTabIndex: Integer; virtual;
    function getTabs: TStrings; virtual;

    procedure setTabIndex(const Value: Integer); virtual;
    procedure setTabs(const Value: TStrings); virtual;

    procedure DelegateOnEnterTab(TabIndex: Integer); (*static;*)
    procedure DelegateOnExitTab(TabIndex: Integer); (*static;*)

    procedure SetSize(var W: Integer; var H: Integer);

    procedure OnTabsChanged(Sender: TObject);

    procedure OnClickDelegateer(Sender: TObject);
    procedure OnMouseDownDelegateer
     (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMoveDelegateer
     (Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure ClearButtons;
    procedure InitButtons;

    procedure Loaded(); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;

    procedure BtnClick(Index: Integer); dynamic;

    procedure ActivateFirst; override;
    procedure DeactivateLast; override;

    procedure DoOnEnterTab(TabIndex: Integer); dynamic;
    procedure DoOnExitTab(TabIndex: Integer); dynamic;
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    (* public declarations *)

(*.IFDEF MSWINDOWS*)
    property DragCursor;
    property DragKind;

    {$IFDEF DELPHI}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}

    property DockManager;
    property UseDockManager default true;

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;

    property TabIndex: Integer
      read getTabIndex write setTabIndex;
    property Tabs: TStrings
      read getTabs write setTabs;

    property OnMouseMove: TOnMouseMoveEvent
      read FOnMouseMove write FOnMouseMove;

    property OnEnterTab: TOnTabEvent
      read FOnEnterTab write FOnEnterTab;
    property OnExitTab: TOnTabEvent
      read FOnExitTab write FOnExitTab;
  end;

(* TSDVTabsBitBtn *)

  TSDVTabsBitBtn = class(TSpeedButton)
  private
    (* Private declarations *)

    FIndex: Integer;
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    property Index: Integer
      read FIndex write FIndex;
  end;

(* TSDVTabs *)

  TSDVTabs = class(TCustomSDVTabs)
  published
    (* published declarations *)

(*.IFDEF MSWINDOWS*)
    property DragCursor;
    property DragKind;

    {$IFDEF DELPHI}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}

    property DockManager;
    property UseDockManager default true;

    property OnEndDock;
    property OnStartDock;
(*.ENDIF*)

    (* TCustomPanel: *)

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;

    (* TCustomSDPanel: *)

    property Activated;
    property Caption;
    property Text;
    property ReadOnly;
    property Font;

    property OnChange;

    (* TCustomSDVTabs: *)

    property TabIndex;
    property Tabs;

    property OnEnterTab;
    property OnExitTab;
  end;

implementation

(* TCustomSDVTabs *)

function TCustomSDVTabs.getTabIndex: Integer;
begin
  Result := FTabIndex;
end;

function TCustomSDVTabs.getTabs: TStrings;
begin
  Result := FTabs;
end;

procedure TCustomSDVTabs.setTabIndex(const Value: Integer);
begin
  if (Value <> FTabIndex) then
  begin
    FTabIndex := Value;
    BtnClick(FTabIndex);
  end;
end;

procedure TCustomSDVTabs.setTabs(const Value: TStrings);
begin
  if (Value <> FTabs) then
  begin
    FTabs.Assign(Value);
  end;
end;

procedure TCustomSDVTabs.DelegateOnEnterTab(TabIndex: Integer);
begin
  if (Assigned(FOnEnterTab))
    then FOnEnterTab(Self, TabIndex);
  // Goal: To execute the "OnEnterTab" event.
  // Objetivo: Ejecutar el evento "OnEnterTab".
end;

procedure TCustomSDVTabs.DelegateOnExitTab(TabIndex: Integer);
begin
  if (Assigned(FOnExitTab))
    then FOnExitTab(Self, TabIndex);
  // Goal: To execute the "OnExitTab" event.
  // Objetivo: Ejecutar el evento "OnExitTab".
end;

procedure TCustomSDVTabs.DoOnEnterTab(TabIndex: Integer);
var ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if (ParentForm.Active)
    then TSDVTabsBitBtn(Buttons[TabIndex]).Down := true;

  TSDVTabsBitBtn(Buttons[TabIndex]).Down := true;
  DelegateOnEnterTab(TabIndex);
  // Goal: Perform "OnEnterTab" button action.
  // Objetivo: Realizar accion de boton "OnEnterTab".
end;

procedure TCustomSDVTabs.DoOnExitTab(TabIndex: Integer);
begin
  DelegateOnExitTab(TabIndex);
  // Goal: Perform "OnExitTab" button action.
  // Objetivo: Realizar accion de boton "OnExitTab".
end;

procedure TCustomSDVTabs.SetSize(var W: Integer; var H: Integer);
var Count: Integer;

  function CountVisible: Integer;
  var I: Integer;
  begin
    Result := 0;
    for I := 0 to Pred(Buttons.Count) do
    begin
      if (TSDVTabsBitBtn(Buttons[I]).Visible) then
      begin
        Inc(Result);
      end;
    end;

    if (Result = 0)
      then Inc(Result);
    // Goal: To obtain no. of visible buttons.
    // Objetivo: Obtener el no. de botones visibles.
  end;

  procedure SetHorizontal;
  var I: Integer;
     MinW, Space, Temp, Remain: Integer;
     X: Integer;
     EachButton: TSDVTabsBitBtn;
  begin
    MinW := Count * MinBtnSize.X;
    if (W < MinW)
      then W := MinW;
    if (H < MinBtnSize.Y)
      then H := MinBtnSize.Y;

    ButtonWidth := W div Count;
    Temp := Count * ButtonWidth;
    if (Align = alNone)
      then W := Temp;

    X := 0;
    Remain := W - Temp;
    Temp := Count div 2;
    for I := 0 to Pred(Buttons.Count) do
    begin
      EachButton := TSDVTabsBitBtn(Buttons[I]);
      if (EachButton.Visible) then
      begin
        Space := 0;
        if (Remain <> 0) then
        begin
          Dec(Temp, Remain);
          if (Temp < 0) then
          begin
            Inc(Temp, Count);
            Space := 1;
          end;
        end;
        EachButton.SetBounds(X, 0, ButtonWidth + Space, Height);
        Inc(X, ButtonWidth + Space);
      end else EachButton.SetBounds(Width + 1, 0, ButtonWidth, Height);
    end;
    // Objetivo: Asignar la orientacion del controltab.
    // Goal: To set the tabcontrol orientation.
  end;

  procedure SetVertical;
  var I: Integer;
     MinH, Space, Temp, Remain: Integer;
     Y: Integer;
     EachButton: TSDVTabsBitBtn;
  begin
    MinH := Count * MinBtnSize.Y;
    if (H < MinH)
      then H := MinH;
    if (W < MinBtnSize.X)
      then W := MinBtnSize.X;

    ButtonHeight := H div Count;
    Temp := Count * ButtonHeight;
    if (Align = alNone)
      then H := Temp;

    Y := 0;
    Remain := H - Temp;
    Temp := Count div 2;

    for I := 0 to Pred(Buttons.Count) do
    begin
      EachButton := TSDVTabsBitBtn(Buttons[I]);
      if (EachButton.Visible) then
      begin
        Space := 0;
        if (Remain <> 0) then
        begin
          Dec(Temp, Remain);
          if (Temp < 0) then
          begin
            Inc(Temp, Count);
            Space := 1;
          end;
        end;
        EachButton.SetBounds(0, Y, Width, ButtonHeight + Space);
        Inc(Y, ButtonWidth + Space);
      end else EachButton.SetBounds (0, Height + 1, Width, ButtonHeight);
    end;
    // Objetivo: Asignar la orientacion del controltab.
    // Goal: To set the tabcontrol orientation.
  end;

begin
  if (csLoading in ComponentState)
    then Exit;
  if (Buttons.Count = 0)
    then Exit;

  Count := CountVisible;
  if (W > H)
    then SetHorizontal
    else SetVertical;
  // Goal: To set the navigator's size.
  // Objetivo: Asignar el tamano al navegador.
end;

procedure TCustomSDVTabs.OnTabsChanged(Sender: TObject);
begin
  InitButtons;
end;

procedure TCustomSDVTabs.OnClickDelegateer(Sender: TObject);
begin
  BtnClick(TSDVTabsBitBtn(Sender).Index);
  // Goal: Transfer the "OnClick" event
  // from each button to the tabcontrol.
end;

procedure TCustomSDVTabs.OnMouseDownDelegateer
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var PrevIndex: Integer;
begin
  PrevIndex := TabIndex;
  TabIndex  := TSDVTabsBitBtn(Sender).Index;

  if ((TabStop and (GetFocus <> Handle) and CanFocus)) then
  begin
    SetFocus;
    if (GetFocus <> Handle)
      then Exit;
  end else if ((TabStop) and (GetFocus = Handle) and (PrevIndex <> TabIndex)) then
  begin
    TSDVTabsBitBtn(Buttons[PrevIndex]).Invalidate();
    TSDVTabsBitBtn(Buttons[TabIndex]).Invalidate();
  end;
  // Goal: Transfer the "OnMouseDown" event
  // from each button to the tabcontrol.
end;

procedure TCustomSDVTabs.OnMouseMoveDelegateer
 (Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (Assigned(FOnMouseMove))
    then FOnMouseMove(Sender, Shift, X, Y );
  // Goal: Transfer the "OnMouseMove" event
  // from each button to the tabcontrol.
end;

procedure TCustomSDVTabs.ClearButtons;
var I: Integer; EachButton: TSDVTabsBitBtn;
begin
  for I := 0 to Pred(Buttons.Count) do
  begin
    EachButton := TSDVTabsBitBtn(Buttons[i]);
    EachButton.Free;
  end;

  FTabIndex := NoIndex;

  Buttons.Clear;
  // vaciar lista de botones
  // empty buttons* list
end;

procedure TCustomSDVTabs.InitButtons;
var I, CurrentIndex, ButtonLast, ButtonPossibleLast, ButtonRealLast: Integer;
    EachButton: TSDVTabsBitBtn;
    X: Integer;
//    ResName: string;
begin
  MinBtnSize := Point(75, Height);

  ClearButtons;
  // vaciar lista de botones
  // empty buttons* list

  ButtonLast := Pred(Tabs.Count);
  if (ButtonLast > NoIndex) then
  begin
    ButtonPossibleLast := (Width div MinBtnSize.X);
  end else
  begin
    ButtonPossibleLast := 0;
  end;
  ButtonRealLast := Math.Min(ButtonLast, ButtonPossibleLast);

  X := 0;
  // preparar coordenada inicial de los botones
  // prepare buttons* initial coordinate

  CurrentIndex := FFirstIndex;
  for I := 0 to ButtonRealLast do
  begin
    EachButton := TSDVTabsBitBtn.Create(Self);

    EachButton.Index := CurrentIndex;

    EachButton.Visible := true;
    EachButton.Enabled := true;

    EachButton.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);

    EachButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}OnClickDelegateer;
    EachButton.OnMouseDown := {$IFNDEF DELPHI}@{$ENDIF}OnMouseDownDelegateer;
    EachButton.OnMouseMove := {$IFNDEF DELPHI}@{$ENDIF}OnMouseMoveDelegateer;
    (* Since tabconbtrol*s buttons CANNOT be access *)
    (* by the user in design time ,*)
    (* then assign handlers directly by programming ;*)

    EachButton.Parent := Self;
    Buttons.Add(EachButton);
    X := X + MinBtnSize.X;

    EachButton.Caption := FTabs[i];
    EachButton.Flat := true;
    EachButton.GroupIndex := 1;

    Inc(CurrentIndex);
  end;
  // crear cada boton
  // create each button

  Self.ActivateFirst;
  // Objetivo: xxxx.
  // Goal: xxxx.
end;

procedure TCustomSDVTabs.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//
end;

procedure TCustomSDVTabs.Loaded();
var W, H: Integer;
begin
  inherited Loaded();
  W := Width;
  H := Height;
  SetSize(W, H);

  if ((W <> Width) or (H <> Height))
    then inherited SetBounds (Left, Top, W, H);
  // Goal: To read this componente from the resource file.
  // Objetivo: Leer este componente desde el archivo de recursos.
end;

procedure TCustomSDVTabs.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    (*Your Notification Code...*)
  end;
end;

procedure TCustomSDVTabs.BtnClick(Index: Integer);
begin
  if (not (csDesigning in ComponentState)) then
  begin
    DoOnExitTab(FTabIndex);
    DoOnEnterTab(Index);
  end;
  // Goal: Do standard "click" action for each button.
  // Objetivo: Realizar accion estandar "click" para cada boton.
end;

procedure TCustomSDVTabs.ActivateFirst;
begin
  if (Buttons.Count > 0) then
  begin
    FTabIndex := InitIndex;
    DoOnEnterTab(FTabIndex);
  end;
  // Goal: Performa an specific action when the control is activated
  // by the first time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // activado la primera vez.
end;

procedure TCustomSDVTabs.DeactivateLast;
begin
  if (FTabIndex > NoIndex)
    then DoOnExitTab(FTabIndex);
  // Goal: Performa an specific action when the control is dectivated
  // by the last time.

  // Objetivo: Realizar una accion especifica cuando el control ha sido
  // deactivado por ultima vez.
end;

constructor TCustomSDVTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if (not NewStyleControls)
    then ControlStyle := ControlStyle + [csFramed];

  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width  := 241;
  Height := 21;
  Color := clGray;

  ButtonHeight := 0;
  ButtonWidth  := 0;

  FTabIndex := NoIndex;
  // boton seleccionado
  // selected button

  FTabs := TStringList.Create;
  TStringList(FTabs).OnChange := {$IFNDEF DELPHI}@{$ENDIF}OnTabsChanged;

  FFirstIndex := 0;
  // primer boton visible
  // first visible button

  Buttons := TList.Create;
  // preparar lista de referencia rapida de botones visibles
  // prepare visible buttons* quick reference list

  InitButtons;

  FullRepaint := false;
end;

destructor TCustomSDVTabs.Destroy;
begin
  Buttons.Free;
  FTabs.Free;
  inherited Destroy;
end;

(* TSDVTabsBitBtn *)

end.
