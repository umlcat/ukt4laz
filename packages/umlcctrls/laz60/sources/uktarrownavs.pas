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

unit uktarrownavs;

interface
uses
SysUtils, Classes, Types,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics,
{$ENDIF}
  Controls, Forms,
  ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  uktkeyconsts,
  uktpanels,
  uktarrownavoptions,
  uktarrownavstrs,
  dummy;

const
  InitRepeatPause = 400;  // pause before repeat timer (ms) 
  RepeatPause     = 100;  // pause before hint window displays (ms)
  SpaceSize       = 5;    // size of space between special buttons

(* TSDVNavigatorStyle *)

type
  TSDVNavigatorStyle  = (* enum of *) (nsAllowTimer, nsFocusRect);
  TSDVNavigatorStyles = set of TSDVNavigatorStyle;

type
  // Event Handler's types for Event Handler's properties; 
  TSDVNavigatorOnClickEvent  =
    procedure (Sender: TObject; Button: TSDVArrowButtonOption) of object;
  TSDVOnClickButtonEvent   =
    procedure (Sender: TObject) of object;
  TSDVOnRefreshScrollEvent =
    procedure (Sender: TObject) of object;
  TSDVOnMouseMoveEvent     =
    procedure (Sender: TObject; Shift: TShiftState; X, Y: Integer) of object;

(* TCustomSDVArrowNavigator *)

  TSDVArrowNavigatorBitBtn = class;

  TCustomSDVArrowNavigator = class(TCustomSDVPanel)
  private
    (* Private declarations *) 

    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    (* Protected declarations *)

    FButtonsVisible: TSDVButtonOptions; // Buttons that will appear; 
    FButtonsEnabled: TSDVButtonOptions; // Buttons that will appear & be enabled; 

    FHints:           TStrings;  // Hints loaded from resource file; 
    FDefaultHints:    TStrings;  // Hints loaded from design time IDE; 

    FCaptions:        TStrings;  // Captions loaded from resource file; 
    FDefaultCaptions: TStrings;  // Captions loaded from design time IDE; 

    FShowCaptions:    Boolean;   // Show Captions field; 
    FInitCaptions:    Boolean;   // Show Captions field at create;

    FDelta: Integer;
    // cuantos elementos el navegador salta con
    // los botones "FastPrior" y "FastNext"

    // how many items the navigator skips with
    // the "FastPrior" & "FastNext" buttons
  protected
    (* Protected declarations *)

    ButtonHeight:     Integer;
    ButtonWidth:      Integer;

    MinBtnSize:       TPoint;
    FocusedButton:    TSDVArrowButtonOption;
  protected
    (* Protected declarations *)

    FOnMouseMove:     TSDVOnMouseMoveEvent;
    FOnRefreshScroll: TSDVOnRefreshScrollEvent;
  protected
    (* Protected declarations *)

    function getButtonsVisible(): TSDVButtonOptions;
    function getButtonsEnabled(): TSDVButtonOptions;
    function getCaptions(): TStrings;
    function getHints(): TStrings;
    function getDelta(): Integer;

    procedure setButtonsVisible(const AValue: TSDVButtonOptions);
    procedure setButtonsEnabled(const AValue: TSDVButtonOptions);
    procedure setCaptions(AValue: TStrings);
    procedure setHints(AValue: TStrings);
    procedure setDelta(const AValue: Integer);
    procedure setShowCaptions(AValue: Boolean);
    procedure setEnabled(const AValue: Boolean); override;
  protected
    (* Protected declarations *)

    procedure SetSize(var W, H: Integer);

    procedure OnClickDelegateer(Sender: TObject);
    procedure OnMouseDownDelegateer (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMoveDelegateer(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure OnHintsChanged(Sender: TObject);
    procedure OnCaptionsChanged(Sender: TObject);

    procedure LoadDefaultHints();
    procedure LoadDefaultCaptions();

    procedure LoadUserHints();
    procedure LoadUserCaptions();
    procedure HideCaptions();

    procedure InitHints();
    procedure InitCaptions();

    procedure InitImages(IsVertical: Boolean);
    procedure InitButtons();

    procedure Loaded(); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    procedure ClickFirst();        virtual;
    procedure ClickFastPrior();    virtual;
    procedure ClickPrior();        virtual;
    procedure ClickSearch();       virtual;
    procedure ClickMoveToRoot();   virtual;
    procedure ClickMoveToParent(); virtual;
    procedure ClickNext();         virtual;
    procedure ClickFastNext();     virtual;
    procedure ClickLast();         virtual;
  public
    (* Public declarations *) 

    Buttons: array[TSDVArrowButtonOption] of TSDVArrowNavigatorBitBtn;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TSDVArrowButtonOption); virtual;
  public
    (* Public declarations *)

    property ButtonsVisible: TSDVButtonOptions
       read getButtonsVisible write setButtonsVisible;
    property ButtonsEnabled: TSDVButtonOptions
       read getButtonsEnabled write setButtonsEnabled;
  public
    (* Public declarations *)

{$IFDEF MSWINDOWS}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
{$ENDIF}

    property Align;
    property Anchors;
    property Constraints;
    property DragMode;
    property Enabled;

    property Delta: Integer
      read getDelta write setDelta;
    property Hints: TStrings
      read getHints write setHints;
    property Captions: TStrings
      read getCaptions write setCaptions;

    property ParentShowHint;
    property PopupMenu;

    property ShowCaptions: Boolean
       read FShowCaptions write setShowCaptions default false;
    property ShowHint;

    property TabOrder;
    property TabStop;
    property Visible;

    property OnMouseMove: TSDVOnMouseMoveEvent
      read FOnMouseMove write FOnMouseMove;
    property OnRefreshScroll: TSDVOnRefreshScrollEvent
      read FOnRefreshScroll write FOnRefreshScroll;

    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnResize;
    property OnStartDrag;
  end;

(* TSDVArrowNavigatorBitBtn *)

  TSDVArrowNavigatorBitBtn = class(TBitBtn)
  private
    (* Private declarations *) 

    FIndex: TSDVArrowButtonOption;
    FNavStyle: TSDVNavigatorStyles;
    FRepeatTimer: TTimer;
  protected
    (* Protected declarations *)

    procedure TimerExpired(Sender: TObject);
  protected
    (* Protected declarations *) 

    procedure MouseDown
      (Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp
      (Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    (* Public declarations *) 

    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* Property declarations *)

    property NavStyle: TSDVNavigatorStyles
      read FNavStyle write FNavStyle;
    property Index: TSDVArrowButtonOption
      read FIndex write FIndex;
  end;

(* TCustomSDVArrowDelegateNavigator *)

  TCustomSDVDelegateArrowNavigator = class(TCustomSDVArrowNavigator)
  private
    (* Private declarations *) 

    FOnClickFirst:        TSDVOnClickButtonEvent;
    FOnClickFastPrior:    TSDVOnClickButtonEvent;
    FOnClickPrior:        TSDVOnClickButtonEvent;
    FOnClickSearch:       TSDVOnClickButtonEvent;
    FOnClickMoveToRoot:   TSDVOnClickButtonEvent;
    FOnClickMoveToParent: TSDVOnClickButtonEvent;
    FOnClickNext:         TSDVOnClickButtonEvent;
    FOnClickFastNext:     TSDVOnClickButtonEvent;
    FOnClickLast:         TSDVOnClickButtonEvent;
  protected
    (* Protected declarations *) 

    procedure DelegateClickFirst();        (* static; *)
    procedure DelegateClickFastPrior();    (* static; *)
    procedure DelegateClickPrior();        (* static; *)
    procedure DelegateClickSearch();       (* static; *)
    procedure DelegateClickMoveToRoot();   (* static; *)
    procedure DelegateClickMoveToParent(); (* static; *)
    procedure DelegateClickNext();         (* static; *)
    procedure DelegateClickFastNext();     (* static; *)
    procedure DelegateClickLast();         (* static; *)
  protected
    (* Protected declarations *)

    procedure ClickFirst();        override;
    procedure ClickPrior();        override;
    procedure ClickFastPrior();    override;
    procedure ClickSearch();       override;
    procedure ClickMoveToRoot();   override;
    procedure ClickMoveToParent(); override;
    procedure ClickNext();         override;
    procedure ClickFastNext();     override;
    procedure ClickLast();         override;
  public
    (* Public declarations *)

    (* Event-Handler declarations *)

    property OnClickFirst: TSDVOnClickButtonEvent
      read FOnClickFirst write FOnClickFirst;
    property OnClickPrior: TSDVOnClickButtonEvent
      read FOnClickPrior write FOnClickPrior;
    property OnClickFastPrior: TSDVOnClickButtonEvent
      read FOnClickFastPrior write FOnClickFastPrior;
    property OnClickSearch: TSDVOnClickButtonEvent
      read FOnClickSearch write FOnClickSearch;
    property OnClickMoveToRoot: TSDVOnClickButtonEvent
      read FOnClickMoveToRoot write FOnClickMoveToRoot;
    property OnClickMoveToParent: TSDVOnClickButtonEvent
      read FOnClickMoveToParent write FOnClickMoveToParent;
    property OnClickNext: TSDVOnClickButtonEvent
      read FOnClickNext write FOnClickNext;
    property OnClickFastNext: TSDVOnClickButtonEvent
      read FOnClickFastNext write FOnClickFastNext;
    property OnClickLast: TSDVOnClickButtonEvent
      read FOnClickLast write FOnClickLast;
  end;

(* TSDVDelegateArrowNavigator *)

  TSDVDelegateArrowNavigator = class(TCustomSDVDelegateArrowNavigator)
  published
    (* Published declarations *) 

{$IFDEF MSWINDOWS}
    property DragCursor;
    property DragKind;
    {$IFDEF DELPHI}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}

    property OnEndDock;
    property OnStartDock;
{$ENDIF}

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

    (* TCustomSDVArrowNavigator: *)

    //property ButtonsVisible;
    //property ButtonsEnabled;

    property Captions;
    property Delta;
    property Hints;

    property ShowCaptions;
    property ShowHint;

    property OnMouseMove;
    property OnRefreshScroll;

    (* TCustomSDVDelegateArrowNavigator: *)

    property OnClickFirst;
    property OnClickPrior;
    property OnClickFastPrior;
    property OnClickSearch;

    property OnClickMoveToRoot;
    property OnClickMoveToParent;
    property OnClickNext;
    property OnClickFastNext;
    property OnClickLast;
  end;

implementation

type
  BitBtnNamesType = array[TSDVArrowButtonOption] of pchar;
var
  BitBtnNames: BitBtnNamesType =
  (
   'NONE',
   'FIRST',
   'FASTPRIOR',
   'PRIOR',
   'SEARCH',
   'ROOT',
   'PARENT',
   'NEXT',
   'FASTNEXT',
   'LAST'
  );

(* TCustomSDVArrowNavigator  *)

procedure TCustomSDVArrowNavigator.WMSize(var Message: TWMSize);
var W, H: Integer;
begin
  inherited;
  // check for minimum size 

  W := Width;
  H := Height;
  SetSize(W, H);

  if ((W <> Width) or (H <> Height)) then
  begin
    inherited SetBounds(Left, Top, W, H);
  end;
  Message.Result := 0;
end;

procedure TCustomSDVArrowNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
  // Goal: To catch the "SetFocus" event/message. 
  // Objetivo: Capturar el mensaje "SetFocus". 
end;

procedure TCustomSDVArrowNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
  // Goal: To catch the "KillFocus" event/message. 
end;

procedure TCustomSDVArrowNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

function TCustomSDVArrowNavigator.getButtonsVisible(): TSDVButtonOptions;
begin
  Result := FButtonsVisible;
end;

function TCustomSDVArrowNavigator.getButtonsEnabled(): TSDVButtonOptions;
begin
  Result := FButtonsEnabled;
end;

function TCustomSDVArrowNavigator.getCaptions(): TStrings;
var UseDefault: Boolean;
begin
  UseDefault :=
  ((csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FCaptions.Count = 0));

  if (UseDefault)
    then Result := FDefaultCaptions
    else Result := FCaptions;
end;

function TCustomSDVArrowNavigator.getHints(): TStrings;
var UseDefault: Boolean;
begin
  UseDefault :=
  ((csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0));

  if (UseDefault)
    then Result := FDefaultHints
    else Result := FHints;
end;

function TCustomSDVArrowNavigator.getDelta(): Integer;
begin
  Result := FDelta;
end;

procedure TCustomSDVArrowNavigator.setButtonsVisible(const AValue: TSDVButtonOptions);
var
  I: TSDVArrowButtonOption;
  W, H: Integer;
  PrevValue, NewValue: Boolean;
begin
  W := Width;
  H := Height;

  FButtonsVisible := AValue;
  for I := Low(TSDVArrowButtonOption) to High(TSDVArrowButtonOption) do
  begin
    PrevValue := Buttons[I].Visible;
    NewValue  := ord(I) in FButtonsVisible;
    Buttons[I].Visible := NewValue;
    // change button active state;

    if (not PrevValue and NewValue) then // change from inactive to active:
    begin
      Buttons[I].Enabled := true;
      Include( FButtonsEnabled, ord(I) );
    end;
    // Enable "I" button;
  end;

  SetSize(W, H);
  if ((W <> Width) or (H <> Height)) then
  begin
    inherited SetBounds(Left, Top, W, H);
  end;
  Invalidate();
  // Goal: To remove buttons from the navigator. 
end;

procedure TCustomSDVArrowNavigator.setButtonsEnabled(const AValue: TSDVButtonOptions);
var I: TSDVArrowButtonOption;
begin
  FButtonsEnabled := [];
  // Clear the set;

  for I := Low(TSDVArrowButtonOption) to High(TSDVArrowButtonOption) do
  begin
    Buttons[I].Enabled := false;

    if (ord(I) in FButtonsVisible) and (ord(I) in AValue) then
    // "I" button is active and "I" button is selected
    begin
      Buttons[I].Enabled := true;

      Include( FButtonsEnabled, ord(I) );
      // Enable "I" button
    end;
  end;  
  // Objetivo: Activar botones del navegador. 
  // Goal: To enable buttons from the navigator. 
end;

procedure TCustomSDVArrowNavigator.setCaptions(AValue: TStrings);
begin
  if (AValue.Text = FDefaultCaptions.Text)
    then FCaptions.Clear()
    else FCaptions.Assign(AValue);
  // Objetivo: Metodo de escritura para propiedad "Captions". 
  // Goal: "Captions" property set method. 
end;

procedure TCustomSDVArrowNavigator.setHints(AValue: TStrings);
begin
  if (AValue.Text = FDefaultHints.Text)
    then FHints.Clear()
    else FHints.Assign(AValue);
  // Objetivo: Metodo de escritura para propiedad "Hints". 
  // Goal: "Hints" property set method. 
end;

procedure TCustomSDVArrowNavigator.setDelta(const AValue: Integer);
begin
  if (FDelta <> AValue) then
  begin
    FDelta := AValue;
  end;
  // Objetivo: Metodo de escritura para propiedad "Delta". 
  // Goal: "Delta" property set method. 
end;

procedure TCustomSDVArrowNavigator.setShowCaptions(AValue: Boolean);
begin
  FShowCaptions := AValue;
  if (AValue)
    then LoadUserCaptions()
    else HideCaptions();
  // Goal: To show the buttons's captions. 
end;

procedure TCustomSDVArrowNavigator.setEnabled(const AValue: Boolean);
var EachOption: TSDVArrowButtonOption;
begin
  InternalEnabled := AValue;
  for EachOption := Low(Buttons) to High(Buttons) do
  begin
    if (Buttons[EachOption].Visible) then
    begin
      Buttons[EachOption].Enabled := AValue;
    end;
  end;
  // Goal: "Enabled" property set method. 
  // Objetivo: Metodo escritura para propiedad "Enabled". 
end;

procedure TCustomSDVArrowNavigator.SetSize(var W, H: Integer);
var Count: Integer;

  function CountVisible: Integer;
  var EachOption: TSDVArrowButtonOption;
  begin
    Result := 0;
    for EachOption := Low(Buttons) to High(Buttons) do
    begin
      if (Buttons[EachOption].Visible) then
      begin
        Inc(Result);
      end;
    end;
    if (Result = 0)
      then Inc(Result);
    // Goal: To obtain no. of visible buttons. 
    // Objetivo: Obtener el no. de botones visibles. 
  end;

  procedure ArrangeHorizontal;
  var EachOption: TSDVArrowButtonOption;
     MinW, Space, Temp, Remain: Integer;
     X: Integer;
  begin
    MinW := Count * MinBtnSize.X;
    if (W < MinW) then
    begin
      W := MinW;
    end;

    if (H < MinBtnSize.Y) then
    begin
      H := MinBtnSize.Y;
    end;

    ButtonWidth := W div Count;
    Temp := Count * ButtonWidth;
    if (Align = alNone) then
    begin
      W := Temp;
    end;

    X := 0;
    Remain := W - Temp;
    Temp := Count div 2;
    for EachOption := Low(Buttons) to High(Buttons) do
    begin
      if (Buttons[EachOption].Visible) then
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

        Buttons[EachOption].SetBounds(X, 0, ButtonWidth + Space, Height);
        Inc(X, ButtonWidth + Space);
      end else
      begin
        Buttons[EachOption].SetBounds(Width + 1, 0, ButtonWidth, Height);
      end;
    end;
    InitImages(false);
    // Goal: To set the navigator orientation. 
    // Objetivo: Asignar la orientacion del navegador. 
  end;

  procedure ArrangeVertical;
  var EachOption: TSDVArrowButtonOption;
     MinH, Space, Temp, Remain: Integer;
     Y: Integer;
  begin
    MinH := Count * MinBtnSize.Y;
    if (H < MinH) then
    begin
      H := MinH;
    end;

    if (W < MinBtnSize.X) then
    begin
      W := MinBtnSize.X;
    end;

    ButtonHeight := H div Count;
    Temp := Count * ButtonHeight;
    if (Align = alNone) then
    begin
      H := Temp;
    end;

    Y := 0;
    Remain := H - Temp;
    Temp := Count div 2;
    for EachOption := Low(Buttons) to High(Buttons) do
    begin
      if (Buttons[EachOption].Visible) then
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

        Buttons[EachOption].SetBounds(0, Y, Width, ButtonHeight + Space);
        Inc(Y, ButtonHeight + Space);
      end else
      begin
        Buttons[EachOption].SetBounds(0, Height + 1, Width, ButtonHeight);
      end;
    end;
    InitImages(true);
    // Goal: To set the navigator orientation. 
  end;

begin
  if (csLoading in ComponentState)
    then Exit;
  if (Buttons[btnopFirst] = nil)
    then Exit;

  Count := CountVisible();
  if (W > H)
    then ArrangeHorizontal()
    else ArrangeVertical();
  // Goal: To set the navigator's size. 
  // Objetivo: Asignar el tamano al navegador. 
end;

procedure TCustomSDVArrowNavigator.OnClickDelegateer(Sender: TObject);
begin
  BtnClick(TSDVArrowNavigatorBitBtn(Sender).Index);
  // Goal: Transfer the "OnClick" event
  // from each button to the navigator. 
end;

procedure TCustomSDVArrowNavigator.OnMouseDownDelegateer
  (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var OldFocus: TSDVArrowButtonOption;
begin
  OldFocus := FocusedButton;
  FocusedButton := TSDVArrowNavigatorBitBtn(Sender).Index;
  if (TabStop and (GetFocus <> Handle) and CanFocus) then
  begin
    SetFocus;
    if (GetFocus <> Handle)
      then Exit;
  end else if ((TabStop) and (GetFocus = Handle) and (OldFocus <> FocusedButton)) then
  begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
  // Goal: Transfer the "OnMouseDown" event 
  // from each button to the navigator. 
end;

procedure TCustomSDVArrowNavigator.OnMouseMoveDelegateer
 (Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (FOnMouseMove <> nil) then
  begin
    FOnMouseMove(Sender, Shift, X, Y );
  end;
  // Goal: Transfer the "OnMouseMove" event
  // from each button to the navigator. 
end;

procedure TCustomSDVArrowNavigator.OnHintsChanged(Sender: TObject);
begin
  LoadUserHints();
  // Goal: To answer to the "Hints.OnChange" event. 
end;

procedure TCustomSDVArrowNavigator.OnCaptionsChanged(Sender: TObject);
begin
  LoadUserCaptions();
  if (not FInitCaptions)
    then ShowCaptions := true;
  // Goal: To answer to the "Captions.OnChange" event. 
end;

procedure TCustomSDVArrowNavigator.LoadDefaultHints();
begin
  if (FDefaultHints = nil) then
  begin
    FDefaultHints := TStringList.Create();

    FDefaultHints.Add(resbtnopNone_Hint);
    FDefaultHints.Add(resbtnopFirst_Hint);
    FDefaultHints.Add(resbtnopPrior_Hint);
    FDefaultHints.Add(resbtnopFastPrior_Hint);
    FDefaultHints.Add(resbtnopSearch_Hint);
    FDefaultHints.Add(resbtnopRoot_Hint);
    FDefaultHints.Add(resbtnopParent_Hint);
    FDefaultHints.Add(resbtnopNext_Hint);
    FDefaultHints.Add(resbtnopFastNext_Hint);    
    FDefaultHints.Add(resbtnopLast_Hint);
  end;
  // Goal: To get standard hints from the resource file. 
end;

procedure TCustomSDVArrowNavigator.LoadDefaultCaptions();
begin
  if (FDefaultCaptions = nil) then
  begin
    FDefaultCaptions := TStringList.Create();

    FDefaultCaptions.Add(resbtnopNone_Caption);
    FDefaultCaptions.Add(resbtnopFirst_Caption);
    FDefaultCaptions.Add(resbtnopFastPrior_Caption);
    FDefaultCaptions.Add(resbtnopPrior_Caption);
    FDefaultCaptions.Add(resbtnopSearch_Caption);
    FDefaultCaptions.Add(resbtnopRoot_Caption);
    FDefaultCaptions.Add(resbtnopParent_Caption);
    FDefaultCaptions.Add(resbtnopNext_Caption);
    FDefaultCaptions.Add(resbtnopFastNext_Caption);
    FDefaultCaptions.Add(resbtnopLast_Caption);
  end;
  // Goal: To get standard captions from the resource file. 
end;

procedure TCustomSDVArrowNavigator.LoadUserHints();
var I: Integer; J: TSDVArrowButtonOption;
begin
  for J := Low(Buttons) to High(Buttons) do
  begin
    Buttons[J].Hint := FDefaultHints[Ord(J)];
  end;

  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if (FHints.Strings[I] <> '') then
    begin
      Buttons[J].Hint := FHints.Strings[I];
    end;

    if (J = High(Buttons))
      then Exit;

    Inc(J);
  end;
  // Objetivo: Obtener los mensajes en tiempo de diseno. 
  // Goal: To get hints from design time. 
end;

procedure TCustomSDVArrowNavigator.LoadUserCaptions();
var I: Integer; J: TSDVArrowButtonOption;
begin
  if (FShowCaptions) then
  begin
    for J := Low(Buttons) to High(Buttons) do
    begin
      Buttons[J].Caption := FDefaultCaptions[Ord(J)];
    end;

    J := Low(Buttons);
    for I := 0 to (FCaptions.Count - 1) do
    begin
      if (FCaptions.Strings[I] <> '') then
      begin
        Buttons[J].Caption := FCaptions.Strings[I];
      end;

      if (J = High(Buttons))
        then Exit;
      Inc(J);
    end;
  end;
  // Objetivo: Obtener los titulos en tiempo de diseno. 
  // Goal: To get captions from design time. 
end;

procedure TCustomSDVArrowNavigator.HideCaptions();
var EachOption: TSDVArrowButtonOption;
begin
  for EachOption := Low(Buttons) to High(Buttons) do
  begin
    Buttons[EachOption].Caption := '';
  end;
  // Goal: To hide captions.
end;

procedure TCustomSDVArrowNavigator.InitHints();
begin
  LoadDefaultHints();
  LoadUserHints();
  // Objetivo: Tomar los mensajes estandar del archivo de recursos. 
  // Goal: To get standard hints from the resource file. 
end;

procedure TCustomSDVArrowNavigator.InitCaptions();
begin
  LoadDefaultCaptions();
  LoadUserCaptions();
  // Objetivo: Tomar los titulos estandar del archivo de recursos. 
  // Goal: To get standard captions from the resource file. 
end;

procedure TCustomSDVArrowNavigator.InitImages(IsVertical: Boolean);
var EachOption: TSDVArrowButtonOption; EachButton: TSDVArrowNavigatorBitBtn;
    PrevCaption, ResName: string; WasEnabled: Boolean;
begin
  for EachOption :=
    Low(TSDVArrowButtonOption) to
    High(TSDVArrowButtonOption) do
  begin
    EachButton := Buttons[EachOption];

    if (IsVertical)
      then FmtStr(ResName, 'arrow_v%s', [BitBtnNames[EachOption]])
      else FmtStr(ResName, 'arrow_h%s', [BitBtnNames[EachOption]]);

    {$IFDEF FPC}
    EachButton.Glyph.LoadFromLazarusResource(ResName);
    {$ENDIF}
    {$IFDEF DELPHI}
    EachButton.Glyph.LoadFromResourceName(HInstance, ResName);
    {$ENDIF}

    EachButton.NumGlyphs := 2;

    WasEnabled := EachButton.Enabled;
      EachButton.Enabled := false;
      EachButton.Enabled := true;
    EachButton.Enabled := WasEnabled;
    // refresh button*s enabled state to force image loading
    // refrescar estado habilitado de boton para forzar cargado de imagen

    PrevCaption := EachButton.Caption;
      EachButton.Caption := '';
    EachButton.Caption := PrevCaption;
  end;
  // cargar mapabits despues de modificar datos
  // load resource bitmap after modifyng data
end;

procedure TCustomSDVArrowNavigator.InitButtons();
var EachOption: TSDVArrowButtonOption;
    EachButton: TSDVArrowNavigatorBitBtn;
    X: Integer;
begin
  MinBtnSize.X := 20;
  MinBtnSize.Y := 18;
  X := 0;

  for EachOption :=
    Low(TSDVArrowButtonOption) to
    High(TSDVArrowButtonOption) do
  begin
    EachButton := TSDVArrowNavigatorBitBtn.Create(Self);

    EachButton.Index := EachOption;

    EachButton.Visible := true;
    EachButton.Enabled := true;

    EachButton.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);

    {$IFDEF DELPHI}
    EachButton.OnClick := OnClickDelegateer;
    EachButton.OnMouseDown := OnMouseDownDelegateer;
    EachButton.OnMouseMove := OnMouseMoveDelegateer;
    {$ENDIF}
    {$IFDEF FPC}
    EachButton.OnClick := @OnClickDelegateer;
    EachButton.OnMouseDown := @OnMouseDownDelegateer;
    EachButton.OnMouseMove := @OnMouseMoveDelegateer;
    {$ENDIF}

    // Since navigator's buttons CANNOT be access 
    // by the user in design time, 
    // then assign handlers directly by programming; 

    EachButton.Parent := Self;
    Buttons[EachOption] := EachButton;
    X := X + MinBtnSize.X;
  end;
  // Create each button

  Buttons[btnopFastPrior].NavStyle
    := Buttons[btnopFastPrior].NavStyle + [nsAllowTimer];
  Buttons[btnopPrior].NavStyle
    := Buttons[btnopPrior].NavStyle + [nsAllowTimer];
  Buttons[btnopNext].NavStyle
    := Buttons[btnopNext].NavStyle + [nsAllowTimer];
  Buttons[btnopFastNext].NavStyle
    := Buttons[btnopFastNext].NavStyle + [nsAllowTimer];
  // Add a pause flag; 

  InitImages(false);
  // cargar mapabits despues de modificar datos
  // load resource bitmap after modifyng data

  // Objetivo: Agregar cada boton al navegador. 
  // Goal: To add each navigator's button. 
end;

procedure TCustomSDVArrowNavigator.Loaded();
var W, H: Integer;
begin
  inherited Loaded();

  W := Width;
  H := Height;
  SetSize(W, H);

  if ((W <> Width) or (H <> Height)) then
  begin
    inherited SetBounds(Left, Top, W, H);
  end;
  // Goal: To read this componente from the resource file.
  // Objetivo: Leer este componente desde el archivo de recursos. 
end;

procedure TCustomSDVArrowNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var NewFocus, OldFocus: TSDVArrowButtonOption;
begin
  OldFocus := FocusedButton;
  case (Key) of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if (NewFocus < High(Buttons))
            then NewFocus := Succ(NewFocus);
        until ((NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible));

        if (NewFocus <> FocusedButton) then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate();
          Buttons[FocusedButton].Invalidate();
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if (NewFocus > Low(Buttons)) then
            NewFocus := Pred(NewFocus);
        until ((NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible));

        if (NewFocus <> FocusedButton) then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate();
          Buttons[FocusedButton].Invalidate();
        end;
      end;
    VK_SPACE:
      begin
        if (Buttons[FocusedButton].Enabled) then
          Buttons[FocusedButton].Click();
      end;
(*
    VK_RETURN:
      begin
        if (Buttons[ButtonsDefault].Enabled) then
          Buttons[ButtonsDefault].Click();
      end;
*)
  end;
end;

procedure TCustomSDVArrowNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Nothing, on purpouse, hide buttons
end;

procedure TCustomSDVArrowNavigator.ClickFirst();
begin
  // Goal: Do "First" button action.  
  // Objetivo: Realizar accion de boton "Primero". 
end;

procedure TCustomSDVArrowNavigator.ClickFastPrior();
begin
  // Goal: Do "Prior" button action.  
  // Objetivo: Realizar accion de boton "Anterior". 
end;

procedure TCustomSDVArrowNavigator.ClickPrior();
begin
  // Goal: Do "Prior" button action.  
  // Objetivo: Realizar accion de boton "Anterior". 
end;

procedure TCustomSDVArrowNavigator.ClickSearch();
begin
  // Goal: Do "Search" button action.  
  // Objetivo: Realizar accion de boton "Buscar". 
end;

procedure TCustomSDVArrowNavigator.ClickMoveToRoot();
begin
  // Goal: Do "Raiz" button action.
  // Objetivo: Realizar accion de boton "Raiz".
end;

procedure TCustomSDVArrowNavigator.ClickMoveToParent();
begin
  // Goal: Do "Parent" button action.  
  // Objetivo: Realizar accion de boton "Padre". 
end;

procedure TCustomSDVArrowNavigator.ClickNext();
begin
  // Goal: Do "Next" button action.  
  // Objetivo: Realizar accion de boton "Siguiente". 
end;

procedure TCustomSDVArrowNavigator.ClickFastNext();
begin
  // Goal: Do "Next" button action.
  // Objetivo: Realizar accion de boton "Siguiente". 
end;

procedure TCustomSDVArrowNavigator.ClickLast();
begin
  // Goal: Do "Last" button action.  
  // Objetivo: Realizar accion de boton "Ultimo". 
end;

constructor TCustomSDVArrowNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if (not NewStyleControls) then
  begin
    ControlStyle := ControlStyle + [csFramed];
  end;

  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 241;
  Height := 25;

  FDelta := 10;

  ButtonHeight := 0;
  ButtonWidth  := 0;

  // assign visible buttons
  FButtonsVisible := [];
  Include(FButtonsVisible, ord(btnopFirst));
  Include(FButtonsVisible, ord(btnopFastPrior));
  Include(FButtonsVisible, ord(btnopPrior));
  Include(FButtonsVisible, ord(btnopSearch));
  Include(FButtonsVisible, ord(btnopFastNext));
  Include(FButtonsVisible, ord(btnopNext));
  Include(FButtonsVisible, ord(btnopLast));

  // assign enabled buttons
  FButtonsEnabled := [];
  Include(FButtonsEnabled, ord(btnopFirst));
  Include(FButtonsEnabled, ord(btnopFastPrior));
  Include(FButtonsEnabled, ord(btnopPrior));
  Include(FButtonsEnabled, ord(btnopSearch));
  Include(FButtonsEnabled, ord(btnopFastNext));
  Include(FButtonsEnabled, ord(btnopNext));
  Include(FButtonsEnabled, ord(btnopLast));

  FHints := TStringList.Create;
  FCaptions := TStringList.Create;

  {$IFDEF DELPHI}
  TStringList(FHints).OnChange := OnHintsChanged;
  TStringList(FCaptions).OnChange := OnCaptionsChanged;
  {$ENDIF}
  {$IFDEF FPC}
  TStringList(FHints).OnChange := @OnHintsChanged;
  TStringList(FCaptions).OnChange := @OnCaptionsChanged;
  {$ENDIF}

  FShowCaptions := false;
  FInitCaptions := false;

  InitButtons();
  InitHints();
  InitCaptions();

  FInitCaptions := true;

  FocusedButton := btnopFirst;
  FullRepaint   := false;
  // Goal: To prepare the navigator. 
end;

destructor TCustomSDVArrowNavigator.Destroy();
begin
  FDefaultHints.Free();
  FCaptions.Free();
  FHints.Free();

  inherited Destroy();
  // Goal: To unprepare the navigator. 
end;

procedure TCustomSDVArrowNavigator.SetBounds
  (ALeft, ATop, AWidth, AHeight: Integer);
var W, H: Integer;
begin
  W := AWidth;
  H := AHeight;

  if (not HandleAllocated) then
  begin
    SetSize(W, H);
  end;

  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TCustomSDVArrowNavigator.BtnClick(Index: TSDVArrowButtonOption);
begin
  if (not (csDesigning in ComponentState)) then
  begin
    case (Index) of
      btnopFirst:        ClickFirst();
      btnopFastPrior:    ClickFastPrior();
      btnopPrior:        ClickPrior();
      btnopSearch:       ClickSearch();
      btnopMoveToRoot:   ClickMoveToRoot();
      btnopMoveToParent: ClickMoveToParent();
      btnopNext:         ClickNext();
      btnopFastNext:     ClickFastNext();
      btnopLast:         ClickLast();
    end;
  end;
  // Goal: Do standard "click" action for each button. 
  // Objetivo: Realizar accion estandar "click" para cada boton. 
end;

(* TSDVArrowNavigatorBitBtn  *)

procedure TSDVArrowNavigatorBitBtn.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (MouseCapture) then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := false;
      raise;
    end;
  end;
end;

procedure TSDVArrowNavigatorBitBtn.MouseDown
  (Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if (nsAllowTimer in FNavStyle) then
  begin
    if (FRepeatTimer = nil) then
    begin
      FRepeatTimer := TTimer.Create(Self);
    end;

    FRepeatTimer.OnTimer  := {$IFNDEF DELPHI}@{$ENDIF}TimerExpired;

    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := true;
  end;
end;

procedure TSDVArrowNavigatorBitBtn.MouseUp
  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Enabled  := false;
  end;
end;

destructor TSDVArrowNavigatorBitBtn.Destroy();
begin
  if (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Free;
  end;
  inherited Destroy();
end;

(* TCustomSDVDelegateArrowNavigator  *)

procedure TCustomSDVDelegateArrowNavigator.DelegateClickFirst();
begin
  if (FOnClickFirst <> nil) then
  begin
    FOnClickFirst(Self);
  end;
  // Goal: To execute the "OnClickFirst" event.
  // Objetivo: Ejecutar el evento "OnClickFirst". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickPrior();
begin
  if (FOnClickPrior <> nil) then
  begin
    FOnClickPrior(Self);
  end;
  // Goal: To execute the "OnClickPrior" event.
  // Objetivo: Ejecutar el evento "OnClickPrior". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickFastPrior();
begin
  if (FOnClickFastPrior <> nil) then
  begin
    FOnClickFastPrior(Self);
  end;
  // Goal: To execute the "OnClickFastPrior" event.
  // Objetivo: Ejecutar el evento "OnClickFastPrior". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickSearch();
begin
  if (FOnClickSearch <> nil) then
  begin
    FOnClickSearch(Self);
  end;
  // Goal: To execute the "OnClickSearch" event.
  // Objetivo: Ejecutar el evento "OnClickSearch". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickMoveToRoot();
begin
  if (FOnClickMoveToRoot <> nil) then
  begin
    FOnClickMoveToRoot(Self);
  end;
  // Goal: To execute the "OnClickMoveToRoot" event.
  // Objetivo: Ejecutar el evento "OnClickMoveToRoot".
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickMoveToParent();
begin
  if (FOnClickMoveToParent <> nil) then
  begin
    FOnClickMoveToParent(Self);
  end;
  // Goal: To execute the "OnClickMoveToParent" event.
  // Objetivo: Ejecutar el evento "OnClickMoveToParent".
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickFastNext();
begin
  if (FOnClickFastNext <> nil) then
  begin
    FOnClickFastNext(Self);
  end;
  // Goal: To execute the "OnClickFastNext" event.
  // Objetivo: Ejecutar el evento "OnClickFastNext". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickNext();
begin
  if (FOnClickNext <> nil) then
  begin
    FOnClickNext(Self);
  end;
  // Goal: To execute the "OnClickNext" event.
  // Objetivo: Ejecutar el evento "OnClickNext". 
end;

procedure TCustomSDVDelegateArrowNavigator.DelegateClickLast();
begin
  if (FOnClickLast <> nil) then
  begin
    FOnClickLast(Self);
  end;
  // Goal: To execute the "OnClickLast" event.
  // Objetivo: Ejecutar el evento "OnClickLast". 
end;

procedure TCustomSDVDelegateArrowNavigator.ClickFirst();
begin
  DelegateClickFirst();
  // Goal:  "First" button action.  
  // Objetivo: Realizar accion de boton "Primero". 
end;

procedure TCustomSDVDelegateArrowNavigator.ClickPrior();
begin
  DelegateClickPrior();
  // Goal:  "Prior" button action.  
  // Objetivo: Realizar accion de boton "Anterior".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickFastPrior();
begin
  DelegateClickFastPrior();
  // Goal:  "FastPrior" button action.  
  // Objetivo: Realizar accion de boton "Anterior".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickSearch();
begin
  DelegateClickSearch();
  // Goal:  "Search" button action.
  // Objetivo: Realizar accion de boton "Buscar".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickMoveToRoot();
begin
  DelegateClickMoveToRoot();
  // Goal: "Root" button action.
  // Objetivo: Realizar accion de boton "Raiz".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickMoveToParent();
begin
  DelegateClickMoveToParent();
  // Goal: "Parent" button action.
  // Objetivo: Realizar accion de boton "Padre".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickNext();
begin
  DelegateClickNext();
  // Goal:  "Next" button action.
  // Objetivo: Realizar accion de boton "Siguiente".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickFastNext();
begin
  DelegateClickFastNext();
  // Goal:  "FastNext" button action.
  // Objetivo: Realizar accion de boton "Siguiente".
end;

procedure TCustomSDVDelegateArrowNavigator.ClickLast();
begin
  DelegateClickLast();
  // Goal:  "Last" button action.
  // Objetivo: Realizar accion de boton "Ultimo".
end;

{$IFDEF DELPHI}
{$R 'uktarrownavres.dcr'}
{$ENDIF}

initialization
{$IFDEF FPC}
{$I 'uktarrownavres.lrs'}
{$ENDIF}
end.
