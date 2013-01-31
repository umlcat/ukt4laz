unit uktdropbtns;

interface
uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages, Graphics,
  Controls, Forms,
  ExtCtrls, Buttons,
(*.ENDIF*)
  SysUtils, Classes, Types,
  ActnList,
  uktpanels, uktcomboctrls,
  dummy;

type

(* TCustomSDVComboButtonControl *)

  TCustomSDVComboButtonControl = class(TCustomSDVComboControl)
  private
    (* Private declarations *)

    procedure MainButtonDelegateOnClick(Sender: TObject);
  protected
    (* Protected declarations *)

    FShowCaption: Boolean;
    FShowGlyph:   Boolean;

    FMainButton: TSDVComboControlSpeedButton;

    function DropDownLocation: TPoint; dynamic;

    procedure DoMainButtonOnClick; dynamic; abstract;

    procedure AssignComboControlTop;
    procedure AssignComboControlBottom;
    procedure AssignComboControlLeft;
    procedure AssignComboControlRight;

    procedure AssignButtons;
    procedure AssignImages;

    procedure CreateControls; override;
    procedure DestroyControls; override;

    procedure RefreshCaption; dynamic; abstract;
    procedure RefreshGlyph; dynamic; abstract;
    procedure ReCreateControls; override;

    function getShowCaption: Boolean;
    function getShowGlyph: Boolean;

    procedure setShowCaption(const Value: Boolean);
    procedure setShowGlyph(const Value: Boolean);

    property ShowCaption: Boolean
      read getShowCaption write setShowCaption;
    property ShowGlyph: Boolean
      read getShowGlyph write setShowGlyph;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

(* TCustomSDVDropDownButton *)

  TOnDelegateSizeEvent = function(Sender: TObject): Integer of object;

  TCustomSDVDropDownButton = class(TCustomSDVComboButtonControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FCaption: string;
    FGlyph: TBitmap;

    FOnClick:    TNotifyEvent;
    FOnDropDown: TNotifyEvent;

    FOnPopUpControlWidth: TOnDelegateSizeEvent;
    FOnPopUpControlHeight: TOnDelegateSizeEvent;

    function DelegateOnPopUpControlWidth: Integer;
    function DelegateOnPopUpControlHeight: Integer;

    function PopUpControlWidth: Integer; dynamic;
    function PopUpControlHeight: Integer; dynamic;

    function getCaption: string; override;
    function getGlyph: TBitmap;

    procedure setAlign(const Value: TAlign); override;
    procedure setCaption(const Value: string); override;
    procedure setGlyph(Value: TBitmap);

    procedure DelegateOnClick;
    procedure DelegateOnDropDown;

    procedure RefreshCaption; override;
    procedure RefreshGlyph; override;

    procedure DoMainButtonOnClick; override;
    procedure DoComboButtonOnClick; override;
    procedure Click; override;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DropDownLocation: TPoint; override;

    property Align;
    property Direction;
    property Orientation;
    property Position;

    (* properties: *)

    property Caption: string
       read getCaption write setCaption;
    property Glyph: TBitmap
       read getGlyph write setGlyph;

    (* events: *)

    property OnClick: TNotifyEvent
       read FOnClick write FOnClick;
    property OnDropDown: TNotifyEvent
       read FOnDropDown write FOnDropDown;

    property OnPopUpControlWidth: TOnDelegateSizeEvent
      read FOnPopUpControlWidth write FOnPopUpControlWidth;
    property OnPopUpControlHeight: TOnDelegateSizeEvent
      read FOnPopUpControlHeight write FOnPopUpControlHeight;
  end;

(* TCustomSDVActionButton *)

  TCustomSDVActionButton = class(TCustomSDVComboButtonControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FAction: TContainedAction;

    property Action: TContainedAction
     read FAction write FAction;
  public
    (* Public declarations *)
  end;

(* TSDVDropDownButton *)

  TSDVDropDownButton = class(TCustomSDVDropDownButton)
  published
    (* Published declarations *)

    (* TCustomPanel: *)

(*.IFDEF MSWINDOWS*)

    (* properties: *)

    property AutoSize;
    property BiDiMode;

    (*$IFDEF DELPHI*)
    property Ctl3D;
    (*$ENDIF*)

    property UseDockManager;
    property DockSite;
    property DragCursor;
    property DragKind;
    property FullRepaint;

    (*$IFDEF DELPHI*)
    property Locked;
    (*$ENDIF*)

    property ParentBiDiMode;

    (*$IFDEF DELPHI*)
    property ParentCtl3D;
    (*$ENDIF*)

    (* events: *)

    (*$IFDEF DELPHI*)
    property OnCanResize;
    (*$ENDIF*)

    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
(*.ENDIF*)

    (* properties: *)

    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    (* events: *)

    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;

    (* TCustomSDPanel: *)

    (* TCustomSDVComboButtonControl: *)

    (* properties: *)

    property ShowCaption;
    property ShowGlyph;

    (* TCustomSDVDropDownButton: *)

    (* properties: *)

    property Glyph;

    property Orientation;
    property Direction;
    property Position;

    (* events: *)

    property OnDropDown;

    property OnPopUpControlWidth;
    property OnPopUpControlHeight;
  end;

  function GlobalOrigin(const Control: TControl): TPoint;

implementation

(*.IFDEF MSWINDOWS*)
function GlobalOrigin(const Control: TControl): TPoint;
var ParentForm: TCustomForm;
    MenuBar, TitleBar, Border: Integer;
begin
  Result.X := 0;
  Result.Y := 0;
  if (Control <> nil) then
  begin
    ParentForm := GetParentForm(Control);
    if (ParentForm <> nil) then
    begin
      TitleBar := GetSystemMetrics(SM_CYCAPTION);
      Result.Y := Result.Y + TitleBar;
      // obtain the main form*s title height

      if (ParentForm.Menu <> nil)
        then MenuBar := GetSystemMetrics(SM_CYMENU)
        else MenuBar := 0;
      Result.Y := Result.Y + MenuBar;
      // obtain the main form*s menu height

      Border := GetSystemMetrics(SM_CYSIZEFRAME);;
      Result.Y := Result.Y + Border;
      // obtain the main form*s vertical border height

      Border := GetSystemMetrics(SM_CXSIZEFRAME);;
      Result.X := Result.X + Border;
      // obtain the main form*s horizontal border height

      Result.X := Result.X + ParentForm.Left;
      Result.Y := Result.Y + ParentForm.Top;
    end;
    Result.X := Result.X + Control.Left;
    Result.Y := Result.Y + Control.Top;
  end;
end;
(*.ENDIF*)

(* TCustomSDVComboButtonControl *)

function TCustomSDVComboButtonControl.DropDownLocation: TPoint;
begin
  Result := GlobalOrigin(Self);
  case (FOrientation) of
    doBottom: Result.Y := Result.Y - Self.Height;
    doLeft:   Result.X := Result.X + Self.Width;
    doRight:  Result.X := Result.X - Self.Width;
    // doTop:
    // doCustom:
    else Result.Y := Result.Y + Self.Height;
  end;
end;

procedure TCustomSDVComboButtonControl.MainButtonDelegateOnClick(Sender: TObject);
begin
  DoMainButtonOnClick;
end;

procedure TCustomSDVComboButtonControl.ReCreateControls;
begin
  inherited ReCreateControls;
  RefreshCaption;
  RefreshGlyph;
end;

function TCustomSDVComboButtonControl.getShowCaption: Boolean;
begin
  Result := FShowCaption;
  // Goal: "ShowCaption" property get method.
  // Objetivo: Metodo lectura para propiedad "ShowCaption".
end;

function TCustomSDVComboButtonControl.getShowGlyph: Boolean;
begin
  Result := FShowGlyph;
  // Goal: "ShowGlyph" property get method.
  // Objetivo: Metodo lectura para propiedad "ShowGlyph".
end;

procedure TCustomSDVComboButtonControl.setShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  RefreshCaption;
  // Goal: "ShowCaption" property set method.
  // Objetivo: Metodo escritura para propiedad "ShowCaption".
end;

procedure TCustomSDVComboButtonControl.setShowGlyph(const Value: Boolean);
begin
  FShowGlyph := Value;
  RefreshGlyph;
  // Goal: "ShowGlyph" property set method.
  // Objetivo: Metodo escritura para propiedad "ShowGlyph".
end;

procedure TCustomSDVComboButtonControl.AssignComboControlTop;
begin
  Width  := MainButtonHeight + TestArea;
  Height := MainButtonWidth + ComboButtonWidth + TestArea;

  FComboButton.SetBounds
    (0, 0, ComboButtonHeight, ComboButtonWidth);
  FMainButton.SetBounds
    (0, ComboButtonWidth, MainButtonHeight, MainButtonWidth);

  FComboButton.Align := alTop;
  FMainButton.Align := alClient;
end;

procedure TCustomSDVComboButtonControl.AssignComboControlBottom;
begin
  Width  := MainButtonHeight + TestArea;
  Height := MainButtonWidth + ComboButtonWidth + TestArea;

  FMainButton.SetBounds
    (0, 0, MainButtonHeight, MainButtonWidth);
  FComboButton.SetBounds
    (0, MainButtonWidth, ComboButtonHeight, ComboButtonWidth);

  FComboButton.Align := alBottom;
  FMainButton.Align := alClient;
end;

procedure TCustomSDVComboButtonControl.AssignComboControlLeft;
begin
  Width  := MainButtonWidth + ComboButtonWidth + TestArea;
  Height := MainButtonHeight + TestArea;

  FComboButton.SetBounds
    (0, 0, ComboButtonWidth, ComboButtonHeight);
  FMainButton.SetBounds
    (ComboButtonWidth, 0, MainButtonWidth, MainButtonHeight);

  FComboButton.Align := alLeft;
  FMainButton.Align := alClient;
end;

procedure TCustomSDVComboButtonControl.AssignComboControlRight;
begin
  Width  := MainButtonWidth + ComboButtonWidth + TestArea;
  Height := MainButtonHeight + TestArea;

  FMainButton.SetBounds
    (0, 0, MainButtonWidth, MainButtonHeight);
  FComboButton.SetBounds
    (MainButtonWidth, 0, ComboButtonWidth, ComboButtonHeight);

  FComboButton.Align := alRight;
  FMainButton.Align := alClient;
end;

procedure TCustomSDVComboButtonControl.AssignButtons;
begin
  case (FPosition) of
    dpBottom: AssignComboControlBottom;
    dpLeft:   AssignComboControlLeft;
    dpRight:  AssignComboControlRight;
//    dpTop:
    else      AssignComboControlTop;
  end;
end;

procedure TCustomSDVComboButtonControl.AssignImages;
var ResName, DirName: string;
begin
  case (FDirection) of
    ddUp:     DirName := 'UP';
    ddLeft:   DirName := 'LEFT';
    ddRight:  DirName := 'RIGHT';
//    ddDown:
    else      DirName := 'DOWN';
  end;

  FmtStr(ResName, 'DRP_%s', [DirName]);
  LoadButtonImage(ResName);
end;

procedure TCustomSDVComboButtonControl.CreateControls;
begin
  FMainButton := TSDVComboControlSpeedButton.Create(Self);
  FComboButton := TSDVComboControlSpeedButton.Create(Self);

  FMainButton.ComboControl := (*@*)Self;
  FComboButton.ComboControl := (*@*)Self;

  FMainButton.OnClick  := {$IFNDEF DELPHI}@{$ENDIF}MainButtonDelegateOnClick;
  FComboButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}ComboButtonDelegateOnClick;

  AssignOrientation;
  AssignButtons;
  AssignImages;

  Self.InsertControl(FMainButton);
  Self.InsertControl(FComboButton);
end;

procedure TCustomSDVComboButtonControl.DestroyControls;
begin
  FComboButton.Free; FComboButton := nil;
  FMainButton.Free; FMainButton := nil;
end;

constructor TCustomSDVComboButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FShowCaption := FALSE;
  FShowGlyph   := TRUE;
  CreateControls;
  // Goal: To prepare the control.
end;

destructor TCustomSDVComboButtonControl.Destroy;
begin
  DestroyControls;
  inherited Destroy;
  // Goal: To unprepare the control.
end;

(* TCustomSDVDropDownButton *)

function TCustomSDVDropDownButton.DelegateOnPopUpControlWidth: Integer;
begin
  Result := 0;
  if (Assigned(FOnPopUpControlWidth))
    then Result := FOnPopUpControlWidth(Self);
end;

function TCustomSDVDropDownButton.DelegateOnPopUpControlHeight: Integer;
begin
  Result := 0;
  if (Assigned(FOnPopUpControlHeight))
    then Result := FOnPopUpControlHeight(Self);
end;

function TCustomSDVDropDownButton.PopUpControlWidth: Integer;
begin
  Result := DelegateOnPopUpControlWidth;
end;

function TCustomSDVDropDownButton.PopUpControlHeight: Integer;
begin
  Result := DelegateOnPopUpControlHeight;
end;

function TCustomSDVDropDownButton.getCaption: string;
begin
  Result := FCaption;
  // Goal: "Caption" property get method.
  // Objetivo: Metodo lectura para propiedad "Caption".
end;

function TCustomSDVDropDownButton.getGlyph: TBitmap;
begin
  Result := FGlyph;
  // Goal: "Glyph" property get method.
  // Objetivo: Metodo lectura para propiedad "Glyph".
end;

procedure TCustomSDVDropDownButton.setAlign(const Value: TAlign);
begin
  InternalAlign := Value;
  ReCreateControls;
  // Goal: "Align" property set method.
  // Objetivo: Metodo escritura para propiedad "Align".
end;

procedure TCustomSDVDropDownButton.setCaption(const Value: string);
begin
  FCaption := Value;
  ShowCaption := (Value <> '');
  // Goal: "Caption" property set method.
  // Objetivo: Metodo escritura para propiedad "Caption".
end;

procedure TCustomSDVDropDownButton.setGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
  ShowGlyph := (Value <> nil);
  // Goal: "Glyph" property set method.
  // Objetivo: Metodo escritura para propiedad "Glyph".
end;

procedure TCustomSDVDropDownButton.DelegateOnClick;
begin
  if (Assigned(FOnClick))
    then FOnClick(Self);
end;

procedure TCustomSDVDropDownButton.DelegateOnDropDown;
begin
  if (Assigned(FOnDropDown))
    then FOnDropDown(Self);
end;

procedure TCustomSDVDropDownButton.RefreshCaption;
begin
  if (FMainButton <> nil) then
  begin
    if (FShowCaption)
      then FMainButton.Caption := FCaption
      else FMainButton.Caption := '';
  end;
end;

procedure TCustomSDVDropDownButton.RefreshGlyph;
begin
  if (FMainButton <> nil) then
  begin
    if (FShowGlyph)
      then FMainButton.Glyph.Assign(FGlyph)
      else FMainButton.Glyph.Assign(nil);
  end;
end;

procedure TCustomSDVDropDownButton.DoMainButtonOnClick;
begin
  DelegateOnClick;
end;

procedure TCustomSDVDropDownButton.DoComboButtonOnClick;
begin
  DelegateOnDropDown;
end;

procedure TCustomSDVDropDownButton.Click;
begin
  DoMainButtonOnClick;
end;

constructor TCustomSDVDropDownButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaption := '';
  FGlyph   := TBitmap.Create;
  // Goal: To prepare the control.
end;

destructor TCustomSDVDropDownButton.Destroy;
begin
  FGlyph.Free;
  FCaption := '';
  inherited Destroy;
  // Goal: To unprepare the control.
end;

function TCustomSDVDropDownButton.DropDownLocation: TPoint;
var W, H: Integer;
begin
  Result := GlobalOrigin(Self);
  // obtain the global coordinates of this control
  // obtener las coordenadas globales de este control

  case (FOrientation) of
    doBottom:
    begin
      H := PopUpControlHeight;
      if (H = 0)
        then H := Self.Height;
      Result.Y := Result.Y - H;
    end;
    doRight:
    begin
      W := PopUpControlWidth;
      if (W = 0)
        then W := Self.Width;
      Result.X := Result.X - W;
    end;
    doLeft: Result.X := Result.X - Self.Width;
    // doTop:
    // doCustom:
    else Result.Y := Result.Y + Self.Height;
  end;
end;

end.
