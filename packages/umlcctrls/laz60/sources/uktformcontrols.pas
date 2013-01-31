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

unit uktformcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,
  Forms,
  uktctrls, uktpanels, uktforms, uktbitbtns,
  dummy;

type

(* TCustomSDVFormPanelControl *)

  TCustomSDVFormPanelControl = class(TCustomSDVPanel)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    FIsActivated: Boolean;

    FOnActivate: TNotifyEvent;
    FOnClose: TCloseEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnDropFiles: TDropFilesEvent;
    FOnHelp: THelpEvent;
    FOnHide: TNotifyEvent;
    FOnShortcut: TShortCutEvent;
    FOnShow: TNotifyEvent;
    FOnWindowStateChange: TNotifyEvent;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Show();
    function ShowModal(): Integer; virtual;

    function Active(): Boolean;

    { May Publish events declarations }

    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery : TCloseQueryEvent
      read FOnCloseQuery write FOnCloseQuery;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnResize;
    property OnShortcut: TShortcutEvent read FOnShortcut write FOnShortcut;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnWindowStateChange: TNotifyEvent
      read FOnWindowStateChange write FOnWindowStateChange;
  end;

  TCustomSDVFormPanel = class;

(* TCustomSDVFormPanelTitleBar *)

  ///<summary>
  ///It's a panel that simulates a menubar.
  ///</summary>
  TCustomSDVFormPanelTitleBar = class(TCustomSDVPanel)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

    (* fields declarations *)

    FCurrentWindowState: TWindowState;

    // initial window state
    FWindowState:    TWindowState;

    FMenuButton:     TCustomSDVBitBtn;
    FMinimizeButton: TCustomSDVBitBtn;
    FMaximizeButton: TCustomSDVBitBtn;
    FExitButton:     TCustomSDVBitBtn;
  protected
    (* Protected declarations *)

    (* accesor declarations *)

    function getCurrentWindowState: TWindowState;
    function getWindowState: TWindowState;

    procedure setCurrentWindowState(const Value: TWindowState);
    procedure setWindowState(const Value: TWindowState);
  protected
    (* Protected declarations *)

    (* properties declarations *)

    property CurrentWindowState: TWindowState
      read getCurrentWindowState write setCurrentWindowState;
    property WindowState: TWindowState
      read getWindowState write setWindowState;
  protected
    (* Protected declarations *)

    procedure MinimizeEvent(Sender: TObject);
    procedure MaximizeEvent(Sender: TObject);
    procedure ExitEvent(Sender: TObject);

    procedure createButtons(); dynamic;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    procedure Close();
  end;

(* TCustomSDVFormPanelMenuBar *)

  ///<summary>
  ///It's a panel that simulates a menubar.
  ///</summary>
  TCustomSDVFormPanelMenuBar = class(TCustomSDVPanel)
  private
    (* Private declarations *)

  protected
    (* Protected declarations *)

  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

(* TCustomSDVFormPanel *)

  ///<summary>
  /// This panel will have a scrollbar and a title bar.
  ///</summary>
  TCustomSDVFormPanel = class(TCustomSDVFormPanelControl)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FTitleBar: TCustomSDVFormPanelTitleBar;
    FMenu:     TCustomSDVFormPanelMenuBar;
    FTitleBarIsAuto: Boolean;
  protected
    (* Protected declarations *)

    function CreateTitleBarClass(): TCustomSDVFormPanelTitleBar; dynamic;
  protected
    (* Protected declarations *)

    function getTitleBar(): TCustomSDVFormPanelTitleBar; dynamic;
    procedure setTitleBar(const Value: TCustomSDVFormPanelTitleBar); dynamic;

    function getMenu(): TCustomSDVFormPanelMenuBar; dynamic;
    procedure setMenu(const Value: TCustomSDVFormPanelMenuBar); dynamic;
  protected
    (* Protected declarations *)

    property TitleBarIsAuto: Boolean
      read FTitleBarIsAuto write FTitleBarIsAuto;

    property TitleBar: TCustomSDVFormPanelTitleBar
      read getTitleBar write setTitleBar;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    (* Can Be Published declarations *)

    property Menu: TCustomSDVFormPanelMenuBar
      read getMenu write setMenu;
  end;

(* TSDVFormPanel *)

  TSDVFormPanel = class(TCustomSDVFormPanel)
  published
    (* Published declarations *)

    property Menu;

    property OnActivate;
    property OnClose;
    property OnCloseQuery;
    property OnCreate;
    property OnDeactivate;
    property OnDestroy;
    property OnDropFiles;
    property OnHelp;
    property OnHide;
    property OnResize;
    property OnShortcut;
    property OnShow;
    property OnWindowStateChange;
  end;

implementation

(* TCustomSDVFormPanelTitleBar *)

function TCustomSDVFormPanelTitleBar.getCurrentWindowState: TWindowState;
begin
  Result := FCurrentWindowState;
end;

function TCustomSDVFormPanelTitleBar.getWindowState: TWindowState;
begin
  Result := FWindowState;
end;

procedure TCustomSDVFormPanelTitleBar.setCurrentWindowState
  (const Value: TWindowState);
begin
  FCurrentWindowState := Value;
end;

procedure TCustomSDVFormPanelTitleBar.setWindowState
  (const Value: TWindowState);
begin
  FWindowState := Value;
end;

procedure TCustomSDVFormPanelTitleBar.MinimizeEvent(Sender: TObject);
begin
  Self.Close;
end;

procedure TCustomSDVFormPanelTitleBar.MaximizeEvent(Sender: TObject);
begin
  Self.Close;
end;

procedure TCustomSDVFormPanelTitleBar.ExitEvent(Sender: TObject);
begin
  Self.Close;
end;

procedure TCustomSDVFormPanelTitleBar.createButtons();
var AWidth: Integer;
begin
  FMenuButton        := TCustomSDVBitBtn.Create(Self);
  FMenuButton.Height := (Self.Height - 2);
  FMenuButton.Width  := (Self.Height - 2);
  FMenuButton.Top    := 1;
  AWidth := FMenuButton.Width;
  FMenuButton.Left   := (Self.Width - AWidth);
  FMenuButton.Color:=clGray;
  FMenuButton.Align:= alLeft;
  Self.InsertControl(FMenuButton);

  FExitButton        := TCustomSDVBitBtn.Create(Self);
  FExitButton.Height := (Self.Height - 2);
  FExitButton.Width  := (Self.Height - 2);
  FExitButton.Top    := 1;
  AWidth := FExitButton.Width;
  FExitButton.Left   := (Self.Width - AWidth);
  FExitButton.Color:=clGray;
  FExitButton.Align:= alRight;
  FExitButton.OnClick:=@ExitEvent;
  Self.InsertControl(FExitButton);

  FMaximizeButton  := TCustomSDVBitBtn.Create(Self);
  FMaximizeButton.Height:= (Self.Height - 2);
  FMaximizeButton.Width:= (Self.Height - 2);
  FMaximizeButton.Top:=1;
  AWidth := FExitButton.Width;
  FMaximizeButton.Left   := (Self.Width - AWidth);
  FMaximizeButton.Color:=clGray;
  FMaximizeButton.Align:= alRight;
  FMaximizeButton.OnClick:=@MaximizeEvent;
  Self.InsertControl(FMaximizeButton);

  FMinimizeButton  := TCustomSDVBitBtn.Create(Self);
  FMinimizeButton.Height:= (Self.Height - 2);
  FMinimizeButton.Width:= (Self.Height - 2);
  FMinimizeButton.Top:=1;
  AWidth := FExitButton.Width;
  FMinimizeButton.Left   := (Self.Width - AWidth);
  FMinimizeButton.Color:=clGray;
  FMinimizeButton.Align:= alRight;
  FMinimizeButton.OnClick:=@MinimizeEvent;
  Self.InsertControl(FMinimizeButton);
end;

constructor TCustomSDVFormPanelTitleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height:=32;
  Self.Align:= alTop;
  Self.Color:=clBlue;
  Self.BorderStyle:=bsNone;
  createButtons;
end;

destructor TCustomSDVFormPanelTitleBar.Destroy();
begin
  inherited Destroy();
end;

function FormByControl((*in/bycopy*) Control: TControl): TCustomForm;
var Found: Boolean;
begin
  Result := nil; Found := false;
  while (Assigned(Control) and (not Found)) do
  begin
    Found := (Control is TCustomForm);
    if (not Found)
      then Control := Control.Parent;
  end;
  if (Found)
    then Result := (Control as TCustomForm);
end;

procedure TCustomSDVFormPanelTitleBar.Close();
var AForm: TCustomForm;
begin
  AForm := FormByControl(Self);
  AForm.Close();
end;

(* TCustomSDVFormPanelMenuBar *)

constructor TCustomSDVFormPanelMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomSDVFormPanelMenuBar.Destroy();
begin
  inherited Destroy();
end;

(* TCustomSDVFormPanelControl *)

constructor TCustomSDVFormPanelControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FKeyValueTypeList := TSDVKeyValueTypeList.Create;
  Align := alClient;
  FIsActivated := false;
end;

destructor TCustomSDVFormPanelControl.Destroy();
begin
  //FKeyValueTypeList.Empty();
  //FKeyValueTypeList.Free();
  inherited Destroy();
end;

procedure TCustomSDVFormPanelControl.Show();
begin
  //
end;

function TCustomSDVFormPanelControl.ShowModal(): Integer;
begin
  Result := -1;
end;

function TCustomSDVFormPanelControl.Active(): Boolean;
begin
  Result := FIsActivated;
end;

(* TCustomSDVFormPanel *)

function TCustomSDVFormPanel.CreateTitleBarClass(): TCustomSDVFormPanelTitleBar;
begin
  Result := TCustomSDVFormPanelTitleBar.Create(Self);
end;

function TCustomSDVFormPanel.getTitleBar(): TCustomSDVFormPanelTitleBar;
begin
  Result := self.FTitleBar;
end;

procedure TCustomSDVFormPanel.setTitleBar
  (const Value: TCustomSDVFormPanelTitleBar);
begin
  self.FTitleBar := Value;
end;

function TCustomSDVFormPanel.getMenu(): TCustomSDVFormPanelMenuBar;
begin
  Result := FMenu;
end;

procedure TCustomSDVFormPanel.setMenu(const Value: TCustomSDVFormPanelMenuBar);
begin
  FMenu := Value;
end;

constructor TCustomSDVFormPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TitleBarIsAuto := True;
  Self.FTitleBar := CreateTitleBarClass;
  Self.FTitleBar.Align:=alTop;
  Self.InsertControl(FTitleBar);
end;

destructor TCustomSDVFormPanel.Destroy();
begin
  //FKeyValueTypeList.Empty();
  //FKeyValueTypeList.Free();
  inherited Destroy();
end;


end.

