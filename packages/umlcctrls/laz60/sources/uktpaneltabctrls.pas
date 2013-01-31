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

unit uktpaneltabctrls;

interface

uses
(*.IFDEF MSWINDOWS*)
  Windows, Messages, Graphics,
  Controls, Forms,
  ExtCtrls, Buttons,
(*.ENDIF*)
  SysUtils, Classes, Math,
  uktctrls, uktpanels, ukttabs,
  uktarrownavoptions,
  uktarrownavs,
  dummy;

(**
 ** This unit defines a tab control.
 **)

type

(* TOnTabControlEvent *)

  TCustomSDVPanelTabControl = class;
  TOnTabControlEvent =
    procedure (TabControl: TCustomSDVPanelTabControl; TabIndex: Integer) of object;

(* TCustomSDVPanelTabControlNavigator *)

  TCustomSDVPanelTabControlNavigator = class(TCustomSDVArrowNavigator)
  private
    (* Private declarations *)
  protected
    (* protected declarations *)
  public
    (* Public declarations *)

    TabControl: TCustomSDVPanelTabControl;

    procedure ClickFirst(); override;
    procedure ClickPrior(); override;
    procedure ClickNext();  override;
    procedure ClickLast();  override;
  end;

(* TCustomSDVPanelTabControl *)

  TCustomSDVPanelTabControl = class(TCustomSDVPanel)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FTabs:      TCustomSDVTabs;
    FNavigator: TCustomSDVPanelTabControlNavigator;
    FHeader:    TCustomSDVPanel;
    // subcontroles
    // subcontrols

    FTabIndex: Integer;
    // boton seleccionado
    // selected button

    FOnTabEnter: TOnTabControlEvent;
    FOnTabExit:  TOnTabControlEvent;
  protected
    (* protected declarations *)

    function getTabs(): TStrings; virtual;
    function getTabIndex(): Integer; virtual;

    procedure setTabs(const Value: TStrings); virtual;
    procedure setTabIndex(const Value: Integer); virtual;
  protected
    (* protected declarations *)

    procedure DelegateOnEnterTab(TabIndex: Integer); (*static;*)
    procedure DelegateOnExitTab(TabIndex: Integer); (*static;*)

    procedure DoOnTabEnter; dynamic;
    procedure DoOnTabExit; dynamic;

    procedure ActivateFirst; override;
    procedure DeactivateLast; override;

    procedure OnTabEnterControlClick
      (TabControl: TCustomSDVTabs; TabIndex: Integer);
    procedure OnTabExitControlClick
      (TabControl: TCustomSDVTabs; TabIndex: Integer);
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* public declarations *)

    property Tabs: TStrings
      read getTabs write setTabs;
    property TabIndex: Integer
      read getTabIndex write setTabIndex;

    property OnTabEnter: TOnTabControlEvent
      read FOnTabEnter write FOnTabEnter;
    property OnTabExit: TOnTabControlEvent
      read FOnTabExit write FOnTabExit;
  end;

(* TSDVPanelTabControl *)

  TSDVPanelTabControl = class(TCustomSDVPanelTabControl)
  published
    (* published declarations *)

    property Tabs;
    property TabIndex;

    (* TCustomSDVPanelTabControl: *)

    property OnTabEnter;
    property OnTabExit;
  end;

implementation

(* TCustomSDVPanelTabControlNavigator *)

procedure TCustomSDVPanelTabControlNavigator.ClickFirst();
begin
  TabControl.TabIndex := 0;
  (* Goal: Do "First" button action  .*)
  (* Objetivo: Realizar accion de boton "Primero" .*)
end;

procedure TCustomSDVPanelTabControlNavigator.ClickPrior();
begin
  TabControl.TabIndex := Pred(TabControl.TabIndex);
  (* Goal: Do "Prior" button action  .*)
  (* Objetivo: Realizar accion de boton "Anterior" .*)
end;

procedure TCustomSDVPanelTabControlNavigator.ClickNext();
begin
  TabControl.TabIndex := Succ(TabControl.TabIndex);
  (* Goal: Do "Next" button action  .*)
  (* Objetivo: Realizar accion de boton "Siguiente" .*)
end;

procedure TCustomSDVPanelTabControlNavigator.ClickLast();
begin
  TabControl.TabIndex := Pred(TabControl.Tabs.Count);
  (* Goal: Do "Last" button action  .*)
  (* Objetivo: Realizar accion de boton "Ultimo" .*)
end;

(* TCustomSDVPanelTabControl *)

function TCustomSDVPanelTabControl.getTabs(): TStrings;
begin
  Result := FTabs.Tabs;
end;

function TCustomSDVPanelTabControl.getTabIndex(): Integer;
begin
  Result := FTabs.TabIndex;
end;

procedure TCustomSDVPanelTabControl.setTabs(const Value: TStrings);
begin
  FTabs.Tabs.Assign(Value);
end;

procedure TCustomSDVPanelTabControl.setTabIndex(const Value: Integer);
begin
  if (Value > NoIndex) and (Value < (FTabs.Tabs.Count))
    then FTabs.TabIndex := Value;
end;

procedure TCustomSDVPanelTabControl.DelegateOnEnterTab(TabIndex: Integer);
begin
  if (Assigned(FOnTabEnter))
    then FOnTabEnter(Self, TabIndex);
  (* Goal: To execute the "OnEnterTab" event .*)
  (* Objetivo: Ejecutar el evento "OnEnterTab" .*)
end;

procedure TCustomSDVPanelTabControl.DelegateOnExitTab(TabIndex: Integer);
begin
  if (Assigned(FOnTabExit))
    then FOnTabExit(Self, TabIndex);
  (* Goal: To execute the "OnExitTab" event .*)
  (* Objetivo: Ejecutar el evento "OnExitTab" .*)
end;

procedure TCustomSDVPanelTabControl.DoOnTabEnter;
begin
  DelegateOnEnterTab(TabIndex);
end;

procedure TCustomSDVPanelTabControl.DoOnTabExit;
begin
  DelegateOnExitTab(TabIndex);
end;

procedure TCustomSDVPanelTabControl.ActivateFirst;
begin
  (*Your Code...*)
end;

procedure TCustomSDVPanelTabControl.DeactivateLast;
begin
  (*Your Code...*)
end;

procedure TCustomSDVPanelTabControl.OnTabEnterControlClick
  (TabControl: TCustomSDVTabs; TabIndex: Integer);
begin
  (*Your Code...*)
end;

procedure TCustomSDVPanelTabControl.OnTabExitControlClick
  (TabControl: TCustomSDVTabs; TabIndex: Integer);
begin
  (*Your Code...*)
end;

constructor TCustomSDVPanelTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 193;
  Self.Width  := 289;
  Self.BevelInner := bvNone;
  Self.BevelOuter := bvNone;
//  Self.Color := clGray;

  FHeader := TCustomSDVPanel.Create(Self);
  FHeader.Align  := alTop;
  FHeader.Height := 21;
  FHeader.BevelInner := bvNone;
  FHeader.BevelOuter := bvNone;

  FNavigator  := TCustomSDVPanelTabControlNavigator.Create(FHeader);
  FNavigator.Align := alRight;
  FNavigator.Width := 88;
  FNavigator.ShowCaptions:=true;

  FNavigator.ButtonsVisible :=
  [
    ord(btnopFirst),
    ord(btnopPrior),
    ord(btnopNext),
    ord(btnopLast)
  ];

  FNavigator.TabControl := Self;

  FTabs       := TCustomSDVTabs.Create(FHeader);
  FTabs.Align := alClient;
  FTabs.Width := 201;
  FTabs.OnEnterTab := {$IFNDEF DELPHI}@{$ENDIF}OnTabEnterControlClick;
  FTabs.OnExitTab  := {$IFNDEF DELPHI}@{$ENDIF}OnTabExitControlClick;

  Self.InsertControl(FHeader);
  FHeader.InsertControl(FTabs);
  FHeader.InsertControl(FNavigator);
end;

destructor TCustomSDVPanelTabControl.Destroy();
begin
  (*Your Code...*)
  inherited Destroy();
end;

end.
 
