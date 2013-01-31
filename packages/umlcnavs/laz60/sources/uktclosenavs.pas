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

unit uktclosenavs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Controls,
  uktnormpanels,
  ukteventargs,
  uktevargbtns,
  uktclosenavenums,
  uktfixedbtnpanels,
  uktclosenavstrs,
  dummy;

type
  CloseButtonTypes =
  (
      None,   // it's not assigned
      OK,     // select current item & exit form
      Cancel, // don't current select item & exit form
      Exit    // just exit form
  ); // enum CloseButtonTypes

type

(* TSDVCloseNavigatorButtonEventArguments *)

  TSDVCloseNavigatorButtonEventArguments = class(TSDVEventArguments)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

   FButtonType: CloseButtonTypes;
  public
    (* Public declarations *)

    function readButtonType(): CloseButtonTypes;
  public
    (* Public declarations *)

    property ButtonType: CloseButtonTypes
      read FButtonType write FButtonType;
  end;

type

(* TSDVInternalCloseNavigatorButton *)

  TSDVCustomCloseNavigator = class;

  TSDVInternalCloseNavigatorButton = class(TCustomSDVEventButton)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FButtonType: CloseButtonTypes;
    FNavigator:  TSDVCustomCloseNavigator;
  public
    (* Public declarations *)

    function readArgs(): TSDVEventArguments; override;

    function readButtonType(): CloseButtonTypes;
  public
    (* Public declarations *)

    constructor CreateButtonType
      (AOwner: TComponent; const AButtonType: CloseButtonTypes);
  public
    (* Public declarations *)

    property Navigator:  TSDVCustomCloseNavigator
      read FNavigator write FNavigator;
  end;

  TSDVBeforeCloseEventHandler =
    function (sender: TObject; Args: TObject): Boolean;
  TSDVAfterCloseEventHandler =
    procedure (sender: TObject; Args: TObject);

(* TSDVCustomCloseNavigator *)

  /// <summary>
  /// Es una barra de botones que despliega botones para cerrar el control contenedor,
  /// generalmente una forma.
  /// Permite seleccionar cuales son los botones que apareceran,
  /// para cerrar la forma o panel,
  /// "OK" y "Cancel" para cerrar, seleccionando un elemento,
  /// o "Close" para salir sin seleccionar.
  /// </summary>
  TSDVCustomCloseNavigator = class(TSDVCustomFixedButtonPanel)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FInternalBtnOK:     TSDVInternalCloseNavigatorButton;
    FInternalBtnCancel: TSDVInternalCloseNavigatorButton;
    FInternalBtnExit:   TSDVInternalCloseNavigatorButton;
  protected
    (* Protected declarations *)

    FCloseMode: TSDVNavigatorCloseModes;
  protected
    (* Protected declarations *)

    function getCloseMode(): TSDVNavigatorCloseModes;
    procedure setCloseMode(const AValue: TSDVNavigatorCloseModes); virtual;
  protected
    (* Protected declarations *)

    // close and select current item
   FOnBeforeCloseOK:     TSDVBeforeCloseEventHandler;
   // close without select current item
   FOnBeforeCloseCancel: TSDVBeforeCloseEventHandler;
   // just close the form
   FOnBeforeCloseExit:   TSDVBeforeCloseEventHandler;

   // close and select current item
   FOnAfterCloseOK:     TSDVAfterCloseEventHandler;
   // close without select current item
   FOnAfterCloseCancel: TSDVAfterCloseEventHandler;
   // just close the form
   FOnAfterCloseExit:   TSDVAfterCloseEventHandler;
  protected
    (* Protected declarations *)

    function getIsButtonVisible
      (thisButton: CloseButtonTypes): Boolean;

    function readVisibleButtonsCount(): Integer;

    function canRebuiltControl(): Boolean; override;
    procedure createContainerControls(); override;

    function SelectDefaultButtonText
      (Args: TSDVEventArguments): string;
    function selectDefaultButtonImage
      (Args: TSDVEventArguments): TBitmap;
  public
    (* Public declarations *)

    /// <summary>
    /// Selecciona cuales son los botones que apareceran,
    /// para cerrar la forma o panel,
    /// "OK" y "Cancel" para cerrar, seleccionando un elemento,
    /// o "Close" para salir sin seleccionar.
    /// </summary>
    property CloseMode: TSDVNavigatorCloseModes
      read getCloseMode write setCloseMode;

    // close and select current item
   property OnBeforeCloseOK:     TSDVBeforeCloseEventHandler
     read FOnBeforeCloseOK write FOnBeforeCloseOK;
   // close without select current item
   property OnBeforeCloseCancel: TSDVBeforeCloseEventHandler
     read FOnBeforeCloseCancel write FOnBeforeCloseCancel;
   // just close the form
   property OnBeforeCloseExit:   TSDVBeforeCloseEventHandler
     read FOnBeforeCloseExit write FOnBeforeCloseExit;

   // close and select current item
   property OnAfterCloseOK:     TSDVAfterCloseEventHandler
     read FOnAfterCloseOK write FOnAfterCloseOK;
   // close without select current item
   property OnAfterCloseCancel: TSDVAfterCloseEventHandler
     read FOnAfterCloseCancel write FOnAfterCloseCancel;
   // just close the form
   property OnAfterCloseExit:   TSDVAfterCloseEventHandler
     read FOnAfterCloseExit write FOnAfterCloseExit;
  end;

implementation

(* TSDVCloseNavigatorButtonEventArguments *)

function
  TSDVCloseNavigatorButtonEventArguments.readButtonType(): CloseButtonTypes;
begin
  Result := Self.FButtonType;
end;

(* TSDVInternalCloseNavigatorButton *)

/// <summary>
/// Generate read-only arguments of the button,
/// for the events / delegators.
/// Returns an argument item for the specific class of the button.
/// </summary>
/// <returns></returns>
function TSDVInternalCloseNavigatorButton.readArgs(): TSDVEventArguments;
var CloseArgs: TSDVCloseNavigatorButtonEventArguments;
begin
  // copiar los datos significativos del boton,
  // a una clase como datos de solo lectura
  CloseArgs :=
    TSDVCloseNavigatorButtonEventArguments.Create();
  CloseArgs.ButtonType := Self.FButtonType;

    //(this._ButtonType, this.Text, this._Data);

  Result := CloseArgs;
end; // StatusButtonEventArgs readArgs(...)

function TSDVInternalCloseNavigatorButton.readButtonType(): CloseButtonTypes;
begin
   Result := FButtonType;
end;

constructor TSDVInternalCloseNavigatorButton.CreateButtonType
  (AOwner: TComponent; const AButtonType: CloseButtonTypes);
begin
  inherited Create(AOwner);
  Self.FButtonType := AButtonType;
end;

(* TSDVCustomCloseNavigator *)

function TSDVCustomCloseNavigator.getCloseMode(): TSDVNavigatorCloseModes;
begin
  Result := FCloseMode;
end;

procedure TSDVCustomCloseNavigator.setCloseMode
  (const AValue: TSDVNavigatorCloseModes);
begin
  FCloseMode := AValue;
end;

/// <summary>
/// Metodo de acceso para lectura para las propiedades,
/// que indican si cada boton es visible.
/// </summary>
/// <param name="thisButton">Valor enumerado que identifica al boton.</param>
/// <returns>Estatus actual de visibilidad</returns>
function TSDVCustomCloseNavigator.getIsButtonVisible
  (thisButton: CloseButtonTypes): Boolean;
begin
  Result := false;

  // nota: el valor enumerado "None" no aplica
  if (thisButton <> sdvclosenavs.None) then
  begin
    case (thisButton) of
      sdvclosenavs.OK:
        Result := (Self.CloseMode = sdvclosenavenums.OKCancel);
      sdvclosenavs.Cancel:
        Result := (Self.CloseMode = sdvclosenavenums.OKCancel);
      sdvclosenavs.Exit:
        Result := (Self.CloseMode = sdvclosenavenums.Exit);
    end; // case
  end;
end; // bool getIsButtonVisible(...)

function TSDVCustomCloseNavigator.readVisibleButtonsCount(): Integer;
begin
  Result := 0;

  case (Self.CloseMode) of
    sdvclosenavenums.OKCancel:
      Result := 2;
    sdvclosenavenums.Exit:
      Result := 1;
  end; // switch
end; // int readVisibleButtonsCount(...)

/// <summary>
/// Returns if the operation can be performed.
/// </summary>
/// <returns>Can rebuilt</returns>
function TSDVCustomCloseNavigator.canRebuiltControl(): Boolean;
begin
  Result := ((Self.Height > 0) and (Self.Width > 0));
end;

/// <summary>
/// Create the panel that contain the buttons.
/// </summary>
procedure TSDVCustomCloseNavigator.createContainerControls();
var ButtonsPanelLocation: TPoint;
begin
  ButtonsPanelLocation.X := 0;
  ButtonsPanelLocation.Y := 0;

  // --> agregar panel que contendra a los botones para opciones
  Self.FInternalItemsPanel := TCustomSDVNormalizedPanel.Create(Self);
    //FInternalItemsPanel.Color := clTransparent;

    FInternalItemsPanel.Top  := ButtonsPanelLocation.Y;
    FInternalItemsPanel.Left := ButtonsPanelLocation.X;

    FInternalItemsPanel.Width  := Self.Width;
    FInternalItemsPanel.Height := Self.Height;

    FInternalItemsPanel.Align := alClient;
  Self.InsertControl(FInternalItemsPanel);
end;

function TSDVCustomCloseNavigator.SelectDefaultButtonText
  (Args: TSDVEventArguments): string;
var CloseArgs: TSDVCloseNavigatorButtonEventArguments;
    thisButtonType: CloseButtonTypes;
begin
  Result := '';

  CloseArgs := (Args as TSDVCloseNavigatorButtonEventArguments);
  thisButtonType := CloseArgs.readButtonType();

  case (thisButtonType) of
    sdvclosenavs.OK:
       Result := resCloseButtonTypes_OK;
    sdvclosenavs.Cancel:
       Result := resCloseButtonTypes_Cancel;
    sdvclosenavs.Exit:
       Result := resCloseButtonTypes_Exit;
    else
      Result := resCloseButtonTypes_None;
  end; // switch
end; // string selectDefaultButtonText(...)

function TSDVCustomCloseNavigator.selectDefaultButtonImage
  (Args: TSDVEventArguments): TBitmap;
var CloseArgs: TSDVCloseNavigatorButtonEventArguments;
    thisButtonType: CloseButtonTypes;
begin
  Result := TBitmap.Create();

  CloseArgs := (Args as TSDVCloseNavigatorButtonEventArguments);
  thisButtonType := CloseArgs.readButtonType();

  case (thisButtonType) of
    sdvclosenavs.OK:
      begin
        {$IFDEF FPC}
        Result.LoadFromLazarusResource('clnv_ok.png');
        {$ENDIF}
        {$IFDEF DELPHI}
        Result.LoadFromResourceName('clnv_ok.png');
        {$ENDIF}
      end;
    sdvclosenavs.Cancel:
      begin
        {$IFDEF FPC}
        Result.LoadFromLazarusResource('clnv_cancel.png');
        {$ENDIF}
        {$IFDEF DELPHI}
        Result.LoadFromResourceName('clnv_cancel.png');
        {$ENDIF}
      end;
    sdvclosenavs.Exit:
      begin
        {$IFDEF FPC}
        Result.LoadFromLazarusResource('clnv_exit.png');
        {$ENDIF}
        {$IFDEF DELPHI}
        Result.LoadFromResourceName('clnv_exit.png');
        {$ENDIF}
      end;
    else
      begin
        {$IFDEF FPC}
        Result.LoadFromLazarusResource('clnv_none.png');
        {$ENDIF}
        {$IFDEF DELPHI}
        Result.LoadFromResourceName('clnv_none.png');
        {$ENDIF}
      end;
  end; // switch
end; // Bitmap selectDefaultButtonImage(...)

end.

