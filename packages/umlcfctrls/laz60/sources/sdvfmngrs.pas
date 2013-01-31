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
unit sdvfmngrs;

interface
uses
  SysUtils, Classes,
{.IFDEF MSWINDOWS}
  Graphics, Controls, StdCtrls,
{.ENDIF}
  uktguidstrs,
  uktcomponents,
  uktmsgtypes, uktmsgctrls,
  uktdoscolors,
  dummy;

// default messages

const
  msgFocusMngrCharCaseChanged
    = '{B6F5BCC4-407D5C44-AC4B9307-80A9B3A6}';

  msgFocusMngrDefaultColorChanged
    = '{CF59A04C-FF028540-9C2ED6EE-D19E1532}';
  msgFocusMngrDefaultFontColorChanged
    = '{8633B12A-C4921149-B4B92A80-98E5EB24}';
  msgFocusMngrDefaultFontNameChanged
    = '{BD42C5C9-1FE07942-A2E7B80D-36E28DB7}';
  msgFocusMngrDefaultFontSizeChanged
    = '{E8D02E41-4C368844-A8645E87-5AC9F737}';

  msgFocusMngrFocusedColorChanged
    = '{065D9221-E3DF5D49-BBB23FE0-54ADEA76}';
  msgFocusMngrFocusedFontColorChanged
    = '{C1E47DE9-1BDB5743-837DC1BE-CA1A9888}';
  msgFocusMngrFocusedFontNameChanged
    = '{19C2EEEF-47DAD64A-95947E3C-C68AE5B0}';
  msgFocusMngrFocusedFontSizeChanged
    = '{52F8BC86-2E1B1448-94CB64C0-D0351A60}';

  msgFocusMngrDisabledColorChanged
    = '{D2AD4E26-EF9A604D-A15D37B1-CD32C676}';
  msgFocusMngrDisabledFontColorChanged
    = '{64656CC0-90616649-9336CD4D-3A93AD66}';
  msgFocusMngrDisabledFontNameChanged
    = '{13014FE7-98D00142-A3034E3E-2039BE3C}';
  msgFocusMngrDisabledFontSizeChanged
    = '{905DE5E7-B1890E42-A3906D4C-EB126DA9}';

  msgFocusMngrReadOnlyColorChanged
    = '{34491779-BEB96044-A8A42A08-84142D10}';
  msgFocusMngrReadOnlyFontColorChanged
    = '{D7E810FE-15E5BC4B-9A1F5413-DA6065F3}';
  msgFocusMngrReadOnlyFontNameChanged
    = '{97041F68-0CE96349-A6CAD5BE-07267738}';
  msgFocusMngrReadOnlyFontSizeChanged
    = '{069ACBED-6B100745-98D7594F-A27EA0A5}';

  msgFocusMngrClear
    = '{9ACE7316-F11A594B-B3E5A57C-B1D23EB8}';
  msgFocusMngrRefresh
    = '{09655E24-5925AE4B-87E9124C-010AAA82}';

type

  TDOSColorPair = record
    Color:    TColor;
    DOSColor: sdvdoscolors.doscolor;
  end;

  TRefreshParams = record
    Color:     TColor;
    FontColor: TColor;
    FontName:  Integer;
    FontSize:  string;
  end;

  (* TCustomSDVFocusableControlMngr *)

  TCustomSDVFocusableControlMngr = class(TSDVMsgServer)
  private
    (* Private declarations *)
  protected
  (* Protected declarations *)

    FAutoForward: Boolean;
    // Enfocar siguiente control al presionar <Enter> ?
    // Focus next control when <Enter> is pressed ?

    FAutoSelect: Boolean;
    // Seleccionar texto al obtener foco ?
    // Select text when gain focus ?

    FAutoTab: Boolean;
    // Enfoca el siguiente control al alcanzar al limite de caracteres?
    // Focus next control when chars limit is reached ?

    FEnabled: Boolean;
    // Puede cambiarse el color ?
    // Can the color be changed ?

    FCharCase: TEditCharCase;

    // default background color
    FGraphicDefaultColor: TColor;
    // default font attributes, including foreground color (text color)
    FDefaultFont:  TFont;

    // focused background color
    FGraphicFocusedColor: TColor;
    // focused font attributes, including foreground color (text color)
    FFocusedFont:  TFont;

    // disabled background color
    FGraphicDisabledColor: TColor;
    // disabled font attributes, including foreground color (text color)
    FDisabledFont:  TFont;

    // read-only background color
    FGraphicReadOnlyColor: TColor;
    // read-only font attributes, including foreground color (text color)
    FReadOnlyFont:  TFont;
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    function getDefaultFontName(): string;
    function getDefaultFontSize(): Integer;

    function getFocusedFontName(): string;
    function getFocusedFontSize(): Integer;

    function getDisabledFontName(): string;
    function getDisabledFontSize(): Integer;

    function getReadOnlyFontName(): string;
    function getReadOnlyFontSize(): Integer;

    procedure setCharCase(AValue: TEditCharCase);

    procedure setDefaultFontName(AValue: string);
    procedure setDefaultFontSize(AValue: Integer);

    procedure setFocusedFontName(AValue: string);
    procedure setFocusedFontSize(AValue: Integer);

    procedure setDisabledFontName(AValue: string);
    procedure setDisabledFontSize(AValue: Integer);

    procedure setReadOnlyFontName(AValue: string);
    procedure setReadOnlyFontSize(AValue: Integer);
  public
    (* Public declarations *)

    function readGraphicDefaultColor(): TColor;
    function readGraphicDefaultFontColor(): TColor;

    function readGraphicFocusedColor(): TColor;
    function readGraphicFocusedFontColor(): TColor;

    function readGraphicDisabledColor(): TColor;
    function readGraphicDisabledFontColor(): TColor;

    function readGraphicReadOnlyColor(): TColor;
    function readGraphicReadOnlyFontColor(): TColor;
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    procedure Clear(); virtual;
    procedure Refresh(); virtual;
  public
    (* Public declarations *)

    (* UnPublished declarations *)

    property AutoForward: Boolean
      read FAutoForward write FAutoForward default FALSE;
    property AutoSelect: Boolean
      read FAutoSelect write FAutoSelect default FALSE;
    property AutoTab: Boolean
      read FAutoTab write FAutoTab default FALSE;
    property CharCase: TEditCharCase
      read FCharCase write setCharCase default ecNormal;
    property Enabled: Boolean
      read FEnabled write FEnabled default TRUE;

    property DefaultFontName: string
      read getDefaultFontName write setDefaultFontName;
    property DefaultFontSize: Integer
      read getDefaultFontSize write setDefaultFontSize;

    property FocusedFontName: string
      read getFocusedFontName write setFocusedFontName;
    property FocusedFontSize: Integer
      read getFocusedFontSize write setFocusedFontSize;

    property DisabledFontName: string
      read getDisabledFontName write setDisabledFontName;
    property DisabledFontSize: Integer
      read getDisabledFontSize write setDisabledFontSize;

    property ReadOnlyFontName: string
      read getReadOnlyFontName write setReadOnlyFontName;
    property ReadOnlyFontSize: Integer
      read getReadOnlyFontSize write setReadOnlyFontSize;
  end;

  (* TCustomSDVDOSFocusMngr *)

  TCustomSDVDOSFocusMngr = class(TCustomSDVFocusableControlMngr)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    FDefaultDOSColor: sdvdoscolors.doscolor;
    FDefaultDOSFontColor: sdvdoscolors.doscolor;

    FFocusedDOSColor: sdvdoscolors.doscolor;
    FFocusedDOSFontColor: sdvdoscolors.doscolor;

    FDisabledDOSColor: sdvdoscolors.doscolor;
    FDisabledDOSFontColor: sdvdoscolors.doscolor;

    FReadOnlyDOSColor: sdvdoscolors.doscolor;
    FReadOnlyDOSFontColor: sdvdoscolors.doscolor;
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    procedure setDefaultColor(AValue: sdvdoscolors.doscolor);
    procedure setDefaultFontColor(AValue: sdvdoscolors.doscolor);

    procedure setFocusedColor(AValue: sdvdoscolors.doscolor);
    procedure setFocusedFontColor(AValue: sdvdoscolors.doscolor);

    procedure setDisabledColor(AValue: sdvdoscolors.doscolor);
    procedure setDisabledFontColor(AValue: sdvdoscolors.doscolor);

    procedure setReadOnlyColor(AValue: sdvdoscolors.doscolor);
    procedure setReadOnlyFontColor(AValue: sdvdoscolors.doscolor);
  public
    (* Public declarations *)

    //constructor Create(AOwner: TComponent); override;
    //destructor Destroy(); override;
  public
    (* Public declarations *)

    (* UnPublished declarations *)

    property DefaultColor: sdvdoscolors.doscolor
      read FDefaultDOSColor write setDefaultColor;
    property DefaultFontColor: sdvdoscolors.doscolor
      read FDefaultDOSFontColor write setDefaultFontColor;

    property FocusedColor: sdvdoscolors.doscolor
      read FFocusedDOSColor write setFocusedColor;
    property FocusedFontColor: sdvdoscolors.doscolor
      read FFocusedDOSFontColor write setFocusedFontColor;

    property DisabledColor: sdvdoscolors.doscolor
      read FDisabledDOSColor write setDisabledColor;
    property DisabledFontColor: sdvdoscolors.doscolor
      read FDisabledDOSFontColor write setDisabledFontColor;

    property ReadOnlyColor: sdvdoscolors.doscolor
      read FReadOnlyDOSColor write setReadOnlyColor;
    property ReadOnlyFontColor: sdvdoscolors.doscolor
      read FReadOnlyDOSFontColor write setReadOnlyFontColor;
  end;

  (* TCustomSDVFocusMngr *)

  TCustomSDVFocusMngr = class(TCustomSDVFocusableControlMngr)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

    (* Accesors declarations *)

    function getDefaultColor(): TColor;
    function getDefaultFontColor(): TColor;

    function getFocusedColor(): TColor;
    function getFocusedFontColor(): TColor;

    function getDisabledColor(): TColor;
    function getDisabledFontColor(): TColor;

    function getReadOnlyColor(): TColor;
    function getReadOnlyFontColor(): TColor;

    procedure setDefaultColor(AValue: TColor);
    procedure setDefaultFontColor(AValue: TColor);

    procedure setDisabledColor(AValue: TColor);
    procedure setDisabledFontColor(AValue: TColor);

    procedure setFocusedColor(AValue: TColor);
    procedure setFocusedFontColor(AValue: TColor);

    procedure setReadOnlyColor(AValue: TColor);
    procedure setReadOnlyFontColor(AValue: TColor);
  public
    (* Public declarations *)

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  public
    (* Public declarations *)

    (* UnPublished declarations *)

    property DefaultColor: TColor
      read getDefaultColor write setDefaultColor;
    property DefaultFontColor: TColor
      read getDefaultFontColor write setDefaultFontColor;

    property FocusedColor: TColor
      read getFocusedColor write setFocusedColor default Graphics.clNavy;
    property FocusedFontColor: TColor
      read getFocusedFontColor write setFocusedFontColor;

    property DisabledColor: TColor
      read getDisabledColor write setDisabledColor default Graphics.clNavy;
    property DisabledFontColor: TColor
      read getDisabledFontColor write setDisabledFontColor;

    property ReadOnlyColor: TColor
      read getReadOnlyColor write setReadOnlyColor default Graphics.clNavy;
    property ReadOnlyFontColor: TColor
      read getReadOnlyFontColor write setReadOnlyFontColor;
  end;

  (* TSDVDOSFocusMngr *)

  TSDVDOSFocusMngr = class(TCustomSDVDOSFocusMngr)
  published
    (* Published declarations *)

    property AutoForward;
    property AutoTab;
    property AutoSelect;
    property Enabled;
    property CharCase;

    property DefaultColor;
    property DefaultFontColor;
    property DefaultFontName;
    property DefaultFontSize;

    property FocusedColor;
    property FocusedFontColor;
    property FocusedFontName;
    property FocusedFontSize;

    property DisabledColor;
    property DisabledFontColor;
    property DisabledFontName;
    property DisabledFontSize;

    property ReadOnlyColor;
    //property ReadOnlyFontColor;
    property ReadOnlyFontName;
    property ReadOnlyFontSize;
  end;

(* TSDVFocusMngr *)

  TSDVFocusMngr = class(TCustomSDVFocusMngr)
  published
    (* Published declarations *)

    property AutoForward;
    property AutoTab;    
    property AutoSelect;
    property Enabled;
    property CharCase;

    property DefaultColor;
    property DefaultFontColor;
    property DefaultFontName;
    property DefaultFontSize;

    property FocusedColor;
    property FocusedFontColor;
    property FocusedFontName;
    property FocusedFontSize;

    property DisabledColor;
    property DisabledFontColor;
    property DisabledFontName;
    property DisabledFontSize;

    property ReadOnlyColor;
    property ReadOnlyFontColor;
    property ReadOnlyFontName;
    property ReadOnlyFontSize;
  end;

  function DefaultForegroundColor(): TColor;
  function DefaultBackgroundColor(): TColor;

  function DefaultFontName(): string;
  function DefaultFontSize(): Integer;

  function NextControl
   (CurForm, CurControl: TWinControl;
    GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;

implementation
var FDefaultFont: TFont;

function DefaultForegroundColor(): TColor;
begin
  Result := Graphics.clWindowText;
end;

function DefaultBackgroundColor(): TColor;
begin
  Result := Graphics.clBackground;
end;

function DefaultFontName(): string;
begin
  Result := FDefaultFont.Name;
end;

function DefaultFontSize(): Integer;
begin
  Result := FDefaultFont.Size;
end;

function NextControl
 (CurForm, CurControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: Boolean): TWinControl;
var
  I, StartIndex: Integer;
  {$IFDEF DELPHI}
  List: TList;
  {$ENDIF}
  {$IFDEF FPC}
  List: TFPList;
  {$ENDIF}
begin
  Result := nil;
  //crear una lista para obtener el taborderlist;
  {$IFDEF DELPHI}
  List := TList.Create();
  {$ENDIF}
  {$IFDEF FPC}
  List := TFPList.Create();
  {$ENDIF}
  try
    //Obtener la del taborderlist de la forma del componente;
    CurForm.GetTabOrderList(List);
    //verificar que la lista no este vacia
    if (List.Count > 0) then
    begin
      //Posicionar el indice en la posicion del componenete actual
      StartIndex := List.IndexOf(CurControl);
      if (StartIndex = -1) then
        if GoForward then StartIndex := List.Count - 1 else StartIndex := 0;
      I := StartIndex;
      repeat
        //Si buscamos el componente hacia adelante
        if (GoForward) then
        begin
          Inc(I);
          //si el componente que buscamos es el primero
          if (I = List.Count) then I := 0;
        end else
        //Si buscamos el componente hacia atras
        begin
          //si el componente que buscamos es el último
          if (I = 0) then I := List.Count;
          Dec(I);
        end;

        //asignar al control actual el siguiente control encontrado
        CurControl := TWinControl(List[I]);

        //verificar que el control se pueda enfocar y que no tenga tabstop
        if ((CurControl.CanFocus) and
          (not CheckTabStop or CurControl.TabStop) and
          (not CheckParent)) then
          Result := CurControl;
      until ((Result <> nil) or (I = StartIndex));
    end;
  finally
    List.Free();
  end;
  (* Goal: To get the next control from the "taborderlist" .*)
  (* Objetivo: Obtener el siguiente control de la "taborderlist" .*)
end;

(* TCustomSDVDOSFocusMngr *)

procedure TCustomSDVDOSFocusMngr.setDefaultColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FDefaultDOSColor <> AValue) then
  begin
    Self.FDefaultDOSColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;
    Self.FGraphicDefaultColor := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDefaultColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setDefaultFontColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FDefaultDOSFontColor <> AValue) then
  begin
    Self.FDefaultDOSFontColor := AValue;

    ADOSPair.Color := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;
    Self.FDefaultFont.Color := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDefaultFontColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setFocusedColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FFocusedDOSColor <> AValue) then
  begin
    Self.FFocusedDOSColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FGraphicFocusedColor := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrFocusedColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setFocusedFontColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FFocusedDOSFontColor <> AValue) then
  begin
    Self.FFocusedDOSFontColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FFocusedFont.Color := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrFocusedFontColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setDisabledColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FDefaultDOSColor <> AValue) then
  begin
    Self.FDisabledDOSColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FGraphicDisabledColor := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDisabledColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setDisabledFontColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FDisabledDOSFontColor <> AValue) then
  begin
    Self.FDisabledDOSFontColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FDisabledFont.Color := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDisabledFontColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setReadOnlyColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FReadOnlyDOSColor <> AValue) then
  begin
    Self.FReadOnlyDOSColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FGraphicReadOnlyColor := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrReadOnlyColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

procedure TCustomSDVDOSFocusMngr.setReadOnlyFontColor
  (AValue: sdvdoscolors.doscolor);
var AMsgRec: TSDVMessageParamsRecord;
    ADOSPair: TDOSColorPair;
begin
  if (Self.FReadOnlyDOSFontColor <> AValue) then
  begin
    Self.FReadOnlyDOSFontColor := AValue;

    ADOSPair.Color    := sdvdoscolors.DOSColorToColor(AValue);
    ADOSPair.DOSColor := AValue;

    Self.FReadOnlyFont.Color := ADOSPair.Color;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrReadOnlyFontColorChanged, AMsgRec.Message);

    AMsgRec.Sender := Self;
    AMsgRec.Param  := @ADOSPair;

    SendMessage(AMsgRec);
  end;
end;

(*
constructor TCustomSDVDOSFocusMngr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomSDVDOSFocusMngr.Destroy();
begin
  inherited Destroy();
end;
*)

(* TCustomSDVFocusableControlMngr *)

function TCustomSDVFocusableControlMngr.getDefaultFontName(): string;
begin
  Result := Self.FDefaultFont.Name;
end;

function TCustomSDVFocusableControlMngr.getDefaultFontSize(): Integer;
begin
  Result := Self.FDefaultFont.Size;
end;

function TCustomSDVFocusableControlMngr.getFocusedFontName(): string;
begin
  Result := Self.FFocusedFont.Name;
end;

function TCustomSDVFocusableControlMngr.getFocusedFontSize(): Integer;
begin
  Result := Self.FFocusedFont.Size;
end;

function TCustomSDVFocusableControlMngr.getDisabledFontName(): string;
begin
  Result := Self.FDisabledFont.Name;
end;

function TCustomSDVFocusableControlMngr.getDisabledFontSize(): Integer;
begin
  Result := Self.FDisabledFont.Size;
end;

function TCustomSDVFocusableControlMngr.getReadOnlyFontName(): string;
begin
  Result := Self.FReadOnlyFont.Name;
end;

function TCustomSDVFocusableControlMngr.getReadOnlyFontSize(): Integer;
begin
  Result := Self.FReadOnlyFont.Size;
end;

procedure TCustomSDVFocusableControlMngr.setDefaultFontName(AValue: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Name := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultFontNameChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setDefaultFontSize(AValue: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Size := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrDefaultFontSizeChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setFocusedFontName
  (AValue: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Name := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedFontNameChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setFocusedFontSize
  (AValue: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Size := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrFocusedFontSizeChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setDisabledFontName
  (AValue: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Name := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledFontNameChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setDisabledFontSize
  (AValue: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Size := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrDisabledFontSizeChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setReadOnlyFontName
  (AValue: string);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Name := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyFontNameChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.setReadOnlyFontSize
  (AValue: Integer);
var AMsgRec: TSDVMessageParamsRecord;
begin
  Self.FDefaultFont.Size := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrReadOnlyFontSizeChanged, AMsgRec.Message);

  AMsgRec.Sender := Self;
  AMsgRec.Param  := @AValue;

  SendMessage(AMsgRec);
end;

function TCustomSDVFocusableControlMngr.readGraphicDefaultColor(): TColor;
begin
  Result := FGraphicDefaultColor;
end;

function TCustomSDVFocusableControlMngr.readGraphicDefaultFontColor: TColor;
begin
  Result := FDefaultFont.Color;
end;

function TCustomSDVFocusableControlMngr.readGraphicFocusedColor(): TColor;
begin
  Result := FGraphicFocusedColor;
end;

function TCustomSDVFocusableControlMngr.readGraphicFocusedFontColor: TColor;
begin
  Result := FFocusedFont.Color;
end;

function TCustomSDVFocusableControlMngr.readGraphicDisabledColor(): TColor;
begin
  Result := FGraphicDisabledColor;
end;

function TCustomSDVFocusableControlMngr.readGraphicDisabledFontColor: TColor;
begin
  Result := FDisabledFont.Color;
end;

function TCustomSDVFocusableControlMngr.readGraphicReadOnlyColor(): TColor;
begin
  Result := FGraphicReadOnlyColor;
end;

function TCustomSDVFocusableControlMngr.readGraphicReadOnlyFontColor: TColor;
begin
  Result := FReadOnlyFont.Color;
end;

procedure TCustomSDVFocusableControlMngr.setCharCase(AValue: TEditCharCase);
var AMsgRec: TSDVMessageParamsRecord;
begin
  FCharCase := AValue;

  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrCharCaseChanged, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := @AValue;

  SendMessage(AMsgRec);
  { Goal: "CharCase" property get method .}
  { Objetivo: Metodo lectura para propiedad "CharCase" .}
end;

constructor TCustomSDVFocusableControlMngr.Create(AOwner: TComponent);
begin
  inherited.Create(AOwner);
  // Call TComponent.Create;
  // Llamar TComponent.Create;

  FAutoForward       := FALSE;
  FAutoSelect        := FALSE;
  FAutoTab           := FALSE;
  FEnabled           := TRUE;
  FCharCase          := ecNormal;

  FGraphicDefaultColor      := Graphics.clWindow;
  FGraphicFocusedColor      := Graphics.clNavy;
  FGraphicDisabledColor     := Graphics.clSilver;
  FGraphicReadOnlyColor     := Graphics.clSilver;

  FDefaultFont              := TFont.Create();
  FDefaultFont.Color        := Graphics.clWindowText;

  FFocusedFont       := TFont.Create();
  FFocusedFont.Color := Graphics.clLime;

  FDisabledFont       := TFont.Create();
  FDisabledFont.Color := Graphics.clBlack;

  FReadOnlyFont       := TFont.Create();
  FReadOnlyFont.Color := Graphics.clBlack;
  // Clear the new properties at start
  // Limpiar las nuevas propiedades al inicio

  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVFocusableControlMngr.Destroy();
begin
  FReadOnlyFont.Free();
  FDisabledFont.Free();
  FFocusedFont.Free();
  FDefaultFont.Free();

  FGraphicReadOnlyColor := 0;
  FGraphicDisabledColor := 0;
  FGraphicFocusedColor := 0;
  FGraphicDefaultColor := 0;

  FCharCase := ecNormal;
  FEnabled := FALSE;
  FAutoSelect := FALSE;
  FAutoForward := FALSE;
  // Clear the new properties at finish
  // Limpiar las nuevas propiedades al terminar

  inherited Destroy();
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

procedure TCustomSDVFocusableControlMngr.Clear();
var AMsgRec: TSDVMessageParamsRecord;
begin
  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrClear, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := nil;

  SendMessage(AMsgRec);
end;

procedure TCustomSDVFocusableControlMngr.Refresh();
var AMsgRec: TSDVMessageParamsRecord;
    AParams: TRefreshParams;
begin
  sdvguidstrs.DoubleStrToGUID
    (msgFocusMngrRefresh, AMsgRec.Message);

  AMsgRec.Sender  := Self;
  AMsgRec.Param   := @AParams;

  SendMessage(AMsgRec);
end;

(* TCustomSDVFocusMngr *)

function TCustomSDVFocusMngr.getDefaultColor(): TColor;
begin
  Result := FGraphicDefaultColor;
end;

function TCustomSDVFocusMngr.getDefaultFontColor(): TColor;
begin
  Result := Self.FDefaultFont.Color;
end;

function TCustomSDVFocusMngr.getFocusedColor(): TColor;
begin
  Result := FGraphicFocusedColor;
end;

function TCustomSDVFocusMngr.getFocusedFontColor(): TColor;
begin
  Result := Self.FFocusedFont.Color;
end;

function TCustomSDVFocusMngr.getDisabledColor(): TColor;
begin
  Result := FGraphicDisabledColor;
end;

function TCustomSDVFocusMngr.getDisabledFontColor(): TColor;
begin
  Result := Self.FDisabledFont.Color;
end;

function TCustomSDVFocusMngr.getReadOnlyColor(): TColor;
begin
  Result := FGraphicReadOnlyColor;
end;

function TCustomSDVFocusMngr.getReadOnlyFontColor(): TColor;
begin
  Result := Self.FReadOnlyFont.Color;
end;

procedure TCustomSDVFocusMngr.setDefaultColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (FGraphicDefaultColor <> AValue) then
  begin
    FGraphicDefaultColor := AValue;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDefaultColorChanged, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := @AValue;

    SendMessage(AMsgRec);
  end;
  { Goal: "DefaultColor" property get method .}
  { Objetivo: Metodo lectura para propiedad "DefaultColor" .}
end;

procedure TCustomSDVFocusMngr.setDefaultFontColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (FDefaultFont.Color <> AValue) then
  begin
    FDefaultFont.Color := AValue;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDefaultFontColorChanged, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := @AValue;

    SendMessage(AMsgRec);
  end;
  { Goal: "DefaultFontColor" property get method .}
  { Objetivo: Metodo lectura para propiedad "DefaultFontColor" .}
end;

procedure TCustomSDVFocusMngr.setDisabledColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (FGraphicDisabledColor <> AValue) then
  begin
    FGraphicDisabledColor := AValue;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrDisabledColorChanged, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := @AValue;

    SendMessage(AMsgRec);
  end;
  { Goal: "DisabledColor" property get method .}
  { Objetivo: Metodo lectura para propiedad "DisabledColor" .}
end;

procedure TCustomSDVFocusMngr.setDisabledFontColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin


end;

procedure TCustomSDVFocusMngr.setReadOnlyColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (FGraphicReadOnlyColor <> AValue) then
  begin
    FGraphicReadOnlyColor := AValue;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrReadOnlyColorChanged, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := @AValue;

    SendMessage(AMsgRec);
  end;
  { Goal: "ReadOnlyColor" property get method .}
  { Objetivo: Metodo lectura para propiedad "ReadOnlyColor" .}
end;

procedure TCustomSDVFocusMngr.setReadOnlyFontColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin

end;

procedure TCustomSDVFocusMngr.setFocusedColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin
  if (FGraphicFocusedColor <> AValue) then
  begin
    FGraphicFocusedColor := AValue;

    sdvguidstrs.DoubleStrToGUID
      (msgFocusMngrFocusedColorChanged, AMsgRec.Message);

    AMsgRec.Sender  := Self;
    AMsgRec.Param   := @AValue;

    SendMessage(AMsgRec);
  end;
  { Goal: "FocusedColor" property get method .}
  { Objetivo: Metodo lectura para propiedad "FocusedColor" .}
end;

procedure TCustomSDVFocusMngr.setFocusedFontColor(AValue: TColor);
var AMsgRec: TSDVMessageParamsRecord;
begin

end;

constructor TCustomSDVFocusMngr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Call TComponent.Create;
  // Llamar TComponent.Create;
  // Clear the new properties at start
  // Limpiar las nuevas propiedades al inicio

  { Goal: To prepare the component .}
  { Objetivo: Preparar el componente .}
end;

destructor TCustomSDVFocusMngr.Destroy();
begin
  inherited Destroy();
  { Goal: To unprepare the component .}
  { Objetivo: Despreparar el componente .}
end;

procedure Prepare();
begin
  FDefaultFont := TFont.Create();
end;

procedure UnPrepare();
begin
  FDefaultFont.Free();
end;

initialization
  Prepare();
finalization
  Unprepare();
end.
