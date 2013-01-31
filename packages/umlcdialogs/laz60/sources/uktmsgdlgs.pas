(*****************************************************************************
 *                                                                           *
 *  This file is part of the Star Developers Component Library.              *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktmsgdlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Graphics, Controls, Forms,
{$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  dummy;

  function MessageDlg
    (const AMessage: string;
     AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons;
     const AHlpCtx: Integer): TMsgDlgButton; overload;
  function MessageDlg
    (const AMessage: string;
     AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons): TMsgDlgButton; overload;

  function MessageDlgDef(
    const AMessage: string; AMessageType: TMsgDlgType;
    const AOptions: TMsgDlgButtons;
    const AHlpCtx: Integer;
    DefOption: TMsgDlgButton): TMsgDlgButton; overload;

  function MessageDlgDef(
    const AMessage: string; AMessageType: TMsgDlgType;
    const AOptions: TMsgDlgButtons;
    DefOption: TMsgDlgButton): TMsgDlgButton; overload;

  procedure ErrorDlg(const AMessage: string);

  function ConfirmDlg(const AMessage: string): Boolean;
  function ConfirmDlgDef
    (const AMessage: string; DefOption: TMsgDlgButton): Boolean;

  function ConfirmMultipleDlg(const AMessage: string): TMsgDlgButton;
  function ConfirmMultipleDlgDef
    (const AMessage: string; DefOption: TMsgDlgButton): TMsgDlgButton;

  procedure ShowMessage(const AMessage: string);

implementation
uses
  lazuktfrmmsgdlg;
  
procedure ShowMessage(const AMessage: string);
begin
  MessageDlg(AMessage, mtCustomized, [mbOK], 0);
end;

procedure ArrangeOddNumberButtons
  (const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
var I, ACenter: Integer;
begin
  ACenter := (Succ(ACount) DIV 2);
  AButtons[ACenter].Left := (AWidth Div 2) - (AButtons[1].Width Div 2);

  if (ACenter > 1)
    then
  for i := Pred(ACenter) to 1 do
    AButtons[i].Left := AButtons[Succ(i)].Left - AButtons[i].Width - 6;
  // Acomodar anteriores

  if (ACenter > 1)
    then
  for i := Succ(ACenter) downto ACount do
    AButtons[i].Left := AButtons[Pred(i)].Left + AButtons[Pred(i)].Width + 6;
  // Acomodar posteriores

  { Goal: Acomodar Buttons con cuenta non .}
end;

procedure ArrangePairNumberButtons
  (const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
var I, ACenter: Integer;
begin
  ACenter := (ACount div 2);
  AButtons[ACenter].Left := (AWidth div 2) - AButtons[1].Width - 3;

  if (ACenter > 1)
    then
  for i := Pred(ACenter) to 1 do
    AButtons[i].Left := AButtons[Succ(i)].Left - AButtons[i].Width - 6;
  // arrange previous buttons

  for i := Succ(ACenter) downto ACount do
    AButtons[i].Left := AButtons[Pred(i)].Left + AButtons[Pred(i)].Width + 6;
  // arrange next buttons

  { Goal: Acomodar Buttons con cuenta par .}
end;

procedure ArrangeButtons
  (const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
begin
  if Odd(ACount)
    then ArrangeOddNumberButtons(AWidth, ACount, AButtons)
    else ArrangePairNumberButtons(AWidth, ACount, AButtons);
end;

function InternalMessageDlg(
  const AMessage: string; AMessageType: TMsgDlgType;
  const AOptions: TMsgDlgButtons;
  const AHlpCtx: Integer;
  UseDefault: Boolean;
  DefOption: TMsgDlgButton): TMsgDlgButton;
var
  AWidth, ALeft, ATop: Integer;
  Count: Byte; EachOption: TMsgDlgButton;
  MoveButtons: TIncButtonsArray;
//  ScreenLogPixels: Integer;
//  DC: HDC;

  procedure PrepareButton
    (const AOpcion: TMsgDlgButton; const AButtons: TButtonsArray);
  begin
    with AButtons[AOpcion] do
    if not (AOpcion in AOptions) then
    begin
      Visible := False;
      Enabled := False;
    end else
    begin
      Inc(Count);
      MoveButtons[Count] := AButtons[AOpcion];
      Left := ALeft;
      Top := ATop;
      Inc(ALeft,80);
      if (AWidth <= (Left + Width))
        then AWidth := Left + Width;
    end;
    { Goal: Activar un Button .}
  end;

  function ObtainFormTitle(const AMessageType: TMsgDlgType): string;
  begin
    if (AMessageType = mtCustomized)
      then Result := Application.Title
      else Result := FormCaptionsArray[AMessageType];
    { Goal: Obtener titulo de acuerdo al tipo de dialogo .}
  end;

  procedure PrepareMessageType(const AMessageType: TMsgDlgType; const AImages: TImagesArray);
  begin
    if Assigned(AImages[AMessageType])
      then AImages[AMessageType].Visible := TRUE;
    { Goal: Asignar imagen de acuerdo al tipo de dialogo .}
  end;

begin
  with TuktfrmMsgDlg.Create(Application) do
  begin
    try
      Count := 0;
      ALeft := 14;
      Position := poDesktopCenter;

      if (AMessageType <> mtCustomized)
        then lbMessage.Left := 56
        else lbMessage.Left := 14;
//      lbMessage.Font.Name := DefFontData.Name;

//      DC := GetDC(0);
//      ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
//      lbMessage.Font.Size := -MulDiv(DefFontData.Height, 72, ScreenLogPixels);
      lbMessage.Caption := AMessage;

      Caption := ObtainFormTitle(AMessageType);
      AWidth := lbMessage.Left + lbMessage.Width;

      ATop := 40 + lbMessage.Height;

      for EachOption := Low(TMsgDlgButton) to High(TMsgDlgButton) do
      begin
        PrepareButton(EachOption, Buttons);
      end;

      Inc(AWidth,16);
      Width := AWidth;
      Height := ATop + 62;

      PrepareMessageType(AMessageType, Images);
      ArrangeButtons(Width, Count, MoveButtons);

      if ((UseDefault) and (DefOption in AOptions)) then
      begin
        ActiveControl := Buttons[DefOption];
        // select as active control the indicated button option
        // seleccionar como control activo la opcion para boton indicada
      end;

      Width := 512;

      ShowModal();
    finally
      Result := ButtonPressed;
    end;
   end;
end;

function MessageDlg
  (const AMessage: string; AMessageType: TMsgDlgType;
const AOptions: TMsgDlgButtons; const AHlpCtx: Integer): TMsgDlgButton;
begin
  Result :=
   InternalMessageDlg(AMessage, AMessageType, AOptions, AHlpCtx, FALSE, mbYes);
   // "mbYes" : dummy argumment, not really used
   // "mbYes" : argumento falso, no se usa realmente
end;

function MessageDlg(const AMessage: string; AMessageType: TMsgDlgType;
const AOptions: TMsgDlgButtons): TMsgDlgButton;
begin
  Result :=
   InternalMessageDlg(AMessage, AMessageType, AOptions, 0, FALSE, mbYes);
   // "mbYes" : dummy argumment, not really used
   // "mbYes" : argumento falso, no se usa realmente
end;

function MessageDlgDef(
  const AMessage: string; AMessageType: TMsgDlgType;
  const AOptions: TMsgDlgButtons;
  const AHlpCtx: Integer;
  DefOption: TMsgDlgButton): TMsgDlgButton;
begin
  Result :=
   InternalMessageDlg
     (AMessage, AMessageType, AOptions, AHlpCtx, TRUE, DefOption);
end;

function MessageDlgDef(
  const AMessage: string; AMessageType: TMsgDlgType;
  const AOptions: TMsgDlgButtons;
  DefOption: TMsgDlgButton): TMsgDlgButton;
begin
  Result :=
   InternalMessageDlg
     (AMessage, AMessageType, AOptions, 0, TRUE, DefOption);
end;

procedure ErrorDlg(const AMessage: string);
begin
  MessageDlg(AMessage, mtError, [mbOK], 0);
end;

function ConfirmDlg(const AMessage: string): Boolean;
begin
  Result := (MessageDlg(AMessage, mtConfirmation, [mbYes, mbNo], 0) = mbYes);
end;

function ConfirmDlgDef
  (const AMessage: string; DefOption: TMsgDlgButton): Boolean;
begin
  Result :=
   (InternalMessageDlg
     (AMessage, mtConfirmation, [mbYes, mbNo], 0, TRUE, DefOption) = mbYes);
end;

function ConfirmMultipleDlg(const AMessage: string): TMsgDlgButton;
begin
  Result :=
   MessageDlg
    (AMessage, mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll], 0);
end;

function ConfirmMultipleDlgDef
  (const AMessage: string; DefOption: TMsgDlgButton): TMsgDlgButton;
begin
  Result :=
    InternalMessageDlg
     (AMessage, mtConfirmation,
      [mbYes, mbNo, mbYesToAll, mbNoToAll], 0, TRUE, DefOption);
end;

end.
