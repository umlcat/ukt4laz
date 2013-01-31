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

unit uktmsgdlgsmemos;

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

  function MessageDlgStrHlp
    (const AMessage: string;
           AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons;
     const AHlpCtx: Integer): TMsgDlgButton;

  function MessageDlgListHlp
    (const AMessage: TStrings;
           AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons;
     const AHlpCtx: Integer): TMsgDlgButton;

  function MessageDlg
    (const AMessage: string; AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons;
     const AHlpCtx: Integer): TMsgDlgButton; overload;
  function MessageDlg
    (const AMessage: string; AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons): TMsgDlgButton; overload;

  function MessageDlg
    (const AMessage: TStrings; AMessageType: TMsgDlgType;
     const AOptions: TMsgDlgButtons): TMsgDlgButton; overload;

  function ConfirmDlg(const AMessage: string): Boolean;
  function ConfirmMultipleDlg(const AMessage: string): TMsgDlgButton;

  procedure ShowMessage(const AMessage: string); overload;
  procedure ShowMessage(const AMessage: TStrings); overload;

implementation

{$ifdef FPC}
uses
  lazuktfrmmsgmemo;
{$else}
uses
  vcluktfrmmsgmemo;
{$endif}

procedure ShowMessage(const AMessage: string);
begin
  MessageDlgStrHlp(AMessage, mtCustomized, [mbOK], 0);
end;

procedure ShowMessage(const AMessage: TStrings);
begin
  MessageDlgListHlp(AMessage, mtCustomized, [mbOK], 0);
end;

procedure ArrangeOddNumberButtons(const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
var I, ACenter: Integer;
begin
  ACenter := (Succ(ACount) div 2);
  AButtons[ACenter].Left := (AWidth div 2) - (AButtons[1].Width div 2);
  AButtons[ACenter].Height := 25;

  if (ACenter > 1)
    then
  for i := Pred(ACenter) to 1 do
  begin
    AButtons[i].Left := AButtons[Succ(i)].Left - AButtons[i].Width - 6;
    AButtons[i].Height := 25;
  end;
  // Acomodar anteriores

  if (ACenter > 1)
    then
  for i := Succ(ACenter) downto ACount do
  begin
    AButtons[i].Left := AButtons[Pred(i)].Left + AButtons[Pred(i)].Width + 6;
    AButtons[i].Height := 25;
  end;
  // Acomodar posteriores

  { Goal: Acomodar Buttons con cuenta non .}
end;

procedure ArrangePairNumberButtons(const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
var I, ACenter: Integer;
begin
  ACenter := (ACount div 2);
  AButtons[ACenter].Left := (AWidth div 2) - AButtons[1].Width - 3;
  AButtons[ACenter].Height := 25;

  if (ACenter > 1)
    then
  for i := Pred(ACenter) to 1 do
  begin
    AButtons[i].Left := AButtons[Succ(i)].Left - AButtons[i].Width - 6;
    AButtons[i].Height := 25;
  end;
  // arrange previous buttons

  for i := Succ(ACenter) downto ACount do
  begin
    AButtons[i].Left := AButtons[Pred(i)].Left + AButtons[Pred(i)].Width + 6;
    AButtons[i].Height := 25;
  end;
  // arrange next buttons

  { Goal: Acomodar Buttons con cuenta par .}
end;

procedure ArrangeButtons(const AWidth, ACount: Integer; const AButtons: TIncButtonsArray);
begin
  if Odd(ACount)
    then ArrangeOddNumberButtons(AWidth, ACount, AButtons)
    else ArrangePairNumberButtons(AWidth, ACount, AButtons);
end;

function MessageDlgStrHlp
  (const AMessage: string;
         AMessageType: TMsgDlgType;
   const AOptions: TMsgDlgButtons;
   const AHlpCtx: Integer): TMsgDlgButton;
var
  AWidth, ALeft, ATop, AMinWidth, AMemoLeft: Integer;
  Count: Byte; EachOption: TMsgDlgButton;
  MoveButtons: TIncButtonsArray;
//  ScreenLogPixels: Integer;
//  DC: HDC;
  AAnchors: TAnchors;

  procedure PrepareButton(const AOpcion: TMsgDlgButton; const AButtons: TButtonsArray);
  begin
    with AButtons[AOpcion] do
    if not (AOpcion in AOptions) then
    begin
      Visible := FALSE;
      Enabled := FALSE;
    end else
    begin
      Inc(Count);
      MoveButtons[Count] := AButtons[AOpcion];

      Left := ALeft;
      Top  := ATop;
      Inc(ALeft, 80);

      if (AWidth <= (Left + Width))
        then AWidth := Left + Width;
    end;
    { Goal: prepare each button .}
  end;

  function ObtainFormTitle(const AMessageType: TMsgDlgType): string;
  begin
    if (AMessageType = mtCustomized)
      then Result := Application.Title
      else Result := FormCaptionsArray[AMessageType];
    { Goal: Obtener titulo de acuerdo al tipo de dialog .}
  end;

  procedure PrepareMessageType(const AMessageType: TMsgDlgType; const AImages: TImagesArray);
  begin
    if (AImages[AMessageType] <> nil) then
    begin
      AImages[AMessageType].Visible := TRUE;
    end;
    { Goal: Asignar imagen de acuerdo al tipo de dialog .}
  end;
begin
  with TuktfrmMsgMemo.Create(Application) do
  begin
    try
      Count := 0;
      ALeft := 14;
      Position := poDesktopCenter;

//      mmMemo.Font.Name := DefFontData.Name;
//      DC := GetDC(0);
//      ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
//      mmMemo.Font.Size := -MulDiv(DefFontData.Height, 72, ScreenLogPixels);

      mmMemo.Lines.Text := AMessage;

      Caption := ObtainFormTitle(AMessageType);

      ATop   := 40 + mmMemo.Height;
      AWidth := 0;

      for EachOption := Low(TMsgDlgButton) to High(TMsgDlgButton) do
      begin
        PrepareButton(EachOption, Buttons);
      end;

      AMemoLeft := mmMemo.Left;

      AAnchors := mmMemo.Anchors;
      Exclude(AAnchors, akRight);
      mmMemo.Anchors := AAnchors;
      // backup memo*s anchors

      Inc(AWidth, AMemoLeft{16});
      // update form*s width

      AMinWidth := (mmMemo.Width + (AMemoLeft * 2));
      if (AWidth < AMinWidth) then
      begin
        AWidth := AMinWidth;
        // calculate form*s minimun width
      end;

      mmMemo.Width := (AWidth - (AMemoLeft * 4));
      // update memo*s width

      Include(AAnchors, akRight);
      mmMemo.Anchors := AAnchors;
      // restore memo*s anchors

      Width  := AWidth;
      Height := ATop + 62;

      PrepareMessageType(AMessageType, Images);
      ArrangeButtons(Width, Count, MoveButtons);

      mmMemo.Width := Width - (mmMemo.Left * 2);

      if (AMessageType = mtCustomized) then
      with mmMemo do
      begin
        Top := imgConfirmation.Top;
        Height := Height + imgConfirmation.Height;
      end;

      ShowModal();
    finally
      Result := ButtonPressed;
    end;
   end;
end;

function MessageDlgListHlp
  (const AMessage: TStrings;
         AMessageType: TMsgDlgType;
   const AOptions: TMsgDlgButtons;
   const AHlpCtx: Integer): TMsgDlgButton;
var
  AWidth, ALeft, ATop, AMinWidth, AMemoLeft: Integer;
  Count: Byte; EachOption: TMsgDlgButton;
  MoveButtons: TIncButtonsArray;
//  ScreenLogPixels: Integer;
//  DC: HDC;
  AAnchors: TAnchors;

  procedure PrepareButton(const AOpcion: TMsgDlgButton; const AButtons: TButtonsArray);
  begin
    with AButtons[AOpcion] do
    if not (AOpcion in AOptions) then
    begin
      Visible := FALSE;
      Enabled := FALSE;
    end else
    begin
      Inc(Count);
      MoveButtons[Count] := AButtons[AOpcion];

      Left := ALeft;
      Top  := ATop;
      Inc(ALeft, 80);

      if (AWidth <= (Left + Width))
        then AWidth := Left + Width;
    end;
    { Goal: prepare each button .}
  end;

  function ObtainFormTitle(const AMessageType: TMsgDlgType): string;
  begin
    if (AMessageType = mtCustomized)
      then Result := Application.Title
      else Result := FormCaptionsArray[AMessageType];
    { Goal: Obtener titulo de acuerdo al tipo de dialog .}
  end;

  procedure PrepareMessageType(const AMessageType: TMsgDlgType; const AImages: TImagesArray);
  begin
    if (AImages[AMessageType] <> nil) then
    begin
      AImages[AMessageType].Visible := TRUE;
    end;
    { Goal: Asignar imagen de acuerdo al tipo de dialog .}
  end;
begin
  with TuktfrmMsgMemo.Create(Application) do
  begin
    try
      Count := 0;
      ALeft := 14;
      Position := poDesktopCenter;

//      mmMemo.Font.Name := DefFontData.Name;
//      DC := GetDC(0);
//      ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
//      mmMemo.Font.Size := -MulDiv(DefFontData.Height, 72, ScreenLogPixels);

      mmMemo.Lines.Clear();
      mmMemo.Lines.AddStrings(AMessage);

      Caption := ObtainFormTitle(AMessageType);

      ATop   := 40 + mmMemo.Height;
      AWidth := 0;

      for EachOption := Low(TMsgDlgButton) to High(TMsgDlgButton) do
      begin
        PrepareButton(EachOption, Buttons);
      end;

      AMemoLeft := mmMemo.Left;

      AAnchors := mmMemo.Anchors;
      Exclude(AAnchors, akRight);
      mmMemo.Anchors := AAnchors;
      // backup memo*s anchors

      Inc(AWidth, AMemoLeft{16});
      // update form*s width

      AMinWidth := (mmMemo.Width + (AMemoLeft * 2));
      if (AWidth < AMinWidth) then
      begin
        AWidth := AMinWidth;
        // calculate form*s minimun width
      end;

      mmMemo.Width := (AWidth - (AMemoLeft * 4));
      // update memo*s width

      Include(AAnchors, akRight);
      mmMemo.Anchors := AAnchors;
      // restore memo*s anchors

      Width  := AWidth;
      Height := ATop + 62;

      PrepareMessageType(AMessageType, Images);
      ArrangeButtons(Width, Count, MoveButtons);

      mmMemo.Width := Width - (mmMemo.Left * 2);

      if (AMessageType = mtCustomized) then
      with mmMemo do
      begin
        Top := imgConfirmation.Top;
        Height := Height + imgConfirmation.Height;
      end;

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
  Result := MessageDlgStrHlp(AMessage, AMessageType, AOptions, AHlpCtx);
end;

function MessageDlg
  (const AMessage: string; AMessageType: TMsgDlgType;
   const AOptions: TMsgDlgButtons): TMsgDlgButton;
begin
  Result := MessageDlgStrHlp(AMessage, AMessageType, AOptions, 0);
end;

function MessageDlg
  (const AMessage: TStrings; AMessageType: TMsgDlgType;
   const AOptions: TMsgDlgButtons): TMsgDlgButton;
begin
  Result :=
    MessageDlgListHlp(AMessage, AMessageType, AOptions, 0);
end;

function ConfirmDlg(const AMessage: string): Boolean;
begin
  Result :=
    (MessageDlgStrHlp(AMessage, mtConfirmation, [mbYes, mbNo], 0) = mbYes);
end;

function ConfirmMultipleDlg(const AMessage: string): TMsgDlgButton;
begin
  Result := MessageDlgStrHlp
    (AMessage, mtConfirmation, [mbYes, mbNo, mbYesToAll, mbNoToAll], 0);
end;

end.
