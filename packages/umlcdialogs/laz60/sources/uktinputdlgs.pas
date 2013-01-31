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

unit uktinputdlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  //ComCtrls,
{$ENDIF}
  Forms,
  uktstrings,
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  dummy;

  function InputString
    (const ATitle, ALabel: string; var Answer: string): Boolean;
  function InputInteger
    (const ATitle, ALabel: string; var Answer: Integer): Boolean;
  function InputDate
    (const ATitle, ALabel: string; var Answer: TDateTime): Boolean;
  function InputTime
    (const ATitle, ALabel: string; var Answer: TDateTime): Boolean;
  function InputMemo
    (const ATitle, ALabel: string; var Answer: TStrings): Boolean;
	
  function InputStrings
    (const ATitle, ALabel: string; var Answer: TStrings): Boolean;
  function InputStringsPlus
    (const ATitle, ALabel1, ALabel2: string; var Answer: TStrings): Boolean;

  function InputOptions
    (const ATitle, ALabel: string; Options: TStrings;
     var Answer: Integer): Boolean;

  // todo:
  //function InputHexa
    //(const ATitle, ALabel: string; var Answer: string): Boolean;
  // todo:
  //function InputId
    //(const ATitle, ALabel: string; var Answer: string): Boolean;

implementation

{$ifdef FPC}
uses
  lazuktfrmInputDate,
  lazuktfrmInputTime,
  lazuktfrmInputInt,
  lazuktfrmInputStr,
  lazuktfrmInputMemo,
  lazuktfrminputstrings,
  lazuktfrmInputStringsPlus,
  lazuktfrmInputOptions;
{$else}
uses
  vcluktfrmInputDate,
  vcluktfrmInputTime,
  vcluktfrmInputInt,
  vcluktfrmInputStr,
  vcluktfrmInputMemo,
  vcluktfrmInputStrings,
  vcluktfrmInputStringsPlus,
  vcluktfrmInputOptions;
{$endif}

function InputString
  (const ATitle, ALabel: string; var Answer: string): Boolean;
begin
  with TuktfrmInputStr.Create(Application) do
  begin
    try
      Option  := mbCancel;

      Caption := ATitle;
      lbMessage.Caption := uktstrings.PadPosfixCopy(ALabel, ':');
      edAnswer.Text := uktstrings.TrimCopy(Answer);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer := uktstrings.TrimCopy(edAnswer.Text);
      end;

      Free();
    end;
  end;
end;

function InputInteger
  (const ATitle, ALabel: string; var Answer: Integer): Boolean;
begin
  with TuktfrmInputInt.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      lbMessage.Caption := uktstrings.PadPosfixCopy(ALabel, ':');
      edAnswer.Text := IntToStr(Answer);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer := StrToIntDef(edAnswer.Text, 0);
      end;

      Free();
    end;
  end;
end;

function InputDate
  (const ATitle, ALabel: string; var Answer: TDateTime): Boolean;
var Year, Month, Day: Word;
begin
  with TuktfrmInputDate.Create(Application) do
  begin
    try
      Answer  := Now;
      Option  := mbCancel;
      Caption := ATitle;
      lbMessage.Caption := uktstrings.PadPosfixCopy(ALabel, ':');

      SysUtils.DecodeDate(Answer, Year, Month, Day);
      edYear.Text  := IntToStr(Year);
      edMonth.Text := IntToStr(Month);
      edDay.Text   := IntToStr(Day);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Year  := StrToIntDef(edYear.Text, 0);
        Month := StrToIntDef(edMonth.Text, 0);
        Day   := StrToIntDef(edDay.Text, 0);

        Answer := EncodeDate(Year, Month, Day);
      end;
      Free();
    end;
  end;
end;

function InputTime
  (const ATitle, ALabel: string; var Answer: TDateTime): Boolean;
var Hour, Min, Sec, MSec: Word;
begin
  with TuktfrmInputTime.Create(Application) do
  begin
    try
      Answer  := Now;
      Option  := mbCancel;
      Caption := ATitle;
      lbMessage.Caption := uktstrings.PadPosfixCopy(ALabel, ':');

      SysUtils.DecodeTime(Answer, Hour, Min, Sec, MSec);
      edHour.Text := IntToStr(Hour);
      edMin.Text  := IntToStr(Min);
      edSec.Text  := IntToStr(Sec);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Hour := StrToIntDef(edHour.Text, 0);
        Min  := StrToIntDef(edMin.Text, 0);
        Sec  := StrToIntDef(edSec.Text, 0);

        Answer := EncodeTime(Hour, Min, Sec, 0);
      end;
      Free();
    end;
  end;
end;

function InputMemo
  (const ATitle, ALabel: string; var Answer: TStrings): Boolean;
begin
  with TuktfrmInputMemo.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      lbMessage.Caption := uktstrings.PadPosfixCopy(ALabel, ':');

      mmAnswer.Lines.Clear;
      mmAnswer.Lines.AddStrings(Answer);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer.Assign(mmAnswer.Lines);
      end;

      Free();
    end;
  end;
end;

function InputStrings
  (const ATitle, ALabel: string; var Answer: TStrings): Boolean;
begin
  with TuktfrmInputStrings.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      lblLabel.Caption := uktstrings.PadPosfixCopy(ALabel, ':');

      AnswerMemo.Lines.Clear();
      AnswerMemo.Lines.AddStrings(Answer);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer.Clear;
        Answer.AddStrings(AnswerMemo.Lines);
      end;

      Free();
    end;
  end;
end;

function InputStringsPlus
  (const ATitle, ALabel1, ALabel2: string; var Answer: TStrings): Boolean;
var I, C: Integer;
begin
  with TuktfrmInputStringsPlus.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      lblLabel1.Caption := uktstrings.PadPosfixCopy(ALabel1, ':');
      lblLabel2.Caption := uktstrings.PadPosfixCopy(ALabel2, ':');

      mmAnswer.Items.Clear();
      mmAnswer.Items.AddStrings(Answer);

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer.Clear;

        C := Pred(mmAnswer.Items.Count);
        for I := 0 to C do
        begin
         if (mmAnswer.Checked[i]) then
         begin
           Answer.Add(mmAnswer.Items[i]);
         end;
        end;
      end;

      Free();
    end;
  end;
end;

function InputOptions
  (const ATitle, ALabel: string; Options: TStrings;
   var Answer: Integer): Boolean;
begin
  with TuktfrmInputOptions.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      rgOptions.Caption := ALabel;

      rgOptions.Items.Clear();
      rgOptions.Items.Assign(Options);
      Height := Height + (18 * Options.Count);
      // agregar espacio por cada opcion de la lista dada
      // add space for each option from the given list

      if (Answer <= Options.Count)
        then rgOptions.ItemIndex := Answer
        else rgOptions.ItemIndex := 0;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        Answer := rgOptions.ItemIndex;
      end;

      Free();
    end;
  end;
end;

end.
