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

unit uktkeytypevalueinputdlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}
    //ComCtrls,
  {$ENDIF}
  Forms,
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  uktonlystringkeytypevaluelists,
  dummy;

  function InputKeyTypeValue
    (const ATitle: string;
     var AKey: string; var AType: string; var AValue: string): Boolean;

  function InputKeyTypeValueList
    (const ATitle, AMessage: string;
     const AOptions: TSDVOnlyStringKeyTypeValueList; var AKey: string): Boolean;

  function InputKeyTypeValueListMany
    (const ATitle, AMessage: string;
     const AOptions: TSDVOnlyStringKeyTypeValueList;
     const ASelOptions: TStringList): Boolean;

  function InputOnlyKeyOptions
    (const ATitle, AMessage: string;
     AOptions: TSDVOnlyStringKeyTypeValueList;
     var AKey: string): Boolean;
  function InputOnlyValueOptions
    (const ATitle, AMessage: string;
     AOptions: TSDVOnlyStringKeyTypeValueList;
     var AKey: string): Boolean;

implementation
{$ifdef FPC}
uses
  lazuktfrminputkeytypevaluelist,
  //lazuktfrminputkeyvaluelistmany,
  lazuktfrminputkeytypevaluelistmany,
  lazuktfrminputkeytypevalue;
{$else}
uses
  //vcluktfrminputkeytypevalue,
  //vcluktfrminputkeytypevaluemany;
  //vclzuktfrminputkeytypevalue;
{$endif}

function InputKeyTypeValue
  (const ATitle: string;
   var AKey: string; var AType: string; var AValue: string): Boolean;
begin
  with Tfrminputkeytypevalue.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      FKey    := AKey;
      FTypeID := AType;
      FValue  := AValue;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        AKey   := FKey;
        AType  := FTypeID;
        AValue := FValue;
      end;

      Free();
    end;
  end;
end;

function InputKeyTypeValueList
 (const ATitle, AMessage: string;
  const AOptions: TSDVOnlyStringKeyTypeValueList; var AKey: string): Boolean;
var K, L: Integer; EachItem: TSDVOnlyStringKeyTypeValueItem;
    EachKey, EachTypeID, EachValue, EachText: string;
begin
  with Tuktfrminputkeytypevaluelist.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      Key := AKey;

      lblMessage.Caption := AMessage;

      ItemsListBox.Items.Clear();

      // assign items to listbox control
      L := (AOptions.Count - 1);
      for K := 0 to L do
      begin
        EachItem   := AOptions.Items[K];
        EachKey    := EachItem.Key;
        EachTypeID := EachItem.TypeID;
        EachValue  := EachItem.Value;
        EachText   :=
          '{"' +
          'Key: "' + EachKey + '", "' +
          'Type: ' + EachTypeID + '", "' +
          'Value: ' + EachValue + '"}';
        ItemsListBox.Items.AddObject(EachText, TObject(EachKey));
      end;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        AKey := Key;
      end;

      Free();
    end;
  end;
end;

function InputKeyTypeValueListMany
  (const ATitle, AMessage: string;
   const AOptions: TSDVOnlyStringKeyTypeValueList;
   const ASelOptions: TStringList): Boolean;
var K, L, EachIndex: Integer; EachItem: TSDVOnlyStringKeyTypeValueItem;
    EachKey, EachTypeID, EachValue, EachText: string;
begin
  with Tuktfrminputkeytypevaluelistmany.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      lblMessage.Caption := AMessage;

      ItemsCheckListBox.Items.Clear();

      // assign items to listbox control
      L := (AOptions.Count - 1);
      for K := 0 to L do
      begin
        EachItem   := AOptions.Items[K];
        EachKey    := EachItem.Key;
        EachTypeID := EachItem.TypeID;
        EachValue  := EachItem.Value;
        EachText   :=
          '{' +
          'Key: "' + EachKey + '", ' +
          'Type: "' + EachTypeID + '", ' +
          'Value: "' + EachValue + '"}';
        EachIndex := ItemsCheckListBox.Items.AddObject
          (EachText, TObject(EachKey));

        if (ASelOptions.IndexOf(EachKey) > -1) then
        begin
          ItemsCheckListBox.Checked[EachIndex] := true;
        end;
      end;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        ASelOptions.Clear();
        ASelOptions.AddStrings(SelItems);
      end;

      Free();
    end;
  end;
end;

function InputOnlyKeyOptions
  (const ATitle, AMessage: string;
   AOptions: TSDVOnlyStringKeyTypeValueList; var AKey: string): Boolean;
begin
  Result := false;
  (*
  with TuktfrmInputOptions.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      rgOptions.Caption := AMessage;

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
  *)
end;

function InputOnlyValueOptions
  (const ATitle, AMessage: string;
   AOptions: TSDVOnlyStringKeyTypeValueList; var AKey: string): Boolean;
begin
  Result := false;
end;


end.

