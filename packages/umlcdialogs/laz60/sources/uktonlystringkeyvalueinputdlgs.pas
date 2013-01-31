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

unit uktonlystringkeyvalueinputdlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  //ComCtrls,
  Forms,
{$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  uktkeyvaluemodes,
  uktonlystringkeyvaluelists,
  uktobjtypes,
  dummy;

  function InputKeyValue
    (const ATitle: string;
     var AKey: string; var AValue: string): Boolean;

  function InputOnlyKeyOptions
    (const ATitle, AMessage: string;
     AOptions: TSDVOnlyStringKeyValueList;
     var AKey: string): Boolean;
  function InputOnlyValueOptions
    (const ATitle, AMessage: string;
     AOptions: TSDVOnlyStringKeyValueList;
     var AKey: string): Boolean;

  function InputKeyValueList
    (const ATitle, AMessage: string;
     const AOptions: TSDVOnlyStringKeyValueList; var AKey: string): Boolean;

  function InputKeyValueListMany
    (const ATitle, AMessage: string;
     const AOptions: TSDVOnlyStringKeyValueList;
     const ASelOptions: TStringList): Boolean;

implementation
{$ifdef FPC}
uses
  lazuktfrminputkeyvaluelist,
  //lazuktfrminputkeyvaluelistmany,
  lazuktfrminputkeyvalue,
  lazuktfrminputkeyvalueoptions;
{$else}
uses
  //vcluktfrminputkeyvalue,
  //vcluktfrminputkeyvaluemany,
  //vcluktfrminputkeyvalue,
  //vcluktfrminputkeyvalueoptions;
{$endif}

function InputKeyValue
  (const ATitle: string;
   var AKey: string; var AValue: string): Boolean;
begin
  with Tfrminputkeyvalue.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      FKey    := AKey;
      FValue  := AValue;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        AKey   := FKey;
        AValue := FValue;
      end;

      Free();
    end;
  end;
end;

function InputKeyValueListMany
  (const ATitle, AMessage: string;
   const AOptions: TSDVOnlyStringKeyValueList;
   const ASelOptions: TStringList): Boolean;
begin
  //
end;

function InputOnlyKeyOptions
 (const ATitle, AMessage: string;
  AOptions: TSDVOnlyStringKeyValueList; var AKey: string): Boolean;
var AKeyList: TStrings; AIndex: Integer;
begin
  Result := false;

  with Tfrminputkeyvalueoptions.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      rgOptions.Caption := AMessage;

      AKeyList := AOptions.ExtractKeysCopy();

      rgOptions.Items.Clear();
      rgOptions.Items.Assign(AKeyList);

      Height := Height + (18 * AOptions.Count);
      // agregar espacio por cada opcion de la lista dada
      // add space for each option from the given list

      // seleccionar llave indicada
      AIndex := AOptions.IndexOfKey(AKey);
      if (AIndex >= 0) then
      begin
        rgOptions.ItemIndex := AIndex
      end else
      begin
        rgOptions.ItemIndex := 0;
      end;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        AIndex := rgOptions.ItemIndex;
        AKey := rgOptions.Items.Strings[AIndex];
      end;

      AKeyList.Free();
      Free();
    end;
  end;
end;

function InputOnlyValueOptions
  (const ATitle, AMessage: string;
  AOptions: TSDVOnlyStringKeyValueList; var AKey: string): Boolean;
var EachIndex, LastIndex: Integer;
    AItem: TSDVOnlyStringKeyValueItem; AObject: TSDVStringObject;
begin
  Result := false;

  with Tfrminputkeyvalueoptions.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      rgOptions.Caption := AMessage;

      rgOptions.Items.Clear();

      // fill the radiogroup control
      // with objects as data
      LastIndex := (AOptions.Count - 1);
      for EachIndex := 0 to LastIndex do
      begin
        AItem := AOptions.Items[EachIndex];

        AObject := TSDVStringObject.Create();
        AObject.Value := AItem.Key;

        rgOptions.Items.AddObject(AItem.Value, AObject);
      end;

      Height := Height + (18 * AOptions.Count);
      // agregar espacio por cada opcion de la lista dada
      // add space for each option from the given list

      EachIndex := AOptions.IndexOfKey(AKey);
      if (EachIndex >= 0) then
      begin
        rgOptions.ItemIndex := EachIndex;
      end else
      begin
        rgOptions.ItemIndex := 0;
      end;

      ShowModal();
    finally
      Result := (Option = mbOK);

      if (Result) then
      begin
        EachIndex := rgOptions.ItemIndex;
        AObject := (rgOptions.Items.Objects[EachIndex] as TSDVStringObject);
        if (AObject <> nil) then
        begin
          AKey := AObject.Value;
        end else
        begin
          AKey := '';
        end;
      end;

      // fill the radiogroup control
      // with objects as data
      LastIndex := (rgOptions.Items.Count - 1);
      for EachIndex := 0 to LastIndex do
      begin
        // release each item*s data object
        AObject := (rgOptions.Items.Objects[EachIndex] as TSDVStringObject);
        AObject.Free();
        rgOptions.Items.Objects[EachIndex] := nil;
      end;

      // remove all items
      rgOptions.Items.Clear();

      Free();
    end;
  end;
end;

function InputKeyValueList
 (const ATitle, AMessage: string;
  const AOptions: TSDVOnlyStringKeyValueList; var AKey: string): Boolean;
var K, L: Integer; EachItem: TSDVOnlyStringKeyValueItem;
    EachKey, EachValue, EachText: string;
begin
  with Tuktfrminputkeyvaluelist.Create(Application) do
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
        EachItem  := AOptions.Items[K];
        EachValue := EachItem.Value;
        EachKey   := EachItem.Key;
        EachText  := '{"' + EachKey + '" = "' + EachValue + '"}';
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

end.

