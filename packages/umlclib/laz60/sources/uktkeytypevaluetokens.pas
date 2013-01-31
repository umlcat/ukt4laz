(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktkeytypevaluetokens;

{$mode objfpc}{$H+}

interface

uses
  // ##temp {
  Crt, uktstrings,
  // ##temp }
  Classes, SysUtils,
  uktnormobjects,
  uktlists,
  uktreclists,
  uktonlystringkeytypevaluelists,
  dummy;

type

{ TKeyValueTypeFileToken }

  TKeyValueTypeFileToken =
  (
    kvtfiletkNone,        // Sin definir
    kvtfiletkObject,      // '<object>...</object>'
    kvtfiletkProperties,  // '<properties>...</properties>'
    kvtfiletkItems,       // '<items>...</items>'
    kvtfiletkItem,        // '<items>...</items>'
    kvtfiletkKey,         // '<key>...</key>'
    kvtfiletkValue,       // '<value>...</value>'
    kvtfiletkType,        // '<type>...</type>'
    kvtfiletkText,        // '<...>Hello World</... >'
    kvtfiletkElse         // ...
  );
  PKeyValueTypeFileToken = ^TKeyValueTypeFileToken;

  function StrToKeyValueType(const Value: string): TKeyValueTypeFileToken;
  function KeyValueTypeToStr({copy} Value: TKeyValueTypeFileToken): string;

  function XMLTagToKeyValueType(const Value: string): TKeyValueTypeFileToken;
  
implementation

const KeyValueTypeToStrArray: array[TKeyValueTypeFileToken] of string =
(
  'kvtfiletkNone',
  'kvtfiletkObject',
  'kvtfiletkProperties',
  'kvtfiletkItems',
  'kvtfiletkItem',
  'kvtfiletkKey',
  'kvtfiletkValue',
  'kvtfiletkType',
  'kvtfiletkText',
  'kvtfiletkElse'
);

const KeyValueTypeToXMLTagArray: array[TKeyValueTypeFileToken] of string =
(
  'none',
  'object',
  'properties',
  'items',
  'item',
  'key',
  'value',
  'type',
  'text',
  'else'
);

function StrToKeyValueType(const Value: string): TKeyValueTypeFileToken;
//var i: TKeyValueTypeFileToken; Found: Boolean;
var i: integer; Found: Boolean;
begin
  i := ord(Low(TKeyValueTypeFileToken)); Found := FALSE;
  while ((i <= ord(High(TKeyValueTypeFileToken))) and (not Found)) do
  begin
    Found := SameText(KeyValueTypeToStrArray[TKeyValueTypeFileToken(i)], Value);
    i := (i + 1);
  end;

  if (Found)
    then Result := TKeyValueTypeFileToken(i - 1)
    else Result := Low(TKeyValueTypeFileToken);
end;

function KeyValueTypeToStr({copy} Value: TKeyValueTypeFileToken): string;
begin
  if (Value < Low(TKeyValueTypeFileToken))
    then Value := Low(TKeyValueTypeFileToken);
  if (Value > High(TKeyValueTypeFileToken))
    then Value := High(TKeyValueTypeFileToken);
  Result := KeyValueTypeToStrArray[Value];
end;

function XMLTagToKeyValueType(const Value: string): TKeyValueTypeFileToken;
//var i: TKeyValueTypeFileToken; Found: Boolean;
var i: integer; Found: Boolean;
begin
  i := ord(Low(TKeyValueTypeFileToken)); Found := FALSE;
  while ((i <= ord(High(TKeyValueTypeFileToken))) and (not Found)) do
  begin
    Found := SameText(KeyValueTypeToXMLTagArray[TKeyValueTypeFileToken(i)], Value);
    i := (i + 1);
  end;

  if (Found)
    then Result := TKeyValueTypeFileToken(i - 1)
    else Result := Low(TKeyValueTypeFileToken);
end;


end.

