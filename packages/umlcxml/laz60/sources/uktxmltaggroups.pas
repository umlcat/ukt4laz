(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
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

unit uktxmltaggroups;

interface
uses
  SysUtils,
  dummy;

type
  TXMLTagGroup = 
  (
  xmltaggrDigit,       // '0' .. '9'
  xmltaggrAZ,          // 'a' .. 'z', 'A' .. 'Z' ( used in identifiers )
  xmltaggrMinus,       // '-' ( treated as a letter in identifiers )
  xmltaggrColon,       // ':' ( used in qualified identifers )
  xmltaggrEqual,       // '=' ( used to assign values to attributes )
  xmltaggrSingleQuote, // pascal string delimiter
  xmltaggrDoubleQuote, // c++ string delimiter
  xmltaggrSpace,       // space
  xmltaggrEoLn,        // End Of Line marker
  xmltaggrEoPg,        // End Of Page marker
  xmltaggrEoF,         // End Of File marker
  xmltaggrElse         // any other character
  );
  PXMLTagGroup = ^TXMLTagGroup;

  function GroupToStr({copy} Value: TXMLTagGroup): string;
  function StrToGroup(const Value: string): TXMLTagGroup;

  function CharToGroup(const Value: ansichar): TXMLTagGroup;

implementation

const EnumToStrArray: array[TXMLTagGroup] of string = 
(
  'xmltaggrDigit',
  'xmltaggrAZ',
  'xmltaggrMinus',
  'xmltaggrColon',
  'xmltaggrEqual',
  'xmltaggrSingleQuote',
  'xmltaggrDoubleQuote',
  'xmltaggrSpace',
  'xmltaggrEoLn',
  'xmltaggrEoPg',
  'xmltaggrEoF',
  'xmltaggrElse'
);

function GroupToStr({copy} Value: TXMLTagGroup): string;
begin
  if (Value < Low(TXMLTagGroup))
    then Value := Low(TXMLTagGroup);
  if (Value > High(TXMLTagGroup))
    then Value := High(TXMLTagGroup);
  Result := EnumToStrArray[Value];
end;

function StrToGroup(const Value: string): TXMLTagGroup;
var i: TXMLTagGroup; Found: Boolean;
begin
  i := Low(TXMLTagGroup); Found := FALSE;
  while ((i <= High(TXMLTagGroup)) and (not Found)) do
  begin
    Found := SameText(EnumToStrArray[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TXMLTagGroup);
end;

function CharToGroup(const Value: ansichar): TXMLTagGroup;
begin
  case (Value) of
    '0'..'9': Result := xmltaggrDigit;
    'A'..'Z',
    'a'..'z': Result := xmltaggrAZ;
    '-':      Result := xmltaggrMinus;
    ':':      Result := xmltaggrColon;
    '=':      Result := xmltaggrEqual;
    #39:      Result := xmltaggrSingleQuote;
    #34:      Result := xmltaggrDoubleQuote;
    #32:      Result := xmltaggrSpace;
    #13, #10: Result := xmltaggrEoLn;
    #12:      Result := xmltaggrEoPg;
    #26:      Result := xmltaggrEoF;
    else      Result := xmltaggrElse;
  end;
end;


end.
