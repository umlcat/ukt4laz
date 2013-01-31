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

unit uktxmltagtokens;

interface
uses
  uktAnsiCharSets, 
  uktStrings, 
  uktStrParsers, 
  uktTextConsts, 
  uktAnsiCharSetConsts;

type
  TXMLTagToken = 
  (
  xmltagtkNone,      // Sin definir
  xmltagtkAttribute, // attribute*s qualified identifier
  xmltagtkAssign,    // '='
  xmltagtkValue,     // attribute*s value
  xmltagtkSpace,     // space character
  xmltagtkEoLn,      // End Of Line marker
  xmltagtkEoPg,      // End Of Page marker
  xmltagtkEoF        // End Of File marker
  );
  PXMLTagToken = ^TXMLTagToken;

  function TokenToStr({copy} Value: TXMLTagToken): string;
  function StrToToken(const Value: string): TXMLTagToken;


implementation

const EnumToStrArray: array[TXMLTagToken] of string =
(
  'xmltagtkNone',
  'xmltagtkAttribute',
  'xmltagtkAssign',
  'xmltagtkValue',
  'xmltagtkSpace',
  'xmltagtkEoLn',
  'xmltagtkEoPg',
  'xmltagtkEoF'
);

function TokenToStr({copy} Value: TXMLTagToken): string;
begin
  if (Value < Low(TXMLTagToken))
    then Value := Low(TXMLTagToken);
  if (Value > High(TXMLTagToken))
    then Value := High(TXMLTagToken);
  Result := EnumToStrArray[Value];
end;

function StrToToken(const Value: string): TXMLTagToken;
var i: TXMLTagToken; Found: Boolean;
begin
  i := Low(TXMLTagToken); Found := FALSE;
  while ((i <= High(TXMLTagToken)) and (not Found)) do
  begin
    Found := SameText(EnumToStrArray[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TXMLTagToken);
end;

end.
