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

unit uktxmlfilegroups;

interface
uses
  SysUtils,
  dummy;

type
  TXMLFileGroup = 
  (
  xmlfilegrDigit,       // '0' .. '9'
  xmlfilegrAF,          // 'a' .. 'f', 'A' .. 'F' ( used in hexadecimal numbers )
  xmlfilegrGWYZ,        // 'g' .. 'w', 'y', 'z', 'G'..'W', 'Y', 'Z' ( any identifier or text )
  xmlfilegrX,           // 'x', 'X' ( used in hexadecimal escape characters )
  xmlfilegrAmpersand,   // '&' ( entities* start marker )
  xmlfilegrPound,       // '#' ( used in decimal escape characters )
  xmlfilegrQuestion,    // '?' ( used in encoding tag )
  xmlfilegrExclamation, // '!' ( used in comments )
  xmlfilegrSlash,       // '/'
  xmlfilegrSemicolon,   // ';'  ( entities* finish marker )
  xmlfilegrLesser,      // '<'
  xmlfilegrGreater,     // '>'
  xmlfilegrSingleQuote, // pascal style string
  xmlfilegrDoubleQuote, // c/c++ style string
  xmlfilegrSpace,       // space
  xmlfilegrEoLn,        // End Of Line marker
  xmlfilegrEoPg,        // End Of Page marker
  xmlfilegrEoF,         // End Of File marker
  xmlfilegrElse         // any other character
  );
  PXMLFileGroup = ^TXMLFileGroup;

  function GroupToStr({copy} Value: TXMLFileGroup): string;
  function StrToGroup(const Value: string): TXMLFileGroup;

  function CharToGroup(const Value: ansichar): TXMLFileGroup;

implementation

const EnumToStrArray: array[TXMLFileGroup] of string = 
(
  'xmlfilegrDigit',
  'xmlfilegrAF',
  'xmlfilegrGWYZ',
  'xmlfilegrX',
  'xmlfilegrAmpersand',
  'xmlfilegrPound',
  'xmlfilegrQuestion',
  'xmlfilegrExclamation',
  'xmlfilegrSlash',
  'xmlfilegrSemicolon',
  'xmlfilegrLesser',
  'xmlfilegrGreater',
  'xmlfilegrSingleQuote',
  'xmlfilegrDoubleQuote',
  'xmlfilegrSpace',
  'xmlfilegrEoLn',
  'xmlfilegrEoPg',
  'xmlfilegrEoF',
  'xmlfilegrElse'
);

function GroupToStr({copy} Value: TXMLFileGroup): string;
begin
  if (Value < Low(TXMLFileGroup))
    then Value := Low(TXMLFileGroup);
  if (Value > High(TXMLFileGroup))
    then Value := High(TXMLFileGroup);
  Result := EnumToStrArray[Value];
end;

function StrToGroup(const Value: string): TXMLFileGroup;
var i: TXMLFileGroup; Found: Boolean;
begin
  i := Low(TXMLFileGroup); Found := FALSE;
  while ((i <= High(TXMLFileGroup)) and (not Found)) do
  begin
    Found := SameText(EnumToStrArray[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TXMLFileGroup);
end;

function CharToGroup(const Value: ansichar): TXMLFileGroup;
begin
  case (Value) of
    '0'..'9': Result := xmlfilegrDigit;
    'A'..'F',
    'a'..'f': Result := xmlfilegrAF;
    ':',
    'G'..'W', 'Y', 'Z',
    'g'..'w', 'y', 'z': Result := xmlfilegrGWYZ;
    'x', 'X': Result := xmlfilegrX;
    '&':      Result := xmlfilegrAmpersand;
    '#':      Result := xmlfilegrPound;
    '?':      Result := xmlfilegrQuestion;
    '!':      Result := xmlfilegrExclamation;
    '/':      Result := xmlfilegrSlash;
    ';':      Result := xmlfilegrSemicolon;
    '<':      Result := xmlfilegrLesser;
    '>':      Result := xmlfilegrGreater;
    #39:      Result := xmlfilegrSingleQuote;
    #34:      Result := xmlfilegrDoubleQuote;
    #32, #8,#9:      Result := xmlfilegrSpace;  // <-- hack !!!
    #13, #10: Result := xmlfilegrEoLn;
    #12:      Result := xmlfilegrEoPg;
    #26:      Result := xmlfilegrEoF;
    else      Result := xmlfilegrElse;
  end;
end;


end.
