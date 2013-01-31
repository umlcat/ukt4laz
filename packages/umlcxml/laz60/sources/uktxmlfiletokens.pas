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

unit uktxmlfiletokens;

interface
uses
  SysUtils,
  uktAnsiCharSets,
  ukttextconsts,
  uktAnsiCharSetConsts,
  uktstrings,
  uktStrParsers,
  dummy;

type

{ TXMLFileToken }

  TXMLFileToken = 
  (
    xmlfiletkNone,        // Sin definir

    xmlfiletkEoF,         // End Of File marker
    xmlfiletkEoPg,        // End Of Page marker
    xmlfiletkEoLn,        // End Of Line marker

    xmlfiletkTab,         // tabulator character
    xmlfiletkSpace,       // space character

    xmlfiletkComment,     // Comment Tag
    xmlfiletkEncoding,    // Character Encoding Tag

    xmlfiletkStart,       // start tag
    xmlfiletkFinish,      // finish tag
    // to-do: reemplazar "xmlfiletkStart" y "xmlfiletkFinish":
    xmlfiletkBlock,
    xmlfiletkSingle,      // single tag

    xmlfiletkText,        // any text

    xmlfiletkEntity,      // Alias escape code characters
    xmlfiletkDecimal,     // Decimal escape code characters
    xmlfiletkHexaDecimal  // Hexadecimal escape code characters
  );
  PXMLFileToken = ^TXMLFileToken;

  function TokenToStr({copy} Value: TXMLFileToken): string;
  function StrToToken(const Value: string): TXMLFileToken;

const
  resFileVersion_DefaultValue  = '5.0';
  resFileEncoding_DefaultValue = 'ISO-8859-1';

  resLesser  = 'lt';
  resGreater = 'gt';
  resSingleQuote = 'apos';
  resDoubleQuote = 'quot';
  resSpace = 'nbsp';

  function CanInsertTo(const Token: TXMLFileToken): Boolean;
  function CanRename(const Token: TXMLFileToken): Boolean;
  function UsesTags(const Token: TXMLFileToken): Boolean;
  function UsesProps(const Token: TXMLFileToken): Boolean;
  function IsSimple(const Token: TXMLFileToken): Boolean;
  function IsComposed(const Token: TXMLFileToken): Boolean;
  function ExtractQuotedStr
    (const Value: ansistring; Index: Integer): ansistring;
  function TagToken
    (const Value: string; const Token: TXMLFileToken): string;
  function ComposeToken(const TokenName, TokenValue: string): string;
  function EntityToChar(const Value: string): Char;
  function EntityToCharEx(const Value: string): Char;
  function CharToEntityEx(const Value: Char): string;
  function CharToEntity(const Value: Char): string;
  function StrToEntityEx(const Value: string): string;
  function StrToEntity(const Value: string): string;
  function UnTagToken
    (const Value: string; const Token: TXMLFileToken): string;

implementation

const EnumToStrArray: array[TXMLFileToken] of string = 
(
  'xmlfiletkNone',

  'xmlfiletkEoF',
  'xmlfiletkEoPg',
  'xmlfiletkEoLn',

  'xmlfiletkSpace',
  'xmlfiletkTab',

  'xmlfiletkComment',
  'xmlfiletkEncoding',
  'xmlfiletkStart',
  'xmlfiletkFinish',
  'xmlfiletkBlock',
  'xmlfiletkSingle',

  'xmlfiletkText',

  'xmlfiletkEntity',
  'xmlfiletkDecimal',
  'xmlfiletkHexaDecimal'
);

function TokenToStr({copy} Value: TXMLFileToken): string;
begin
  if (Value < Low(TXMLFileToken))
    then Value := Low(TXMLFileToken);
  if (Value > High(TXMLFileToken))
    then Value := High(TXMLFileToken);
  Result := EnumToStrArray[Value];
end;

function StrToToken(const Value: string): TXMLFileToken;
var i: TXMLFileToken; Found: Boolean;
begin
  i := Low(TXMLFileToken); Found := FALSE;
  while ((i <= High(TXMLFileToken)) and (not Found)) do
  begin
    Found := SameText(EnumToStrArray[i], Value);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TXMLFileToken);
end;

function CanInsertTo(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkStart:
      Result := TRUE;
    xmlfiletkFinish:
      Result := TRUE;
    xmlfiletkBlock:
      Result := TRUE;
    xmlfiletkSingle:
      Result := TRUE;
    else
      Result := FALSE;
  end;
end;

function CanRename(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkStart:
      Result := TRUE;
    xmlfiletkFinish:
      Result := TRUE;
    xmlfiletkBlock:
      Result := TRUE;
    xmlfiletkSingle:
      Result := TRUE;
    else
      Result := FALSE;
  end;
end;

function UsesTags(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkEoF: Result := FALSE;
    xmlfiletkEoPg: Result := FALSE;
    xmlfiletkEoLn: Result := FALSE;
    xmlfiletkSpace: Result := FALSE;
    xmlfiletkComment: Result := TRUE;
    xmlfiletkEncoding: Result := TRUE;
    xmlfiletkStart: Result := TRUE;
    xmlfiletkFinish: Result := TRUE;
    xmlfiletkBlock: Result := TRUE;
    xmlfiletkSingle: Result := TRUE;
    xmlfiletkText: Result := FALSE;
    xmlfiletkEntity: Result := TRUE;
    xmlfiletkDecimal: Result := TRUE;
    xmlfiletkHexaDecimal: Result := TRUE;
//    xmlfiletkNone:
    else
    Result := TRUE;
  end;
end;

function UsesProps(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkStart:
      Result := TRUE;
    xmlfiletkFinish:
      Result := TRUE;
    xmlfiletkBlock:
      Result := TRUE;
    xmlfiletkSingle:
      Result := TRUE;
    else
      Result := FALSE;
  end;
end;

function IsSimple(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkStart:
      Result := FALSE;
    xmlfiletkFinish:
      Result := FALSE;
    xmlfiletkBlock:
      Result := FALSE;
    xmlfiletkSingle:
      Result := FALSE;
    else
      Result := TRUE;
  end;
end;

function IsComposed(const Token: TXMLFileToken): Boolean;
begin
  case (Token) of
    xmlfiletkStart:
      Result := TRUE;
    xmlfiletkFinish:
      Result := TRUE;
    xmlfiletkBlock:
      Result := TRUE;
    xmlfiletkSingle:
      Result := TRUE;
    else
      Result := FALSE;
  end;
end;

function ExtractQuotedStr
  (const Value: ansistring; Index: Integer): ansistring;
var L: Integer;
begin
  Result := '';
  if (Value[1] = ansiSingleQuote) then
  begin
    L := System.Length(Value);
    Index := uktstrings.SkipWhile(Value, 2, IDSet);
    if (Pred(Index) = L) and (Value[L] = ansiSingleQuote)
      then Result := Value
      else Result := '';
  end;
end;

function StartXML(const Value: string; const Token: TXMLFileToken): string;
begin
  Result := Value;
  case (Token) of
    xmlfiletkNone:        Result := '<none';
    xmlfiletkComment:     Result := '<!' + Value;
    xmlfiletkEncoding:    Result := '<?xml ' + Value;
    xmlfiletkStart:       Result := '<+' + Value;
    xmlfiletkFinish:      Result := '<-' + Value;
    xmlfiletkSingle:      Result := '<'  + Value;
    xmlfiletkDecimal:     Result := '&#' + Value;
    xmlfiletkHexaDecimal: Result := '&#0' + Value;
    xmlfiletkEntity:      Result := '&' + Value;
    else Exit;
  end;
end;

function FinishXML(const Value: string; const Token: TXMLFileToken): string;
begin
  Result := Value;
  case (Token) of
    xmlfiletkNone:        Result := Value + '>';
    xmlfiletkComment:     Result := Value + '>';
    xmlfiletkEncoding:    Result := Value + '?>';
    xmlfiletkStart:       Result := Value + '>';
    xmlfiletkFinish:      Result := Value + '>';
    xmlfiletkSingle:      Result := Value + '/ >';
    xmlfiletkDecimal:     Result := Value + ';';
    xmlfiletkHexaDecimal: Result := Value + ';';
    xmlfiletkEntity:      Result := Value + ';';
    else Exit;
  end;
end;

function TagToken
  (const Value: string; const Token: TXMLFileToken): string;
begin
  Result := StartXML(Value, Token);
  Result := FinishXML(Result, Token);
end;

function ComposeToken(const TokenName, TokenValue: string): string;
begin
  Result := TagToken(TokenName, xmlfiletkStart);
  Result := Result + TokenValue;
  Result := Result + TagToken(TokenName, xmlfiletkFinish);
end;

function EntityToChar(const Value: string): Char;
begin
  Result := #32;
  if (SameText(Value, resLesser))
    then Result := '<'
  else if (SameText(Value, resGreater))
    then Result := '>'
  else if (SameText(Value, resSingleQuote))
      then Result := #39
  else if (SameText(Value, resDoubleQuote))
    then Result := #34;
end;

function EntityToCharEx(const Value: string): Char;
begin
  Result := #32;
  if (SameText(Value, resSpace))
    then Result := #32
  else if (SameText(Value, resLesser))
    then Result := '<'
  else if (SameText(Value, resGreater))
    then Result := '>'
  else if (SameText(Value, resSingleQuote))
      then Result := #39
  else if (SameText(Value, resDoubleQuote))
    then Result := #34
  (*
  else if (Value = 'ntilde')
    then Result := 'ñ'
  else if (Value = 'ntilde')
    then Result := 'Ñ';
  *)
end;

function CharToEntityEx(const Value: Char): string;
begin
  case (Value) of
    '<': Result := TagToken(resLesser, xmlfiletkEntity);
    '>': Result := TagToken(resGreater, xmlfiletkEntity);
    #39: Result := TagToken(resSingleQuote, xmlfiletkEntity);
    #34: Result := TagToken(resDoubleQuote, xmlfiletkEntity);
    #32: Result := TagToken(resSpace, xmlfiletkEntity);
    (*
    'ñ': Result := TagToken('ntilde', xmlfiletkEntity);
    'Ñ': Result := TagToken('Ntilde', xmlfiletkEntity);
    *)
    else Result := Value;
  end;
end;

function CharToEntity(const Value: Char): string;
begin
  case (Value) of
    '<': Result := TagToken(resLesser, xmlfiletkEntity);
    '>': Result := TagToken(resGreater, xmlfiletkEntity);
    #39: Result := TagToken(resSingleQuote, xmlfiletkEntity);
    #34: Result := TagToken(resDoubleQuote, xmlfiletkEntity);
    else Result := Value;
  end;
end;

function StrToEntityEx(const Value: string): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to System.Length(Value) do
    Result := Result + CharToEntityEx(Value[i]);
end;

function StrToEntity(const Value: string): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to System.Length(Value) do
    Result := Result + CharToEntity(Value[i]);
end;

function UnTagToken
  (const Value: string; const Token: TXMLFileToken): string;
var P, L: Integer;
begin
  Result := Value;
  case (Token) of
    xmlfiletkNone:
    begin
      L := System.Length(Value) - 2;
      Result := System.Copy(Value, 2, L);
    end;
    xmlfiletkComment:
    begin
      P := System.Pos('<!', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    xmlfiletkEncoding: Result := '<?xml ' + Value;
    xmlfiletkStart:
    begin
      P := System.Pos('<+', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    xmlfiletkFinish:
    begin
      P := System.Pos('<-', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    xmlfiletkSingle:
    begin
      L := System.Length(Value) - 2;
      Result := System.Copy(Value, 2, L);
    end;
    xmlfiletkDecimal:
    begin
      P := System.Pos('<#', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    xmlfiletkHexaDecimal:
    begin
      P := System.Pos('<$', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    xmlfiletkEntity:
    begin
      P := System.Pos('<&', Value);
      L := System.Length(Value) - 3;
      Result := System.Copy(Value, P, L);
    end;
    else Exit;
  end;
end;


end.
