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

unit uktxmlfiletreenodetokens;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dummy;

resourcestring
  resxmlfiletrntkNone     = 'None';

  resxmlfiletrntkDocument    = 'Document';

  resxmlfiletrntkEoF         = 'EoF';
  resxmlfiletrntkEoPg        = 'EoPg';
  resxmlfiletrntkEoLn        = 'EoLn';

  resxmlfiletrntkTab         = 'Tab';
  resxmlfiletrntkSpace       = 'Space';

  resxmlfiletrntkComment     = 'Comment';
  resxmlfiletrntkEncoding    = 'Encoding';

  resxmlfiletrntkBlock       = 'Block';
  resxmlfiletrntkSingle      = 'Single';

  resxmlfiletrntkText        = 'Text';
  resxmlfiletrntkEntity      = 'Entity';
  resxmlfiletrntkHexaDecimal = 'HexaDecimal';

type

(* TXMLFileTreeNodeToken *)

  TXMLFileTreeNodeToken =
  (
    xmlfiletrntkNone,     // Sin definir

    xmlfiletrntkDocument, // whole document

    xmlfiletrntkEoF,      // End Of File marker
    xmlfiletrntkEoPg,     // End Of Page marker
    xmlfiletrntkEoLn,     // End Of Line marker

    xmlfiletrntkTab,      // tabulator character
    xmlfiletrntkSpace,    // space character

    xmlfiletrntkComment,  // Comment Tag
    xmlfiletrntkEncoding, // Character Encoding Tag

    xmlfiletrntkBlock,    // pair of tags
    xmlfiletrntkSingle,   // single tag

    xmlfiletrntkText,        // any text
    xmlfiletrntkEntity,      // Alias escape code characters
    xmlfiletrntkHexaDecimal  // Hexadecimal escape code characters
  );
  PXMLFileTreeNodeToken = ^TXMLFileTreeNodeToken;

  function TokenInRange
    (const AValue: TXMLFileTreeNodeToken): TXMLFileTreeNodeToken;

  function TokenToStr((* copy *) AValue: TXMLFileTreeNodeToken): string;
  function TokenToText((* copy *) AValue: TXMLFileTreeNodeToken): string;

  function StrToToken(const AValue: string): TXMLFileTreeNodeToken;

implementation

const EnumToStrArray: array[TXMLFileTreeNodeToken] of string =
(
  'xmlfiletrntkNone',

  'xmlfiletrntkDocument',

  'xmlfiletrntkEoF',
  'xmlfiletrntkEoPg',
  'xmlfiletrntkEoLn',

  'xmlfiletrntkTab',
  'xmlfiletrntkSpace',

  'xmlfiletrntkComment',
  'xmlfiletrntkEncoding',

  'xmlfiletrntkBlock',
  'xmlfiletrntkSingle',

  'xmlfiletrntkText',
  'xmlfiletrntkEntity',
  'xmlfiletrntkHexaDecimal'
);

const EnumToTextArray: array[TXMLFileTreeNodeToken] of string =
(
  resxmlfiletrntkNone,

  resxmlfiletrntkDocument,

  resxmlfiletrntkEoF,
  resxmlfiletrntkEoPg,
  resxmlfiletrntkEoLn,

  resxmlfiletrntkTab,
  resxmlfiletrntkSpace,

  resxmlfiletrntkComment,
  resxmlfiletrntkEncoding,

  resxmlfiletrntkBlock,
  resxmlfiletrntkSingle,

  resxmlfiletrntkText,
  resxmlfiletrntkEntity,
  resxmlfiletrntkHexaDecimal
);

function TokenInRange
  (const AValue: TXMLFileTreeNodeToken): TXMLFileTreeNodeToken;
begin
  Result := AValue;

  if (Result < Low(TXMLFileTreeNodeToken)) then
  begin
    Result := Low(TXMLFileTreeNodeToken);
  end;

  if (Result > High(TXMLFileTreeNodeToken)) then
  begin
    Result := High(TXMLFileTreeNodeToken);
  end;
end;

function TokenToStr((* copy *) AValue: TXMLFileTreeNodeToken): string;
begin
  AValue := TokenInRange(AValue);
  Result := EnumToStrArray[AValue];
end;

function TokenToText((* copy *) AValue: TXMLFileTreeNodeToken): string;
begin
  AValue := TokenInRange(AValue);
  Result := EnumToTextArray[AValue];
end;

function StrToToken(const AValue: string): TXMLFileTreeNodeToken;
var i: TXMLFileTreeNodeToken; Found: Boolean;
begin
  i := Low(TXMLFileTreeNodeToken); Found := FALSE;
  while ((i <= High(TXMLFileTreeNodeToken)) and (not Found)) do
  begin
    Found := SameText(EnumToStrArray[i], AValue);
    Inc(i);
  end;

  if (Found)
    then Result := Pred(i)
    else Result := Low(TXMLFileTreeNodeToken);
end;

end.

