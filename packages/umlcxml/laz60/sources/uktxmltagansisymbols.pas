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

unit uktxmltagansisymbols;

interface
uses
  SysUtils,
  uktxmltagTokens;

type
  TXMLTagANSISymbol = record
    Token:      TXMLTagToken;
    Text:       ANSIstring;
    StartRow,
    StartCol,
    FinishRow,
    FinishCol:  Integer;
  end;
  POMLLiteTagANSISymbol = ^TXMLTagANSISymbol;

  procedure Clear(var Symbol: TXMLTagANSISymbol);

implementation

procedure Clear(var Symbol: TXMLTagANSISymbol);
begin
  System.FillChar(Symbol, SizeOf(Symbol), 0);
end;

end.
