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
 
unit uktxmlfileansisymbols;

interface
uses
  SysUtils, Classes,
  uktshortansistrs,
  uktxmlfiletokens,
  uktxmlfiletreenodetokens,
  dummy;

type
  TXMLFileANSISymbol = record
    Token:      TXMLFileToken;
    TreeToken:  TXMLFileTreeNodeToken;
    Text:       shortansistring;
    StartRow,
    StartCol,
    FinishRow,
    FinishCol:  Integer;
  end;
  PXMLFileANSISymbol = ^TXMLFileANSISymbol;

  TXMLFileANSISymbolrecord = record
    Token:      TXMLFileToken;
    TreeToken:  TXMLFileTreeNodeToken;
    Key:        Integer;
    StartRow,
    StartCol,
    FinishRow,
    FinishCol:  Integer;
  end;

  procedure Clear(var Symbol: TXMLFileANSISymbol);
  procedure Assign(var Dest, Source: TXMLFileANSISymbol);
  procedure Concat(var Dest, Source: TXMLFileANSISymbol);
  procedure Move(var Dest, Source: TXMLFileANSISymbol);
  procedure Exchange(var A, B: TXMLFileANSISymbol);

  procedure Compact
    (const Source: TXMLFileANSISymbol;
     SourceKey: Integer; var Dest: TXMLFileANSISymbolrecord);

implementation

procedure Clear(var Symbol: TXMLFileANSISymbol);
begin
  System.FillChar(Symbol, SizeOf(Symbol), 0);
end;

procedure Assign(var Dest, Source: TXMLFileANSISymbol);
begin
  Dest.Text      := Source.Text;
  Dest.Token     := Source.Token;
  Dest.TreeToken := Source.TreeToken;

  Dest.StartRow  := Source.StartRow;
  Dest.StartCol  := Source.StartCol;
  Dest.FinishRow := Source.FinishRow;
  Dest.FinishCol := Source.FinishCol;
end;

procedure Concat(var Dest, Source: TXMLFileANSISymbol);
begin
  Dest.Token := xmlfiletkText;
  Dest.Text  := Dest.Text + Source.Text;

  if (Dest.StartRow = 0) then
  begin
    Dest.StartRow := Source.StartRow;
  end;
  if (Dest.StartCol = 0) then
  begin
    Dest.StartCol := Source.StartCol;
  end;

  Dest.FinishRow := Source.FinishRow;
  Clear(Source);
end;

procedure Move(var Dest, Source: TXMLFileANSISymbol);
begin
  Dest := Source;
  Clear(Source);
end;

procedure Exchange(var A, B: TXMLFileANSISymbol);
var C: TXMLFileANSISymbol;
begin
  C := A;
  A := B;
  B := C;
end;

procedure Compact
 (const Source: TXMLFileANSISymbol;
  SourceKey: Integer; var Dest: TXMLFileANSISymbolrecord);
begin
  with Dest do
  begin
    Token     := Source.Token;
    Key       := SourceKey;
    StartRow  := Source.StartRow;
    StartCol  := Source.StartCol;
    FinishRow := Source.FinishRow;
    FinishCol := Source.FinishCol;
  end;
end;

end.
