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

unit uktansicharsetconsts;

interface
uses
  SysUtils, Math,
  uktcomparisons, uktANSIChars, uktANSICharSets,
  dummy;

var
  BlanksSet, EoLnSet, NullSet: ansicharset;
  BoolSet: ansicharset;
  UpperSet, LowerSet: ansicharset;
  AlphaSet, DigitSet, FloatSet, CurrSet: ansicharset;
  IDSet, QIDSet, WildcardSet: ansicharset;

implementation

procedure UnitConstructor;
begin
  BlanksSet := #32#13#10;
  // BlanksSet := [#32, #13, #10];

  EoLnSet := #13#10;
  // EoLnSet := [#13, #10];

  NullSet := #0#13#10#26;
  // NullSet := [#0, #13, #10, #26];

  BoolSet := '0fF';
  // BoolSet := ['0', 'f', 'F'];

  // set of values used for FALSE representation
  // conjunto de valores utilizados para la representacion de FALSO

  uktansicharsets.Empty(UpperSet);
  uktansicharsets.IncludeRange(UpperSet, 'A', 'Z');
  //uktansicharsets.Include(UpperSet, chr(ord('Ñ')));
  // UpperSet := ['A'..'Z', 'Ñ'];

  uktansicharsets.Empty(LowerSet);
  uktansicharsets.IncludeRange(LowerSet, 'a', 'z');
  //uktansicharsets.Include(LowerSet, 'ñ');
  // LowerSet := ['a'..'z', 'ñ'];

  uktansicharsets.Empty(AlphaSet);
  uktansicharsets.IncludeRange(AlphaSet, 'A', 'Z');
  uktansicharsets.IncludeRange(AlphaSet, 'a', 'z');
  // AlphaSet  := ['A'..'Z', 'a'..'z'];

  uktansicharsets.Empty(DigitSet);
  uktansicharsets.IncludeRange(DigitSet, '0', '9');
  // DigitSet  := ['0'..'9'];

  uktansicharsets.Empty(FloatSet);
  uktansicharsets.IncludeRange(FloatSet, '0', '9');
  uktansicharsets.Include(FloatSet, '.');
  uktansicharsets.Include(FloatSet, ',');
  uktansicharsets.Include(FloatSet, '+');
  uktansicharsets.Include(FloatSet, '-');
  uktansicharsets.Include(FloatSet, 'E');
  uktansicharsets.Include(FloatSet, 'e');
  // FloatSet  := ['0'..'9', '.', ',', '+', '-', 'E', 'e'];

  uktansicharsets.Empty(CurrSet);
  uktansicharsets.IncludeRange(CurrSet, '0', '9');
  uktansicharsets.Include(CurrSet, '.');
  uktansicharsets.Include(CurrSet, ',');
  // CurrSet   := ['0'..'9', '.', ','];

  uktansicharsets.Empty(IDSet);
  uktansicharsets.IncludeRange(IDSet, 'A', 'Z');
  uktansicharsets.IncludeRange(IDSet, 'a', 'z');
  uktansicharsets.IncludeRange(IDSet, '0', '9');
  uktansicharsets.Include(IDSet, '_');
  // IDSet := ['A'..'Z', 'a'..'z', '_', '0'..'9'];

  WildcardSet := IDSet;
  uktansicharsets.Include(WildcardSet, '*');
  uktansicharsets.Include(WildcardSet, '?');
  // WildcardSet := ['A'..'Z', 'a'..'z', '_', '0'..'9', '*', '?'];

  QIDSet := IDSet;
  uktansicharsets.Include(QIDSet, '.');
  // QIDSet := ['A'..'Z', 'a'..'z', '_', '0'..'9', '.'];
end;

procedure UnitDestructor;
begin
  WildcardSet := '';
  IDSet     := '';
  QIDSet    := '';

  CurrSet   := '';
  FloatSet  := '';
  DigitSet  := '';
  AlphaSet  := '';

  LowerSet  := '';
  UpperSet  := '';

  BoolSet   := '';

  NullSet   := '';
  EoLnSet   := '';
  BlanksSet := '';
end;

initialization
  UnitConstructor;
finalization
  UnitDestructor;
end.
