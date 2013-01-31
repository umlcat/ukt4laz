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

unit ukthexas;

interface
uses
  SysUtils,
  Math,
  uktdecs,
  uktmaskarrays,
  dummy;

resourcestring
  resHexOverflow = 'Hexadecimal digit overflow';
(*
resource
  resHexaOverflow: string = '';
*)

type
  hexadecimal  = char;

  hexastring   = ansistring;

  THexArray    = array [0 .. 65535] of char;
  PHexArray    = ^THexArray;

  EHexOverflow = exception;

  function TryHexToByte(const Source: hexastring; var Dest: Byte): Boolean;
  function TryHexToWord(const Source: hexastring; var Dest: Word): Boolean;
  function TryHexToLongWord(const Source: hexastring; var Dest: LongWord): Boolean;

  function HexToDec({in} AValue: hexadecimal): decimal;
  function HexToInt(const AValue: hexastring): Integer;
  function HexToByte(const AValue: hexastring): Byte;
  function HexToWord(const AValue: hexastring): Word;
  function HexToLongWord(const AValue: hexastring): LongWord;

  function DecToHex({in} AValue: decimal): hexadecimal;
  function IntToHex({in} AValue: Integer): hexastring;
  function ByteToHex(const AValue: Byte): hexastring;
  function WordToHex(const AValue: Word): hexastring;
  function LongWordToHex(const AValue: LongWord): hexastring;

  procedure FillZeroOpenStr(var ADest: string; ASize: Integer);
  procedure FillZeroAnsiStr(var ADest: ansistring; ASize: Integer);
  procedure FillZeroShortStr(var ADest: shortstring; ASize: Integer);
  procedure FillZeroCharPtr(var ADest: PChar; ASize: Integer);

  function IsHexDigit(AValue: char): Boolean;

  function IndexNonHexShortStr
    (const ADest: shortstring; ASize: Integer): Integer;
  function IndexNonHexAnsiCharPtr
    (const ADest: PChar; ASize: Integer): Integer;

  function IsHexShortStr
    (const ADest: shortstring; ASize: Integer): Boolean;
  function IsHexAnsiCharPtr
    (const ADest: PChar; ASize: Integer): Boolean;

  function IndexNonHexaStrByMask
    (const ASource: shortstring; AMask: PMaskArray; ASize: Integer): Integer;
  function IndexNonHexaAnsiCharPtrByMask
    (const ASource: PChar; AMask: PMaskArray; ASize: Integer): Integer;

implementation

const HexChars: array[0..15] of char =
   ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F');

function HexToDec({in} AValue: hexadecimal): decimal;
var I: Integer; Found: Boolean;
begin
  Result := 0;
  AValue := UpCase(AValue);

  I := 0; Found := FALSE;
  while (not Found) and (I <= 15) do
  begin
    Found := (AValue = HexChars[i]);
    Inc(I);
  end;

  if (Found) then
  begin
    Result := Pred(I);
  end;
end;

function HexToInt(const AValue: hexastring): Integer;
var I, L, J, D: Integer; C: char;
begin
  Result := 0; L := System.Length(AValue);
  J := 0;
  for I := L downto 1 do
  begin
    C := AValue[i];
    D := HexToDec(C);
    D := Trunc(D * IntPower(16, J));
    Result := Result + D;
    Inc(J);
  end;
end;

function TryHexToByte(const Source: hexastring; var Dest: Byte): Boolean;
var L: Integer;
begin
  L := System.Length(Source); Dest := 0;
  Result := (L > 0) and (L < 3);

  if (Result) then
  begin
    Dest := HexToInt(Source);
  end;
end;

function TryHexToWord(const Source: hexastring; var Dest: Word): Boolean;
var L: Integer;
begin
  L := System.Length(Source); Dest := 0;
  Result := (L > 0) and (L < 5);

  if (Result) then
  begin
    Dest := HexToInt(Source);
  end;
end;

function TryHexToLongWord(const Source: hexastring; var Dest: LongWord): Boolean;
var L: Integer;
begin
  L := System.Length(Source); Dest := 0;
  Result := (L > 0) and (L < 9);

  if (Result) then
  begin
    Dest := HexToInt(Source);
  end;
end;

function HexToByte(const AValue: hexastring): Byte;
begin
  if (not TryHexToByte(AValue, Result)) then
  begin
    raise EHexOverflow.Create(resHexOverflow);
  end;
end;

function HexToWord(const AValue: hexastring): Word;
begin
  if (not TryHexToWord(AValue, Result)) then
  begin
    raise EHexOverflow.Create(resHexOverflow);
  end;
end;

function HexToLongWord(const AValue: hexastring): LongWord;
begin
  if (not TryHexToLongWord(AValue, Result)) then
  begin
    raise EHexOverflow.Create(resHexOverflow);
  end;
end;

function DecToHex({in} AValue: decimal): hexadecimal;
begin
  Result := IntToStr(AValue)[1];
end;

function IntToHex({in} AValue: Integer): hexastring;
var I: Integer;
begin
  Result := '';
  repeat
    I := AValue mod 16;
    AValue := AValue div 16;
    Result := Concat(HexChars[I], Result);
  until (AValue < 1);
end;

function ByteToHex(const AValue: Byte): hexastring;
begin
  Result := '';
  Result := Result + HexChars[AValue shr 4];
  Result := Result + HexChars[AValue and 15];
end;

function WordToHex(const AValue: Word): hexastring;
begin
  Result := IntToHex(AValue);
end;

function LongWordToHex(const AValue: LongWord): hexastring;
begin
  Result := IntToHex(AValue);
end;

procedure FillZeroOpenStr(var ADest: string; ASize: Integer);
var I: Integer;
begin
  SetLength(ADest, ASize);
  for I := 1 to ASize do
  begin
    ADest[I] := '0';
  end;
end;

procedure FillZeroAnsiStr(var ADest: ansistring; ASize: Integer);
begin
  System.SetLength(ADest, ASize);
  System.FillChar(ADest[1], ASize, '0');
  // Goal: fills with zero a pascal string.
end;

procedure FillZeroShortStr(var ADest: shortstring; ASize: Integer);
begin
  System.FillChar(ADest, ASize, '0');
  ADest[0] := chr(ASize);
  // Goal: fills with zero a pascal string.
end;

procedure FillZeroCharPtr(var ADest: PChar; ASize: Integer);
begin
  System.FillChar(ADest^, ASize, '0');
  // Goal: fills with zero an array of ansichar.
end;

function IsHexDigit(AValue: char): Boolean;
begin
  Result := false;
  AValue := UpCase(AValue);
  Result :=
    (
      ((AValue >= '0') or (AValue <= '9')) or
      ((AValue >= 'A') or (AValue <= 'F'))
    );
end;

function IndexNonHexShortStr
  (const ADest: shortstring; ASize: Integer): Integer;
var I: Integer; CanContinue: Boolean;
begin
  Result := -1;

  if (ASize > 0) then
  begin
    CanContinue := true;
    I := 1;
    while ((I <= ASize) and CanContinue) do
    begin
      CanContinue := IsHexDigit(ADest[I]);
      Inc(I);
    end;

    if (not CanContinue) then
    begin
      Result := (I - 1);
    end;
  end;
  // Goal: Returns the index of a non hexadecimal character,
  // in the given parameter. If all characters are hexadecimal digits,
  // then, "-1" is returned.
end;

function IndexNonHexAnsiCharPtr
  (const ADest: PChar; ASize: Integer): Integer;
var I: Integer; CanContinue: Boolean; P: PChar;
begin
  Result := -1;

  if (ASize > 0) then
  begin
    P := @ADest;
    CanContinue := true;
    I := 1;
    while ((I <= ASize) and CanContinue) do
    begin
      CanContinue := IsHexDigit(P^);
      Inc(P);
      Inc(I);
    end;

    if (not CanContinue) then
    begin
      Result := (I - 1);
    end;
  end;
  // Goal: Returns if there is a non hexadecimal character,
  // in the given parameter.
end;

function IsHexShortStr
  (const ADest: shortstring; ASize: Integer): Boolean;
begin
  Result := (IndexNonHexShortStr(ADest, ASize) = -1);
  // Goal: Returns if there is a non hexadecimal character,
  // in the given parameter.
end;

function IsHexAnsiCharPtr(const ADest: PChar; ASize: Integer): Boolean;
begin
  Result := (IndexNonHexAnsiCharPtr(ADest, ASize) = -1);
  // Goal: Returns if there is a non hexadecimal character,
  // in the given parameter.
end;

function IndexNonHexaStrByMask
  (const ASource: shortstring; AMask: PMaskArray; ASize: Integer): Integer;
var ALen, AIndex: Integer;
    CanValidate, CanContinue: Boolean;
begin
  Result := -1;

  AIndex := 0;
  CanContinue := true;
  while ((AIndex < ASize) and CanContinue) do
  begin
    CanValidate := MaskArrayGetAt(AMask, ASize, AIndex);
    if (CanValidate) then
    begin
      CanContinue := IsHexDigit(ASource[AIndex + 1]);
    end;
    Inc(AIndex);
  end;

  if (not CanContinue) then
  begin
    Result := AIndex;
  end;
  // Goal: Receives an array of char ("ADest"),
  // and and array of booleans ("AMask"), of the same length,
  // and checks only the characters indicated by its matching
  // boolean values, are hexadecimal digits.
  // other characters, are ignored.
  //
  // Warning: if the string is empty, it returns as if not error found.
end;

function IndexNonHexaAnsiCharPtrByMask
  (const ASource: PChar; AMask: PMaskArray; ASize: Integer): Integer;
begin
  Result := -1;
  // Goal: Receives an array of char ("ADest"),
  // and and array of booleans ("AMask"), of the same length,
  // and checks only the characters indicated by its matching
  // boolean values, are hexadecimal digits.
  // other characters, are ignored.
  //
  // Warning: if the string is empty, it returns as if not error found.
end;

end.
