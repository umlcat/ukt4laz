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

unit uktguidstrs;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  uktguids,
  ukthexas,
  uktansicharsets,
  uktansicharsetconsts,
  uktmaskarrays,
  dummy;

(**
 ** Description:
 ** This unit supports function for Global Unique Identifiers.
 **
 ** There are several string representations,
 ** all based on hexadecimal digits. The "hyphen" character,
 ** or "minus" operator characters are used as separators,
 ** and "curly brackets" character as delimiters.
 **
 ** - Single.
 ** 32 characters. Only hexadecimal digits,
 ** without separators, or delimiters.
 ** Example: "21EC20203AEA1069A2DD08002B30309D"
 **
 ** - Double
 ** 37 characters. Hexadecimal digits, with separators, and delimiters.
 ** The digits are grouped in groups of 8 digits, each.
 ** Example: "{21EC2020-3AEA1069-A2DD0800-2B30309D}"
 **
 ** + Long
 ** 38 characters. Hexadecimal digits, with separators, and delimiters.
 ** The digits are grouped as:
 ** - 8 digits,
 ** - 4 digits,
 ** - 4 digits,
 ** - 4 digits,
 ** - 12 digits
 ** Example: "{21EC2020-3AEA-1069-A2DD-08002B30309D}"
 **
 ** This is the most commonly used representation.
 **)

 resourcestring
   resGUIDSingleStrOverflow = 'Single String GUID overflow';
   resGUIDDoubleStrOverflow = 'Double String GUID overflow';
   resGUIDLongStrOverflow   = 'Long String GUID overflow';

 (*
 resource
   resGUIDSingleStrOverflow: string = '';
   resGUIDDoubleStrOverflow: string = '';
   resGUIDLongStrOverflow:   string = '';
 *)

const
  SingleGUIDStrMaxSize = 32;
  DoubleGUIDStrMaxSize = 37;
  LongGUIDStrMaxSize   = 38;
  PascalArrayGUIDStrMaxSize = 66;
  PlainCArrayGUIDStrMaxSize = 82;

 (* String types *)

 type
   TSingleGUIDStr = string[SingleGUIDStrMaxSize];
 type
   PSingleGUIDStr = ^TSingleGUIDStr;

 type
   TDoubleGUIDStr = string[DoubleGUIDStrMaxSize];
 type
   PDoubleGUIDStr = ^TDoubleGUIDStr;

 type
   TLongGUIDStr = string[LongGUIDStrMaxSize];
 type
   PLongGUIDStr = ^TLongGUIDStr;

 (* Array types *)

 type
   TSingleGUIDArray = array[0 .. 31] of ansichar;
 type
   PSingleGUIDArray = ^TSingleGUIDArray;
 type
   TDoubleGUIDArray = array[0 .. 36] of ansichar;
 type
   PDoubleGUIDArray = ^TDoubleGUIDArray;
 type
   TLongGUIDArray = array[0 .. 37] of ansichar;
 type
   PLongGUIDArray = ^TLongGUIDArray;

   (* Other types *)

 type
   EGUIDOverflow = exception;

  (* Global Functions *)

  function IndexNonSingleGUIDStr
    (const ASource: shortstring): Integer;
  function IndexNonDoubleGUIDStr
    (const ASource: shortstring): Integer;
  function IndexNonLongGUIDStr
    (const ASource: shortstring): Integer;

  function IndexNonSingleGUIDAnsiCharPtr
    (const ASource: PChar): Integer;
  function IndexNonDoubleGUIDAnsiCharPtr
    (const ASource: PChar): Integer;
  function IndexNonLongGUIDAnsiCharPtr
    (const ASource: PChar): Integer;

  function IsSingleGUIDStr
    (const ASource: TSingleGUIDStr): Boolean;
  function IsDoubleGUIDStr
    (const ASource: TDoubleGUIDStr): Boolean;
  function IsLongGUIDStr
    (const ASource: TLongGUIDStr): Boolean;

  function IsSingleGUIDAnsiCharPtr
    (const ASource: PChar): Boolean;
  function IsDoubleGUIDAnsiCharPtr
    (const ASource: PChar): Boolean;
  function IsLongGUIDAnsiCharPtr
    (const ASource: PChar): Boolean;

  function TrySingleStrToGUID
    (const ASource: TSingleGUIDStr; var ADest: TGUID): Boolean;
  function TryDoubleStrToGUID
    (const ASource: TDoubleGUIDStr; var ADest: TGUID): Boolean;
  function TryLongStrToGUID
    (const ASource: TLongGUIDStr; var ADest: TGUID): Boolean;

  function TrySingleStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID): Boolean;
  function TryDoubleStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID): Boolean;
  function TryLongStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID): Boolean;

  procedure SingleStrToGUID
    (const ASource: TSingleGUIDStr; var ADest: TGUID);
  procedure DoubleStrToGUID
    (const ASource: TSingleGUIDStr; var ADest: TGUID);
  procedure LongStrToGUID
    (const ASource: TLongGUIDStr; var ADest: TGUID);

  procedure SingleStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID);
  procedure DoubleStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID);
  procedure LongStrToGUIDAnsiCharPtr
    (const ASource: PChar; var ADest: TGUID);

  procedure ClearSingleGUIDStr(var ADest: TSingleGUIDStr);

  procedure GUIDToSingleStr
    (const ASource: TGUID; var ADest: TSingleGUIDStr);
  procedure GUIDToDoubleStr
    (const ASource: TGUID; var ADest: TDoubleGUIDStr);
  procedure GUIDToLongStr
    (const ASource: TGUID; var ADest: TLongGUIDStr);

  procedure GUIDToSingleAnsiCharPtr
    (const ASource: TGUID; var ADest: PChar);
  procedure GUIDToDoubleAnsiCharPtr
    (const ASource: TGUID; var ADest: PChar);
  procedure GUIDToLongAnsiCharPtr
    (const ASource: TGUID; var ADest: PChar);

  procedure GUIDToPascalByteArray
    (const ASource: TGUID; var ADest: ansistring);
  procedure GUIDToPlainCByteArray
    (const ASource: TGUID; var ADest: ansistring);

implementation

function IndexNonSingleGUIDStr
  (const ASource: shortstring): Integer;
var ALen: Integer; AMask: PMaskArray;
begin
  Result := -1;

  ALen := Length(ASource);
  if (ALen = SingleGUIDStrMaxSize) then
  begin
    Result := IndexNonHexShortStr(ASource, ALen);
  end;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IndexNonDoubleGUIDStr
  (const ASource: shortstring): Integer;
var ALen: Integer; AMask: PMaskArray;
begin
  Result := -1;

  ALen := Length(ASource);
  if (ALen = DoubleGUIDStrMaxSize) then
  begin
    AMask := NewClearMaskArray(ALen);
    // indicate all characters are hexadecimal digits
    FillMaskArray(AMask, ALen);

    // mark skip initial delimiter
    MaskArraySetAt(AMask, ALen, 0, false);

    // mark skip first separator
    MaskArraySetAt(AMask, ALen, 9, false);

    // mark skip second separator
    MaskArraySetAt(AMask, ALen, 18, false);

    // mark skip third separator
    MaskArraySetAt(AMask, ALen, 27, false);

    // mark skip final delimiter
    MaskArraySetAt(AMask, ALen, (ALen - 1), false);

    // compare only the characters indicated by the mask array
    Result := ukthexas.IndexNonHexaStrByMask(ASource, AMask, ALen);
  end;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IndexNonLongGUIDStr
  (const ASource: shortstring): Integer;
var ALen: Integer; AMask: PMaskArray;
begin
  Result := -1;

  ALen := Length(ASource);
  if (ALen = DoubleGUIDStrMaxSize) then
  begin
    AMask := NewClearMaskArray(ALen);
    // indicate all characters are hexadecimal digits
    FillMaskArray(AMask, ALen);

    // mark skip initial delimiter
    MaskArraySetAt(AMask, ALen, 0, false);

    // mark skip first separator
    MaskArraySetAt(AMask, ALen, 9, false);

    // mark skip second separator
    MaskArraySetAt(AMask, ALen, 14, false);

    // mark skip third separator
    MaskArraySetAt(AMask, ALen, 19, false);

    // mark skip final delimiter
    MaskArraySetAt(AMask, ALen, (ALen - 1), false);

    // compare only the characters indicated by the mask array
    Result := ukthexas.IndexNonHexaStrByMask(ASource, AMask, ALen);
  end;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IndexNonSingleGUIDAnsiCharPtr(const ASource: PChar): Integer;
begin
  Result := -1;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IndexNonDoubleGUIDAnsiCharPtr(const ASource: PChar): Integer;
begin
  Result := -1;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IndexNonLongGUIDAnsiCharPtr(const ASource: PChar): Integer;
begin
  Result := -1;
  // Goal: Returns the index of the first character,
  // in the given parameter, that doesn't match
  // the required mask or pattern,
  // of the indicated GUID, string format.
  // If all characters match,
  // then, "-1" is returned.
end;

function IsSingleGUIDStr
  (const ASource: TSingleGUIDStr): Boolean;
begin
  Result := (IndexNonSingleGUIDStr(ASource) = -1);
end;

function IsDoubleGUIDStr
  (const ASource: TDoubleGUIDStr): Boolean;
begin
  Result := (IndexNonDoubleGUIDStr(ASource) = -1);
end;

function IsLongGUIDStr(const ASource: TLongGUIDStr): Boolean;
begin
  Result := (IndexNonLongGUIDStr(ASource) = -1);
end;

function IsSingleGUIDAnsiCharPtr(const ASource: PChar): Boolean;
begin
  Result := (IndexNonSingleGUIDAnsiCharPtr(ASource) = -1);
end;

function IsDoubleGUIDAnsiCharPtr(const ASource: PChar): Boolean;
begin
  Result := (IndexNonDoubleGUIDAnsiCharPtr(ASource) = -1);
end;

function IsLongGUIDAnsiCharPtr(const ASource: PChar): Boolean;
begin
  Result := (IndexNonLongGUIDAnsiCharPtr(ASource) = -1);
end;

function TrySingleStrToGUID
  (const ASource: TSingleGUIDStr; var ADest: TGUID): Boolean;
var S1, S2: PChar; D: PByte; ACount, K: Integer; S: ansistring;
begin
  Result := IsSingleGUIDStr(ASource);
  if (Result) then
  begin
    ClearGUID(ADest);

    // --> prepare pointers
    D  := @ADest;
    // characters will be copied 2, at a time,
    // since each byte is converted as 2 hexadecimal digits
    S1 := @(ASource[1]);
    S2 := @(ASource[2]);

    ACount := 16;
    K  := 0;
    while (K < ACount) do
    begin
      // merge 2 digits into a byte
      S  := S1^ + S2^;
      D^ := HexToByte(S);

      Inc(D);

      Inc(S1, 2);
      Inc(S2, 2);

      Inc(K);
    end;
  end;
  // Goal: To convert a partial readeable text into a GUID type.
end;

function TryDoubleStrToGUID
  (const ASource: TDoubleGUIDStr; var ADest: TGUID): Boolean;
var S1, S2: PChar; D: PByte; ACount, I, J: Integer; S: ansistring;
    U: PUUID;
begin
  Result := false;
  Result := IsDoubleGUIDStr(ASource);
  if (Result) then
  begin
    ClearGUID(ADest);

    // --> prepare pointers
    D  := @ADest;
    U  := @ADest;
    // characters will be copied 2, at a time,
    // since each byte is converted as 2 hexadecimal digits
    S1 := @(ASource[2]);
    S2 := @(ASource[3]);
    // first character is skipped, is initial delimiter

    // each group
    for I := 1 to 4 do
    begin
      for J := 1 to 4 do
      begin
        // merge 2 digits into a byte
        S  := S1^ + S2^;
        D^ := HexToByte(S);

        Inc(D);

        Inc(S1, 2);
        Inc(S2, 2);
      end;

      if (I < 4) then
      begin
        // skip separator
        Inc(S1, 1);
        Inc(S2, 1);
      end;
    end;
  end;
  // Goal: To convert a full readeable text into a GUID type.
end;

function TryLongStrToGUID
  (const ASource: TLongGUIDStr; var ADest: TGUID): Boolean;
var S1, S2: PChar; D: PByte; ACount, I, J: Integer; S: ansistring;
    U: PUUID;
begin
  Result := false;
  Result := IsLongGUIDStr(ASource);
  if (Result) then
  begin
    ClearGUID(ADest);

    // --> prepare pointers
    D  := @ADest;
    U  := @ADest;
    // characters will be copied 2, at a time,
    // since each byte is converted as 2 hexadecimal digits
    S1 := @(ASource[2]);
    S2 := @(ASource[3]);
    // first character is skipped, is initial delimiter

    // first 4 bytes
    for I := 1 to 4 do
    begin
      // merge 2 digits into a byte
      S  := S1^ + S2^;
      D^ := HexToByte(S);

      Inc(D);

      Inc(S1, 2);
      Inc(S2, 2);
    end;

    // skip separator
    Inc(S1, 1);
    Inc(S2, 1);

    // next 3 groups
    for J := 1 to 3 do
    begin
      // 2 bytes each
      for I := 1 to 2 do
      begin
        // merge 2 digits into a byte
        S  := S1^ + S2^;
        D^ := HexToByte(S);

        Inc(D);

        Inc(S1, 2);
        Inc(S2, 2);

        // skip separator
        Inc(S1, 1);
        Inc(S2, 1);
      end;
    end;

    // last 6 bytes
    for I := 1 to 6 do
    begin
      // merge 2 digits into a byte
      S  := S1^ + S2^;
      D^ := HexToByte(S);

      Inc(D);

      Inc(S1, 2);
      Inc(S2, 2);
    end;
  end;
  // Goal: To convert a full readeable text into a GUID type.
end;

function TrySingleStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID): Boolean;
begin
  Result := false;
end;

function TryDoubleStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID): Boolean;
begin
  Result := false;
end;

function TryLongStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID): Boolean;
begin
  Result := false;
end;

procedure SingleStrToGUID(const ASource: TSingleGUIDStr; var ADest: TGUID);
begin
  if (not TrySingleStrToGUID(ASource, ADest)) then
  begin
     raise EGUIDOverflow.Create(resGUIDSingleStrOverflow);
  end;
  // Goal: To convert a partial readeable text into a GUID type.
end;

procedure DoubleStrToGUID(const ASource: TSingleGUIDStr; var ADest: TGUID);
begin
  if (not TryDoubleStrToGUID(ASource, ADest)) then
  begin
     raise EGUIDOverflow.Create(resGUIDDoubleStrOverflow);
  end;
  // Goal: To convert a full readeable text into a GUID type.
end;

procedure LongStrToGUID
  (const ASource: TLongGUIDStr; var ADest: TGUID);
begin
  if (not TryLongStrToGUID(ASource, ADest)) then
  begin
     raise EGUIDOverflow.Create(resGUIDLongStrOverflow);
  end;
  // Goal: To convert a full readeable text into a GUID type.
end;

procedure SingleStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID);
begin
  // ...
end;

procedure DoubleStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID);
begin
  // ...
end;

procedure LongStrToGUIDAnsiCharPtr
  (const ASource: PChar; var ADest: TGUID);
begin
  // ...
end;

procedure ClearSingleGUIDStr(var ADest: TSingleGUIDStr);
begin
  FillZeroShortStr(ADest, SizeOf(TSingleGUIDStr));
  // Goal: Returns a GUID null value with a short textual representation.
end;

procedure GUIDToSingleStr
  (const ASource: TGUID; var ADest: TSingleGUIDStr);
var S: shortstring;
    D1, D2: PChar; P: PByte; K, ACount: Integer;
begin
  // clear result
  FillZeroShortStr(ADest, SizeOf(TSingleGUIDStr));

  ACount := 16;

  // --> prepare pointers
  P  := @ASource;
  // characters will be copied 2, at a time,
  // since each byte is converted as 2 hexadecimal digits
  D1 := @(ADest[1]);
  D2 := @(ADest[2]);

  K  := 0;
  while (K < ACount) do
  begin
    S  := ByteToHex(P^);
    D1^ := S[1];
    D2^ := S[2];

    Inc(D1, 2);
    Inc(D2, 2);
    Inc(P);
    Inc(K);
  end;

  // ...
  SetLength(ADest, SingleGUIDStrMaxSize);
  // Goal: To convert a GUID type into a partial readeable text.
end;

procedure GUIDToDoubleStr(const ASource: TGUID; var ADest: TDoubleGUIDStr);
var S: shortstring; I, J: Integer;
    D1, D2: PChar; P: PByte; ACount: Integer;
begin
  // --> fill with '0' character
  FillZeroShortStr(ADest, SizeOf(TDoubleGUIDStr));

  // --> prepare pointers
  P  := @ASource;
  // characters will be copied 2, at a time,
  // since each byte is converted as 2 hexadecimal digits
  D1 := @(ADest[1]);
  D2 := @(ADest[2]);

  // --> add initial delimiter
  D1 := @(ADest[1]);
  D1^ := '{';
  Inc(D1, 1);
  Inc(D2, 1);

  // --> add groups
  for I := 1 to 4 do
  begin
    for J := 1 to 4 do
    begin
      S  := ByteToHex(P^);
      D1^ := S[1];
      D2^ := S[2];

      Inc(D1, 2);
      Inc(D2, 2);
      Inc(P);
    end;

    // add separator
    if (I < 4) then
    begin
      D1^ := '-';
      Inc(D1, 1);
      Inc(D2, 1);
    end;
  end;

  // --> add final delimiter
  D1^ := '}';

  // ...
  SetLength(ADest, DoubleGUIDStrMaxSize);

  // Goal: To convert a GUID type into a full readeable text.
end;

procedure GUIDToLongStr
  (const ASource: TGUID; var ADest: TLongGUIDStr);
var S: shortstring; I, J: Integer;
    D1, D2: PChar; P: PByte; ACount: Integer;
begin
  // --> fill with '0' character
  FillZeroShortStr(ADest, SizeOf(TLongGUIDStr));

  // --> prepare pointers
  P  := @ASource;
  // characters will be copied 2, at a time,
  // since each byte is converted as 2 hexadecimal digits
  D1 := @(ADest[1]);
  D2 := @(ADest[2]);

  // --> add initial delimiter
  D1 := @(ADest[1]);
  D1^ := '{';
  Inc(D1, 1);
  Inc(D2, 1);

  // --> add groups

  // first group of 4 bytes
  for I := 1 to 4 do
  begin
    S  := ByteToHex(P^);
    D1^ := S[1];
    D2^ := S[2];

    Inc(D1, 2);
    Inc(D2, 2);
    Inc(P);
  end;

  // add first group separator
  D1^ := '-';
  Inc(D1, 1);
  Inc(D2, 1);

  // 3 groups of 2 bytes
  for I := 1 to 3 do
  begin
    for J := 1 to 2 do
    begin
      S  := ByteToHex(P^);
      D1^ := S[1];
      D2^ := S[2];

      Inc(D1, 2);
      Inc(D2, 2);
      Inc(P);
    end;

    // add each group separator
    D1^ := '-';
    Inc(D1, 1);
    Inc(D2, 1);
  end;

  // last group of 6 bytes
  for I := 1 to 6 do
  begin
    S  := ByteToHex(P^);
    D1^ := S[1];
    D2^ := S[2];

    Inc(D1, 2);
    Inc(D2, 2);
    Inc(P);
  end;

  // --> add final delimiter
  D1^ := '}';

  // ...
  SetLength(ADest, LongGUIDStrMaxSize);
  // Goal: To convert a GUID type into a full readeable text.
end;

procedure GUIDToSingleAnsiCharPtr
  (const ASource: TGUID; var ADest: PChar);
begin
  // ...
end;

procedure GUIDToDoubleAnsiCharPtr
  (const ASource: TGUID; var ADest: PChar);
begin
  // ...
end;

procedure GUIDToLongAnsiCharPtr
  (const ASource: TGUID; var ADest: PChar);
begin
  // ...
end;

procedure GUIDToPascalByteArray
  (const ASource: TGUID; var ADest: ansistring);
var S: string; K: Integer;
    D1, D2, D3, D4: PChar; P: PByte; ACount: Integer;
begin
  // --> fill with '0' character
  ACount := PascalArrayGUIDStrMaxSize;
  FillZeroAnsiStr(ADest, ACount);

  // --> prepare pointers
  P  := @ASource;
  // characters will be copied 2, at a time,
  // since each byte is converted as 2 hexadecimal digits
  // additionally a third character, a colon, will be included
  // additionally a fourth character, a money sign, will be included
  D1 := @(ADest[1]);
  D2 := @(ADest[2]);
  D3 := @(ADest[3]);
  D4 := @(ADest[4]);

  // --> add initial delimiter
  D1 := @(ADest[1]);
  D1^ := '(';
  Inc(D1, 1);
  Inc(D2, 1);
  Inc(D3, 1);
  Inc(D4, 1);

  ACount := 16;
  K  := 0;
  while (K < ACount) do
  begin
    S  := ByteToHex(P^);
    D1^ := '$';
    D2^ := S[1];
    D3^ := S[2];

    if (K < (ACount - 1)) then
    begin
      D4^ := ',';
    end else
    begin
      D4^ := ')';
    end;

    Inc(D1, 4);
    Inc(D2, 4);
    Inc(D3, 4);
    Inc(D4, 4);

    Inc(P);
    Inc(K);
  end;

  // --> add final delimiter
  D1^ := ';';
end;

procedure GUIDToPlainCByteArray(const ASource: TGUID; var ADest: ansistring);
var S: string; K: Integer;
    P: PByte; ACount: Integer;
    D1, D2, D3, D4, D5: PChar;
begin
  // --> fill with '0' character
  ACount := PlainCArrayGUIDStrMaxSize;
  FillZeroAnsiStr(ADest, ACount);

  // --> prepare pointers
  P  := @ASource;
  // characters will be copied 2, at a time,
  // since each byte is converted as 2 hexadecimal digits
  D1 := @(ADest[1]); // "0"
  D2 := @(ADest[2]); // "x"
  D3 := @(ADest[3]); // 0 ... F
  D4 := @(ADest[4]); // 0 ... F
  D5 := @(ADest[5]); // ',' or ')'

  // --> add initial delimiter
  D1 := @(ADest[1]);
  D1^ := '{';
  Inc(D1, 1);
  Inc(D2, 1);
  Inc(D3, 1);
  Inc(D4, 1);
  Inc(D5, 1);

  ACount := 16;
  K  := 0;
  while (K < ACount) do
  begin
    S  := ByteToHex(P^);
    D1^ := '0';
    D2^ := 'x';
    D3^ := S[1];
    D4^ := S[2];

    if (K < (ACount - 1)) then
    begin
      D5^ := ',';
    end else
    begin
      D5^ := '}';
    end;

    Inc(D1, 5);
    Inc(D2, 5);
    Inc(D3, 5);
    Inc(D4, 5);
    Inc(D5, 5);

    Inc(P);
    Inc(K);
  end;

  // --> add final delimiter
  D1^ := ';';
end;

end.

