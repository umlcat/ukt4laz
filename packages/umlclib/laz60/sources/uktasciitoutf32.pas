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

unit uktasciitoutf32;

(* sdvANSIToUTF32 *)

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, //Messages, Consts,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  Math,
  SysConst,
  SysUtils;

type
  UTF32Byte  = array[0..3] of byte;
  TUTF32Byte = UTF32Byte;
  PUTF32Byte = ^UTF32Byte;

  // Warning: "pointer to array" seems to store,
  // additional info, not just the items, like "C"

  ASCIIArray  = array[0..255] of char;
  TASCIIArray = ASCIIArray;
  PASCIIArray = ^TASCIIArray;

  UTF32Array  = array[0..255] of UTF32Byte;
  TUTF32Array = UTF32Array;
  PUTF32Array = ^TUTF32Array;

  function ExChr(const Value: Byte): string;

  function StrToConstPtr(const Value: string): pointer;

  function NewUTF32Buffer(const ACount: Word): PUTF32Byte;
  procedure DisposeUTF32Buffer(var ADest: PUTF32Byte; const ACount: Word);

  ///<summary>
  ///Converts an A.S.C.I.I. string into a 4 byte U.T.F.-32 string
  ///<param name="src">
  ///</param>
  ///<param name="dest">
  ///</param>
  ///<param name="srccount">
  ///</param>
  ///<param name="srcmaxsize">
  ///</param>
  ///<param name="destmaxsize">
  ///</param>
  ///</summary>
  procedure ASCIIToUTF32
    (
      //const src: (* const *) ^byte
      const src:  pointer; // address of 1 byte ascii char item [0] source str.
      const dest: pointer; // address of 4 bytes item [0] dest. array
      const srccount: word; // how many source characters / bytes to copy
      const srcmaxsize: word; // source maximum bytes length
      const destmaxsize: word // destination maximum bytes length
    );

  ///<summary>
  ///Converts an 4 byte U.T.F.-32 string into an A.S.C.I.I. string
  ///<param name="src">
  ///</param>
  ///<param name="dest">
  ///</param>
  ///<param name="srccount">
  ///</param>
  ///<param name="srcmaxsize">
  ///</param>
  ///<param name="destmaxsize">
  ///</param>
  ///</summary>
  procedure UTF32ToASCII
    (
      const src:  pointer; // address of 4 bytes item [0] source array
      const dest: pointer; // address of 1 byte ascii char item [0] dest. string
      const srccount: word; // how many source characters to copy
      const srcmaxsize: word; // source maximum bytes length
      const destmaxsize: word // destination maximum bytes length
    );

implementation

function ExChr(const Value: Byte): string;
begin
  Result := '';

  case (ord(Value)) of
    0: Result := '{null}';
    else
      Result := chr(Value);
  end;
end;

function StrToConstPtr(const Value: string): pointer;
begin
  Result := @(Value[1]);
end;

function NewUTF32Buffer(const ACount: Word): PUTF32Byte;
var ASize: Word;
begin
  Result := NIL;
  ASize := (ACount * SizeOf(UTF32Byte));

  GetMem(Result, ASize);
  //FillChar(Result, 0, ASize);
end;

procedure DisposeUTF32Buffer(var ADest: PUTF32Byte; const ACount: Word);
var ASize: Word;
begin
  ASize := (ACount * SizeOf(UTF32Byte));

  FreeMem(ADest, ASize);
  ADest := NIL;
end;

procedure ASCIIToUTF32
  (
    const src: pointer; // address of ascii char item [0] string
    const dest: pointer; // address of 4 bytes item [0] array
    const srccount: word; // how many source characters to copy
    const srcmaxsize: word; // source maximum bytes length
    const destmaxsize: word // destination maximum bytes length
  );
var s: pbyte;  d: PUTF32Byte; //x: PASCIIArray;
    CanContinue: Boolean;
    scount: word;
    j: word;
begin
  CanContinue :=
    ( (srccount > 0) and (srcmaxsize > 0) and (destmaxsize > 0) );
  if (CanContinue) then
  begin
    scount := Min(srccount, srcmaxsize);
    scount := Min(scount, destmaxsize);

    CanContinue := (scount > 0);
    if (CanContinue) then
    begin
      s := src;
      d := dest;
      //x := dest;

      for j := 1 to scount do
      begin
        d^[0] := s^;
        d^[1] := 0;
        d^[2] := 0;
        d^[3] := 0;

        Inc(s);
        Inc(d);
      end;
    end;
  end;
end;

procedure UTF32ToASCII
  (
    const src:  pointer; // address of 4 bytes item [0] source array
    const dest: pointer; // address of 1 byte ascii char item [0] dest. string
    const srccount: word; // how many source 4 bytes-characters to copy
    const srcmaxsize: word; // source maximum bytes length
    const destmaxsize: word // destination maximum bytes length
  );
var s: PUTF32Byte;  d: pbyte;
    x: PASCIIArray;
    CanContinue: Boolean;
    scount: word;
    j: word;
begin
  CanContinue :=
    ( (srccount > 0) and (srcmaxsize > 0) and (destmaxsize > 0) );
  if (CanContinue) then
  begin
    scount := Min(srccount, srcmaxsize);
    scount := Min(scount, destmaxsize);

    CanContinue := (scount > 0);
    if (CanContinue) then
    begin
      s := src;
      d := dest;
      x := dest;

      for j := 1 to scount do
      begin
        d^ := s^[0];

        Inc(s);
        Inc(d);
      end;

      // ...
    end;
  end;
end;

end.

