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

unit uktconfigs;

(* Espanol *)

  // TConfig. Es un tipo enumerado utilizado para almacenar y recuperar
  // datos de cualquier tipo simple como si fueran cadenas.

  // Se utiliza comunmente en archivos de configuracion.

(* English *)

  // TConfig. It's a enumerated type used for backup & restore any simple type
  // data as strings.

  // It's commonly used in configuration files.

interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc,
{$ENDIF}
  SysUtils, Classes, Math,
//  Graphics, Controls, Forms, Dialogs,
  ukttimes, uktdates, uktBooleans, uktTimeStamps,
  uktresConfigs,
  dummy;

const
  strcfgNone      = 'cfgNone';
  strcfgCustom    = 'cfgCustom';
  strcfgPointer   = 'cfgPointer';
  strcfgBoolean   = 'cfgBoolean';
  strcfgInteger   = 'cfgInteger';
  strcfgFloat     = 'cfgFloat';
  strcfgCurrency  = 'cfgCurrency';
  strcfgString    = 'cfgString';
  strcfgDate      = 'cfgDate';
  strcfgTime      = 'cfgTime';
  strcfgDateTime  = 'cfgDateTime';
  strcfgColor     = 'cfgColor';
  strcfgFile      = 'cfgFile';
  strcfgPath      = 'cfgPath';
  strcfgChar      = 'cfgChar';
  strcfgArray     = 'cfgArray';

const
  cfgCurrencyFormat  = '$';
  cfgDateFormat      = 'dd/MM/aaaa';
  cfgTimeFormat      = 'hh:mm:ss';
  cfgDateTimeFormat  = 'hh:mm:ss dd/MM/aaaa';
  cfgTimeStampFormat = 'hh:mm:ss tt dd/MM/aaaa';
  // default formats for date & time values
  // formatos default para valores fecha y hora

type

{ TConfig }

  TConfig =
  ( cfgNone,     cfgCustom,   cfgPointer, cfgBoolean, cfgInteger,
    cfgFloat,    cfgCurrency, cfgString,  cfgDate,    cfgTime,
    cfgDateTime, cfgColor,    cfgFile,    cfgPath,    cfgChar,
    cfgArray );

  function SafeConfig(const Value: TConfig): TConfig;

  function ConfigToInt(const Value: TConfig): Integer;
  function IntToConfig(const Value: Integer): TConfig;

  function StrToConfig(const Value: string): TConfig;
  function TextToConfig(const Value: string): TConfig;

  function ConfigToStr(const Value: TConfig): string;
  function ConfigToText(const Value: TConfig): string;

implementation

const
   IntToConfigArray: array[Ord(Low(TConfig))..Ord(High(TConfig))] of TConfig =
  ( cfgNone,     cfgCustom,   cfgPointer, cfgBoolean, cfgInteger,
    cfgFloat,    cfgCurrency, cfgString,  cfgDate,    cfgTime,
    cfgDateTime, cfgColor,    cfgFile,    cfgPath,    cfgChar,
    cfgArray
  );

type
  TConfigNames = array[TConfig] of string;
  PConfigNames = ^TConfigNames;

const
   ConfigToStrArray: TConfigNames =
  ( strcfgNone,     strcfgCustom,   strcfgPointer, strcfgBoolean, strcfgInteger,
    strcfgFloat,    strcfgCurrency, strcfgString,  strcfgDate,    strcfgTime,
    strcfgDateTime, strcfgColor,    strcfgFile,    strcfgPath,    strcfgChar,
    strcfgArray );

   ConfigToTextArray: TConfigNames =
  ( textcfgNone,     textcfgCustom,   textcfgPointer, textcfgBoolean, textcfgInteger,
    textcfgFloat,    textcfgCurrency, textcfgString,  textcfgDate,    textcfgTime,
    textcfgDateTime, textcfgColor,    textcfgFile,    textcfgPath,    textcfgChar,
    textcfgArray );

function SafeConfig(const Value: TConfig): TConfig;
begin
  Result := Value;
  if Result < Low(TConfig)
    then Result := Low(TConfig);
  if Result > High(TConfig)
    then Result := High(TConfig);
  // Goal: Checks that a Config number is between 0 and 7.
  // Objetivo: Revisa que el numero de dia este entre 0 y 7.
end;

function ConfigToInt(const Value: TConfig): Integer;
begin
  Result := Ord(Value);
end;

function IntToConfig(const Value: Integer): TConfig;
begin
  Result := IntToConfigArray[Value];
end;

function MatchConfig
  (const Value: string; const ConfigNames: TConfigNames): TConfig;
var i: TConfig; Found: Boolean;
begin
  i := Low(TConfig); Found := FALSE;
  while (i <= High(TConfig)) and (not Found) do
  begin
    Found := SameText(ConfigNames[i], Value);
    Inc(i);
  end;

  if Found
    then Result := Pred(i)
    else Result := Low(TConfig);
  // Goal: Locates a configuration by its name in a given array.
end;

function StrToConfig(const Value: string): TConfig;
begin
  Result := MatchConfig(Value, ConfigToStrArray);
  // Goal: To cast a "string" value to a "config" value.
  // Objetivo: Convertir un valor "string" a un valor "config".
end;

function TextToConfig(const Value: string): TConfig;
begin
  Result := MatchConfig(Value, ConfigToTextArray);
  // Goal: To cast a "string" value to a "config" value.
  // Objetivo: Convertir un valor "string" a un valor "config".
end;

function ConfigToStr(const Value: TConfig): string;
begin
  Result := ConfigToStrArray[Value];
  // Goal: To cast a "config" value to a "string".
  // Objetivo: Convertir un valor "config" a un valor "string".
end;

function ConfigToText(const Value: TConfig): string;
begin
  Result := ConfigToTextArray[Value];
  // Goal: To cast a "config" value to a "string".
  // Objetivo: Convertir un valor "config" a un valor "string".
end;

end.
