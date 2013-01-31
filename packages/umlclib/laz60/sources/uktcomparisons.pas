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

unit uktcomparisons;

interface

(* TComparison *)

type
  TComparison = Integer;
const
  cmpLower  = -1;
  cmpEqual  = 00;
  cmpHigher = +1;

type

(* TStringOptions *)

  PStringOptions = ^TStringOptions;
  TStringOptions = (soExactMatch, soForceMatch, soForceFirst, soForceLast);
  // used in order to compare 2 strings

  // soExactMatch: strings are case sensitive, must match
  // soForceMatch: a copy of both strings are casted to uppercase,
  // and then compared
  // soForceFirst: a copy of first string will be cast to uppercase,
  // and then compared
  // soForceLast: a copy of last string will be cast to uppercase,
  // and then compared

  // utilizado para comparar 2 cadenas

  // soExactMatch: las cadenas son sensibles al caso, deben coincidir
  // soForceMatch: una copia de ambas cadenas son convertidas a mayusculas,
  // y despues comparadas
  // soForceFirst: una copia de la primera cadena sera convertida a mayusculas
  // y despues comparada
  // soForceLast: una copia de la ultima cadena sera convertida a mayusculas
  // y despues comparada

implementation

end.
