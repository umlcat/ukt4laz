(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the UMLCat's Component Library.                  *
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

unit uktscannerstates;

interface
uses
  dummy;

(* TSDVState *)

type
  TSDVState = Integer;

const
  stStart:  TSDVState = 01;
  stFinish: TSDVState = 00;

  stErrorUnexpectedEoF:    TSDVState = -1;
  stErrorUnexpectedEoLn:   TSDVState = -2;
  stErrorUnexpectedChar:   TSDVState = -3;
  stErrorUnexpectedStart:  TSDVState = -4;
  stErrorUnexpectedFinish: TSDVState = -5;

implementation

end.

