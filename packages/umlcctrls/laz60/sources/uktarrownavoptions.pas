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

unit uktarrownavoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

(* TSDVArrowButtonOption *)

type
  TSDVArrowButtonOption = (* enum of *)
    (
     btnopNone,
     btnopFirst,
     btnopFastPrior,
     btnopPrior,
     btnopSearch,
     btnopMoveToRoot,
     btnopMoveToParent,
     btnopNext,
     btnopFastNext,
     btnopLast
    );
  //TSDVButtonOptions = set of TSDVButtonOption;
  TSDVButtonOptions = set of byte;

const
  foCount = ord(btnopLast) - 1;   // count of TSDVButtonOption-1;

implementation

end.

