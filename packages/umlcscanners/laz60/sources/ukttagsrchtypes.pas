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

unit uktTagSrchTypes;

interface
uses 
  uktSrchTypes,
  dummy;

const

(* TSearchOptions *)

(*
  sropShowReplaceAll         = 01;
  sropShowHelp               = 02;

  sropShowCaseSensitive      = 03;
  sropShowWholeWordsOnly     = 04;
  sropShowRegularExpressions = 05;

  sropShowPromptOnReplace    = 06;
  sropShowDeleteOnReplace    = 07;
  sropShowKeepCapitalCase    = 08;
*)

  sropShowReplaceTagKeyword  = 09;
  sropShowReplacePropKeyword = 10;
  sropShowReplacePropValue   = 11;

(* TSearchStatus *)

(*
  srstWantCaseSensitive      = 01;
  srstWantWholeWordsOnly     = 02;
  srstWantRegularExpressions = 03;

  srstWantPromptOnReplace    = 04;
  srstWantDeleteOnReplace    = 05;
  srstWantKeepCapitalCase    = 06;
*)

  srstWantReplaceTagKeyword  = 07;
  srstWantReplacePropKeyword = 08;
  srstWantReplacePropValue   = 09;

implementation

end.
