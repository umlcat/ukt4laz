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

unit uktsrchtypes;

interface

type

{ TSDVSearchResult }

  TSDVSearchResult = (srrNone, srrCancel, srrSearch, srrReplace, srrReplaceALL);

{ TSDVSearchOptions }

  TSDVSearchOption  = Byte;
  TSDVSearchOptions = set of TSDVSearchOption;

{ TSDVSearchStatus }

  TSDVSearchStatusItem = Byte;
  TSDVSearchStatus = set of TSDVSearchStatusItem;

{ TSDVSearchDirection }

  TSDVSearchDirection = (srdForward, srdBackward);

{ TSDVSearchScope }

  TSDVSearchScope = (srscpGlobal, srscpSelectedText);

{ TSDVSearchFileScope }

  TSDVSearchFileScope = (srscpFilename, srscpFileExt);

{ TSDVSearchOrigin }

  TSDVSearchOrigin = (sropFromCursor, sropEntireScope);

const

{ TSDVSearchOptions }

  sropShowReplaceAll         = 01;
  sropShowHelp               = 02;

  sropShowCaseSensitive      = 03;
  sropShowWholeWordsOnly     = 04;
  sropShowRegularExpressions = 05;

  sropShowPromptOnReplace    = 06;
  sropShowDeleteOnReplace    = 07;
  sropShowKeepCapitalCase    = 08;

{ TSDVSearchStatus }

  srstWantCaseSensitive      = 01;
  srstWantWholeWordsOnly     = 02;
  srstWantRegularExpressions = 03;

  srstWantPromptOnReplace    = 04;
  srstWantDeleteOnReplace    = 05;
  srstWantKeepCapitalCase    = 06;

implementation

end.
