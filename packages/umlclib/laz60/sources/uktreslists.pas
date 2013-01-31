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

unit uktreslists;

interface
uses
  dummy;

{$INCLUDE 'uktlib_language.inc'}

{$IFDEF uktlib_language_english}
resourcestring
  err_ListIndexError = 'List index out of bounds (%d)';
{$ENDIF}

{$IFDEF uktlib_language_spanisheurope}
resourcestring
  err_ListIndexError = 'Indice de Lista fuera de rango (%d)';
{$ENDIF}

{$IFDEF uktlib_language_spanishlatam}
resourcestring
  err_ListIndexError = 'Indice de Lista fuera de rango (%d)';
{$ENDIF}

implementation

end.
