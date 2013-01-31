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

unit uktresbooleans;

interface
uses
  dummy;

{$INCLUDE 'uktlib_language.inc'}

{$IFDEF uktlib_language_english}

{.WARNING "uktresBooleans.pas Enabled"}

const
  reslongFalse = 'FALSE';
  reslongTrue  = 'TRUE';

  resshortFalse = 'NO';
  resshortTrue  = 'YES';
{$ELSE}

{.WARNING "uktresBooleans.pas Disabled"}

{$ENDIF}

{$IFDEF uktlib_language_spanisheurope}
const
  reslongFalse = 'FALSO';
  reslongTrue  = 'VERDADERO';

  resshortFalse = 'NO';
  resshortTrue  = 'SI';
{$ENDIF}

{$IFDEF uktlib_language_spanishlatam}

const
  reslongFalse = 'FALSO';
  reslongTrue  = 'VERDADERO';

  resshortFalse = 'NO';
  resshortTrue  = 'SI';
{$ENDIF}

{$IFDEF uktlib_language_french}
const
  reslongFalse = 'FALSE';
  reslongTrue  = 'VRAI';

  resshortFalse = 'NO';
  resshortTrue  = 'OUI';
{$ENDIF}

implementation

end.
