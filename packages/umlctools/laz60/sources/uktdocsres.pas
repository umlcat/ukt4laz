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

unit uktdocsres;

interface

{$INCLUDE 'ukttools_language.inc'}

{$IFDEF ukttools_language_english}
const
  resNoName   = 'NoName';

  resFileSave = 'File "%s" not saved, save now ?';
  resFileOverwrite  = 'File "%s" already exists, overwite ?';
{$ENDIF}

{$IFDEF ukttools_language_spanisheurope}
const
  resNoName   = 'SinNombre';
  resFileSave = 'El Fichero "%s" no esta guardado, guardar ahora ?';
  resFileOverwrite  = 'El Fichero "%s" ya existe, reescribirlo ?';
{$ENDIF}

{$IFDEF ukttools_language_spanishlatam}
const
  resNoName   = 'SinNombre';
  resFileSave = 'El archivo "%s" no esta guardado, guardar ahora ?';
  resFileOverwrite  = 'El archivo "%s" ya existe, reescribirlo ?';
{$ENDIF}

{$IFDEF ukttools_language_french}
const
  resNoName   = 'Anonyme';
  resFileSave = 'File "%s" not saved, save now ?';
  resFileOverwrite  = 'File "%s" already exists, overwite ?';
{$ENDIF}

{$IFDEF ukttools_language_german}
const
  resNoName   = 'Anonyme';
  resFileSave = 'File "%s" not saved, save now ?';
  resFileOverwrite  = 'File "%s" already exists, overwite ?';
{$ENDIF}

{$IFDEF ukttools_language_portuguese}
const
  resNoName   = 'Anonyme';
  resFileSave = 'File "%s" not saved, save now ?';
  resFileOverwrite  = 'File "%s" already exists, overwite ?';  
{$ENDIF}

implementation

end.
