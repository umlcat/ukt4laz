(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktxmlfileexts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

const
  extALL   = '*';
  extText  = 'txt';
  extXML   = 'xml';

  wldALL   = '*.*';
  wldText  = '*.txt';
  wldXML   = '*.xml';

{$INCLUDE 'runuktxml60_language.inc'}

{$IFDEF uktxml_language_english}
resourcestring

  fltALL  = 'All Files';
  fltText = 'Text Files';
  fltXML  = 'X.M.L. Documents';
{$ENDIF}

{$IFDEF uktxml_language_spanisheurope}
resourcestring

  fltALL  = 'Todos los Ficheros';
  fltText = 'Ficheros de Texto';
  fltXML  = 'Documentos X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_spanishlatam}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_french}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_portuguese}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_german}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
{$ENDIF}

implementation

end.

