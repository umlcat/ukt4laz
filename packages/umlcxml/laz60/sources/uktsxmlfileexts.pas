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

unit uktsxmlfileexts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

const
  extALL   = '*';
  extText  = 'txt';
  extXML   = 'xml';
  extSXML  = 'sxml';
  extTemplate_SXML  = 'sxtpl';

  wldALL   = '*.*';
  wldText  = '*.txt';
  wldXML   = '*.xml';
  wldSXML  = '*.sxml';
  wldTemplate_SXML  = '*.sxtpl';

{$INCLUDE 'runuktxml60_language.inc'}

{$IFDEF uktxml_language_english}
resourcestring

  fltALL  = 'All Files';
  fltText = 'Text Files';
  fltXML  = 'X.M.L. Documents';
  fltSXML = 'Simple X.M.L. Documents';
  fltTemplate_SXML  = 'Simple X.M.L. Templates';
{$ENDIF}

{$IFDEF uktxml_language_spanisheurope}
resourcestring

  fltALL  = 'Todos los Ficheros';
  fltText = 'Ficheros de Texto';
  fltXML  = 'Documentos X.M.L.';
  fltSXML = 'Documentos Simple X.M.L.';
  fltTemplate_SXML  = 'Plantillas para Simple X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_spanishlatam}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
  fltSXML = 'Documentos Simple X.M.L.';
  fltTemplate_SXML  = 'Plantillas para Simple X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_french}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
  fltSXML = 'Documentos Simple X.M.L.';
  fltTemplate_SXML  = 'Plantillas para Simple X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_portuguese}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
  fltSXML = 'Documentos Simple X.M.L.';
  fltTemplate_SXML  = 'Plantillas para Simple X.M.L.';
{$ENDIF}

{$IFDEF uktxml_language_german}
resourcestring

  fltALL  = 'Todos los Archivos';
  fltText = 'Archivos de Texto';
  fltXML  = 'Documentos X.M.L.';
  fltSXML = 'Documentos Simple X.M.L.';
  fltTemplate_SXML  = 'Plantillas para Simple X.M.L.';
{$ENDIF}


implementation

end.

