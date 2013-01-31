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

unit ukttextmarkers;

interface

  // special character markers used in a standard operating
  // system independent text file

  // marcadores de caracter especiales utilizados en un archivo de
  // texto estandar independiente del sistema operativo

const
  AppSpaceMarker = #31;
  UserSpaceMarker = #32;
  // a single character that indicates a space
  // un solo caracter que indica un espacio

  UserLineMarker = #10;
  AppLineMarker = #13;
  // a single character that indicates that the current line finishes
  // un solo caracter que indica que la linea actual termina

  UserPageMarker = #11;
  AppPageMarker = #12;
  // a single character that indicates that the current page finishes
  // note: a page marker doesn*t need a line marker

  SpaceMarker = #32;
  // a single character that indicates a space
  // un solo caracter que indica un espacio

  LineMarker = #13;
  // a single character that indicates that the current line finishes
  // un solo caracter que indica que la linea actual termina

  PageMarker = #12;
  // a single character that indicates that the current page finishes
  // note: a page marker doesn*t need a line marker

  // un solo caracter que indica que la pagina actual termina
  // nota: un marcador de pagina no necesita un marcador de linea

  FileMarker = #26;
  // a single character that indicates that the current file finishes
  // note: a file marker doesn*t need a line or page marker

  // un solo caracter que indica que el archivo actual termina
  // nota: un marcador de archivo no necesita un marcador de linea o pagina

  // note: tabulator characters aren*t supported
  // nota: caracteres de tabulacion no son soportados

  LineMarkers = [LineMarker, PageMarker, FileMarker];
  PageMarkers = [PageMarker, FileMarker];

implementation

end.
