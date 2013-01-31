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

unit uktscanneroptions;

interface
uses 
  dummy;

type

(* TSDVScannerOption *)

  TSDVScannerOption  =
  (
  scnopIgnoreTag,
  // el elemento es brincado, el analizador lexico busca el siguiente elemento
  // the element is skipped, the parser looks out for next element

  scnopReturnTag,
  // el elemento es regresado
  // the element is returned

  scnopReturnAsText,
  // el elemento es respaldado y concatenado con los elementos y
  // texto siguiente como texto

  // the element is backup and concatenated with the following elements
  // or text as text

  scnopRemoveTabs
  // los espacios son tratados como texto y tabuladores iniciales
  // the spaces are treated both as text and leading tabulator
  );

implementation

end.
