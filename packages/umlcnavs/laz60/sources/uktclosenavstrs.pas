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

unit uktclosenavstrs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

{$include 'sdvctrls_language.inc'}

{$IFDEF sdvctrls_language_english}
resourcestring
  resCloseButtonTypes_None   = 'None';
  resCloseButtonTypes_OK     = 'OK';
  resCloseButtonTypes_Cancel = 'Cancel';
  resCloseButtonTypes_Exit   = 'Exit';
{$ENDIF}

{$IFDEF sdvctrls_language_spanisheurope}
resourcestring
  resCloseButtonTypes_None   = 'Ninguno';
  resCloseButtonTypes_OK     = 'Aceptar';
  resCloseButtonTypes_Cancel = 'Cancelar';
  resCloseButtonTypes_Exit   = 'Salir';
{$ENDIF}

{$IFDEF sdvctrls_language_spanishlatam}
resourcestring
  resCloseButtonTypes_None   = 'Ninguno';
  resCloseButtonTypes_OK     = 'Aceptar';
  resCloseButtonTypes_Cancel = 'Cancelar';
  resCloseButtonTypes_Exit   = 'Salir';
{$ENDIF}

implementation


end.

