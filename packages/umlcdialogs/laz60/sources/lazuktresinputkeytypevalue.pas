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

unit lazuktresinputkeytypevalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

{$include 'uktdialogs_language.inc'}

{$IFDEF uktdialogs_language_english}
resourcestring
  resKeyLabel_Caption   = 'Key:';
  resTypeLabel_Caption  = 'Type:';
  resValueLabel_Caption = 'Value:';
{$ENDIF}

{$IFDEF uktdialogs_language_spanisheurope}
resourcestring
  resKeyLabel_Caption   = 'Llave:';
  resTypeLabel_Caption  = 'Tipo:';
  resValueLabel_Caption = 'Valor:';
{$ENDIF}

{$IFDEF uktdialogs_language_spanishlatam}
resourcestring
  resKeyLabel_Caption   = 'Llave:';
  resTypeLabel_Caption  = 'Tipo:';
  resValueLabel_Caption = 'Valor:';
{$ENDIF}

implementation

end.

