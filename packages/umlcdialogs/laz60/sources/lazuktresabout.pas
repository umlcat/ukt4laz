unit lazuktresabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

{$include 'uktdialogs_language.inc'}

{$IFDEF uktdialogs_language_english}
resourcestring
  resExitButton_Caption = 'Exit';
  resSummaryTabSheet_Caption = 'Summary';
  resCopyrightTabSheet_Caption = 'Copyright';
{$ENDIF}

{$IFDEF uktdialogs_language_spanisheurope}
resourcestring
  resExitButton_Caption = 'Salir';
  resSummaryTabSheet_Caption = 'Descripcion';
  resAuthorsTabSheet_Caption = 'Copyright';
{$ENDIF}

{$IFDEF uktdialogs_language_spanishlatam}
resourcestring
  resExitButton_Caption = 'Salir';
  resSummaryTabSheet_Caption = 'Descripcion';
  resAuthorsTabSheet_Caption = 'Copyright';
{$ENDIF}

implementation

end.

