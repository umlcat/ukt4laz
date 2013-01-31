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

unit uktressrchmngrs;

interface

const
  piSearch   = 0;
  piReplace  = 1;

  riSearchDirectionForward  = 0;
  riSearchDirectionBackward = 1;

  riSearchScopeGlobal   = 0;
  riSearchScopeSelected = 1;

  riSearchOriginCursor = 0;
  riSearchOriginEntire = 1;

{$include 'ukttools_language.inc'}

{$IFDEF ukttools_language_english}
const
  resTsdfrmSearchDialog_Caption  = 'Search Text';
  resTsdfrmReplaceDialog_Caption = 'Search & Replace Text';

  resTestDialog = 'Test Dialog';

  reslblSearchTextToSearch_Caption = 'Text to &search:';

//  restsSearch_Caption = '&Search';

  resgbSearchOptions_Caption = #32'Options'#32;
  reschbSearchCaseSensitive_Caption = 'Case Sensitive';
  reschbSearchWholeWordsOnly_Caption = 'Whole Words Only';
  reschbSearchRegularExpressions_Caption = 'Regular Expressions';
  reschbSearchPromptOnMatch_Caption = 'Prompt &on match';
  reschbReplaceDeleteOnReplace_Caption = 'D&elete on Replace';

  resrgSearchDirection_Caption = #32'Direction'#32;
  resriSearchDirectionForward_Caption = 'Forwar&d';
  resriSearchDirectionBackward_Caption = '&Backward';

  resrgSearchScope_Caption = #32'Scope'#32;
  resriSearchScopeGlobal_Caption = 'Global';
  resriSearchScopeSelected_Caption = 'Selected Text';

  resrgSearchOrigin_Caption = #32'Origin'#32;
  resriSearchOriginCursor_Caption = '&From Cursor';
  resriSearchOriginEntire_Caption = '&Entire Scope';

//  restsReplace_Caption = '&Replace';

  reslblReplaceTextToSearch_Caption = 'Text to &search:';
  reslblReplaceReplaceWith_Caption = 'Replace &with:';

  resbtnOK_Caption = 'OK';
  resbtnALL_Caption = 'Replace &ALL';
  resbtnCancel_Caption = 'Cancel';
  resbtnHelp_Caption = 'Help';

{$ENDIF}

{$IFDEF ukttools_language_spanisheurope}
const
  resTsdfrmSearchDialog_Caption  = 'Buscar Texto';
  resTsdfrmReplaceDialog_Caption = 'Buscar y Reemplazar Texto';

  resTestDialog = 'Prueba Dialogo';

  reslblSearchTextToSearch_Caption = 'Texto que va a bu&scar:';

//  restsSearch_Caption = 'Bu&scar';

  resgbSearchOptions_Caption = #32'Opciones'#32;
  reschbSearchCaseSensitive_Caption = 'Distinguir Mayusculas y Minusculas';
  reschbSearchWholeWordsOnly_Caption = 'Solo Palabras completas';
  reschbSearchRegularExpressions_Caption = 'Expresiones Regulares';
  reschbSearchPromptOnMatch_Caption = 'Preguntar al c&oincidir';
  reschbReplaceDeleteOnReplace_Caption = '&Eliminar al Reemplazar';

  resrgSearchDirection_Caption = #32'Direccion'#32;
  resriSearchDirectionForward_Caption = 'A&delante';
  resriSearchDirectionBackward_Caption = 'Atras';

  resrgSearchScope_Caption = #32'Ambito'#32;
  resriSearchScopeGlobal_Caption = 'Global';
  resriSearchScopeSelected_Caption = 'Texto Seleccionado';

  resrgSearchOrigin_Caption = #32'Origen'#32;
  resriSearchOriginCursor_Caption = '&Desde Cursor';
  resriSearchOriginEntire_Caption = '&Ambito Completo';

//  restsReplace_Caption = '&Reemplazar';

  reslblReplaceTextToSearch_Caption = 'Texto que va a bu&scar:';
  reslblReplaceReplaceWith_Caption = 'Reemplazar &con:';

  resbtnOK_Caption = 'Aceptar';
  resbtnALL_Caption = 'Reemplazar Todo';
  resbtnCancel_Caption = 'Cancelar';
  resbtnHelp_Caption = 'Ayuda';
{$ENDIF}

{$IFDEF ukttools_language_spanishlatam}
const
  resTsdfrmSearchDialog_Caption  = 'Buscar Texto';
  resTsdfrmReplaceDialog_Caption = 'Buscar y Reemplazar Texto';

  resTestDialog = 'Prueba Dialogo';

  reslblSearchTextToSearch_Caption = 'Texto que va a bu&scar:';

//  restsSearch_Caption = 'Bu&scar';

  resgbSearchOptions_Caption = #32'Opciones'#32;
  reschbSearchCaseSensitive_Caption = 'Distinguir Mayusculas y Minusculas';
  reschbSearchWholeWordsOnly_Caption = 'Solo Palabras completas';
  reschbSearchRegularExpressions_Caption = 'Expresiones Regulares';
  reschbSearchPromptOnMatch_Caption = 'Preguntar al c&oincidir';
  reschbReplaceDeleteOnReplace_Caption = '&Eliminar al Reemplazar';

  resrgSearchDirection_Caption = #32'Direccion'#32;
  resriSearchDirectionForward_Caption = 'A&delante';
  resriSearchDirectionBackward_Caption = 'Atras';

  resrgSearchScope_Caption = #32'Ambito'#32;
  resriSearchScopeGlobal_Caption = 'Global';
  resriSearchScopeSelected_Caption = 'Texto Seleccionado';

  resrgSearchOrigin_Caption = #32'Origen'#32;
  resriSearchOriginCursor_Caption = '&Desde Cursor';
  resriSearchOriginEntire_Caption = '&Ambito Completo';

//  restsReplace_Caption = '&Reemplazar';

  reslblReplaceTextToSearch_Caption = 'Texto que va a bu&scar:';
  reslblReplaceReplaceWith_Caption = 'Reemplazar &con:';

  resbtnOK_Caption = 'Aceptar';
  resbtnALL_Caption = 'Reemplazar Todo';
  resbtnCancel_Caption = 'Cancelar';
  resbtnHelp_Caption = 'Ayuda';
{$ENDIF}

{$IFDEF ukttools_language_french}
const
  resTsdfrmSearchDialog_Caption  = 'Search Text';
  resTsdfrmReplaceDialog_Caption = 'Search & Replace Text';

  resTestDialog = 'Test Dialog';

  reslblSearchTextToSearch_Caption = 'Text to &search:';

//  restsSearch_Caption = '&Search';

  resgbSearchOptions_Caption = #32'Options'#32;
  reschbSearchCaseSensitive_Caption = 'Case Sensitive';
  reschbSearchWholeWordsOnly_Caption = 'Whole Words Only';
  reschbSearchRegularExpressions_Caption = 'Regular Expressions';
  reschbSearchPromptOnMatch_Caption = 'Prompt &on match';
  reschbReplaceDeleteOnReplace_Caption = 'D&elete on Replace';

  resrgSearchDirection_Caption = #32'Direction'#32;
  resriSearchDirectionForward_Caption = 'Forwar&d';
  resriSearchDirectionBackward_Caption = '&Backward';

  resrgSearchScope_Caption = #32'Scope'#32;
  resriSearchScopeGlobal_Caption = 'Global';
  resriSearchScopeSelected_Caption = 'Selected Text';

  resrgSearchOrigin_Caption = #32'Origin'#32;
  resriSearchOriginCursor_Caption = '&From Cursor';
  resriSearchOriginEntire_Caption = '&Entire Scope';

//  restsReplace_Caption = '&Replace';

  reslblReplaceTextToSearch_Caption = 'Text to &search:';
  reslblReplaceReplaceWith_Caption = 'Replace &with:';

  resbtnOK_Caption = 'OK';
  resbtnALL_Caption = 'Replace &ALL';
  resbtnCancel_Caption = 'Cancel';
  resbtnHelp_Caption = 'Help';

{$ENDIF}

implementation

end.
