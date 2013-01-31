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

unit uktarrownavstrs;

interface

{$include 'uktctrls_language.inc'}

{$IFDEF uktctrls_language_english}
resourcestring
  resbtnopNone_Caption      = 'NONE';
  resbtnopFirst_Caption     = 'FIRST';
  resbtnopFastPrior_Caption = 'FastPrior';
  resbtnopPrior_Caption     = 'PRIOR';
  resbtnopSearch_Caption    = 'SEARCH';
  resbtnopRoot_Caption      = 'ROOT';
  resbtnopParent_Caption    = 'PARENT';
  resbtnopNext_Caption      = 'NEXT';
  resbtnopFastNext_Caption  = 'FastNext';
  resbtnopLast_Caption      = 'LAST';

  resbtnopNone_Hint      = 'Nothing';
  resbtnopFirst_Hint     = 'Return to first record';
  resbtnopPrior_Hint     = 'Return to previous record';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopSearch_Hint    = 'Search record';
  resbtnopRoot_Hint      = 'Return to root item';
  resbtnopParent_Hint    = 'Return to parent item';
  resbtnopNext_Hint      = 'Avanzar to next record';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar to last record';
{$ENDIF}

{$IFDEF uktctrls_language_spanisheurope}
resourcestring
  resbtnopNone_Caption      = 'Ninguno';
  resbtnopFirst_Caption     = 'Primero';
  resbtnopFastPrior_Caption = 'Regresar';
  resbtnopPrior_Caption     = 'Anterior';
  resbtnopSearch_Caption    = 'Buscar';
  resbtnopRoot_Caption      = 'Raiz';
  resbtnopParent_Caption    = 'Padre';
  resbtnopNext_Caption      = 'Siguiente';
  resbtnopFastNext_Caption  = 'Avanzar';
  resbtnopLast_Caption      = 'Ultimo';

  resbtnopNone_Hint      = 'Nada';
  resbtnopFirst_Hint     = 'Regresar al primer registro';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopPrior_Hint     = 'Regresar al registro anterior';
  resbtnopSearch_Hint    = 'Buscar registro';
  resbtnopRoot_Hint      = 'Regresar al registro nivel raiz';
  resbtnopParent_Hint    = 'Regresar al registro nivel superior';
  resbtnopNext_Hint      = 'Avanzar al siguiente registro';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar al ultimo registro';
{$ENDIF}

{$IFDEF uktctrls_language_spanishlatam}
resourcestring
  resbtnopNone_Caption      = 'Ninguno';
  resbtnopFirst_Caption     = 'Primero';
  resbtnopFastPrior_Caption = 'Regresar';
  resbtnopPrior_Caption     = 'Anterior';
  resbtnopSearch_Caption    = 'Buscar';
  resbtnopRoot_Caption      = 'Raiz';
  resbtnopParent_Caption    = 'Padre';
  resbtnopNext_Caption      = 'Siguiente';
  resbtnopFastNext_Caption  = 'Avanzar';
  resbtnopLast_Caption      = 'Ultimo';

  resbtnopNone_Hint      = 'Nada';
  resbtnopFirst_Hint     = 'Regresar al primer registro';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopPrior_Hint     = 'Regresar al registro anterior';
  resbtnopSearch_Hint    = 'Buscar registro';
  resbtnopRoot_Hint      = 'Regresar al registro nivel raiz';
  resbtnopParent_Hint    = 'Regresar al registro nivel superior';
  resbtnopNext_Hint      = 'Avanzar al siguiente registro';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar al ultimo registro';
{$ENDIF}

{$IFDEF uktctrls_language_french}
resourcestring
  resbtnopNone_Caption      = 'NONE';
  resbtnopFirst_Caption     = 'PRIMERO';
  resbtnopFastPrior_Caption = 'FastPrior';
  resbtnopPrior_Caption     = 'PREVIO';
  resbtnopSearch_Caption    = 'BUSCAR';
  resbtnopRoot_Caption      = 'RAIZ';
  resbtnopParent_Caption    = 'PADRE';
  resbtnopNext_Caption      = 'SIGUIENTE';
  resbtnopFastNext_Caption  = 'SIGUIENTE';
  resbtnopLast_Caption      = 'ULTIMO';

  resbtnopNone_Hint      = 'Nada';
  resbtnopFirst_Hint     = 'Regresar al primer registro';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopPrior_Hint     = 'Regresar al registro anterior';
  resbtnopSearch_Hint    = 'Buscar registro';
  resbtnopRoot_Hint      = 'Regresar al registro nivel raiz';
  resbtnopParent_Hint    = 'Regresar al registro nivel superior';
  resbtnopNext_Hint      = 'Avanzar al siguiente registro';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar al ultimo registro';
{$ENDIF}

{$IFDEF uktctrls_language_german}
resourcestring
  resbtnopNone_Caption      = 'NONE';
  resbtnopFirst_Caption     = 'PRIMERO';
  resbtnopFastPrior_Caption = 'FastPrior';
  resbtnopPrior_Caption     = 'PREVIO';
  resbtnopSearch_Caption    = 'BUSCAR';
  resbtnopRoot_Caption      = 'RAIZ';
  resbtnopParent_Caption    = 'PADRE';
  resbtnopNext_Caption      = 'SIGUIENTE';
  resbtnopFastNext_Caption  = 'SIGUIENTE';
  resbtnopLast_Caption      = 'ULTIMO';

  resbtnopNone_Hint      = 'Nada';
  resbtnopFirst_Hint     = 'Regresar al primer registro';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopPrior_Hint     = 'Regresar al registro anterior';
  resbtnopSearch_Hint    = 'Buscar registro';
  resbtnopRoot_Hint      = 'Regresar al registro nivel raiz';
  resbtnopParent_Hint    = 'Regresar al registro nivel superior';
  resbtnopNext_Hint      = 'Avanzar al siguiente registro';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar al ultimo registro';
{$ENDIF}

{$IFDEF uktctrls_language_portuguese}
resourcestring
  resbtnopNone_Caption      = 'NONE';
  resbtnopFirst_Caption     = 'PRIMERO';
  resbtnopFastPrior_Caption = 'FastPrior';
  resbtnopPrior_Caption     = 'PREVIO';
  resbtnopSearch_Caption    = 'BUSCAR';
  resbtnopRoot_Caption      = 'RAIZ';
  resbtnopParent_Caption    = 'PADRE';
  resbtnopNext_Caption      = 'SIGUIENTE';
  resbtnopFastNext_Caption  = 'SIGUIENTE';
  resbtnopLast_Caption      = 'ULTIMO';

  resbtnopNone_Hint      = 'Nada';
  resbtnopFirst_Hint     = 'Regresar al primer registro';
  resbtnopFastPrior_Hint = 'Return to previous record';
  resbtnopPrior_Hint     = 'Regresar al registro anterior';
  resbtnopSearch_Hint    = 'Buscar registro';
  resbtnopRoot_Hint      = 'Regresar al registro nivel raiz';
  resbtnopParent_Hint    = 'Regresar al registro nivel superior';
  resbtnopNext_Hint      = 'Avanzar al siguiente registro';
  resbtnopFastNext_Hint  = 'Avanzar to FastNext record';
  resbtnopLast_Hint      = 'Avanzar al ultimo registro';
{$ENDIF}

implementation

end.
