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

unit uktresdays;

interface
uses
  dummy;

{$INCLUDE 'uktlib_language.inc'}

(*
resourcestring
  shortdyNone      = 'Non';
  shortdySunday    = 'Sun';
  shortdyMonday    = 'Mon';
  shortdyTuesday   = 'Tue';
  shortdyWednesday = 'Wed';
  shortdyThursday  = 'Thu';
  shortdyFriday    = 'Fri';
  shortdySaturday  = 'Sat';

  longdyNone      = 'None';
  longdySunday    = 'Sunday';
  longdyMonday    = 'Monday';
  longdyTuesday   = 'Tuesday';
  longdyWednesday = 'Wednesday';
  longdyThursday  = 'Thursday';
  longdyFriday    = 'Friday';
  longdySaturday  = 'Saturday';
*)

{$IFDEF uktlib_language_english}
resourcestring
  shortdyNone      = 'Non';
  shortdySunday    = 'Sun';
  shortdyMonday    = 'Mon';
  shortdyTuesday   = 'Tue';
  shortdyWednesday = 'Wed';
  shortdyThursday  = 'Thu';
  shortdyFriday    = 'Fri';
  shortdySaturday  = 'Sat';

  longdyNone      = 'None';
  longdySunday    = 'Sunday';
  longdyMonday    = 'Monday';
  longdyTuesday   = 'Tuesday';
  longdyWednesday = 'Wednesday';
  longdyThursday  = 'Thursday';
  longdyFriday    = 'Friday';
  longdySaturday  = 'Saturday';
{$ENDIF}

{$IFDEF uktlib_language_spanisheurope}
resourcestring
  shortdyNone      = 'Nin';
  shortdySunday    = 'Dom';
  shortdyMonday    = 'Lun';
  shortdyTuesday   = 'Mar';
  shortdyWednesday = 'Mie';
  shortdyThursday  = 'Jue';
  shortdyFriday    = 'Vie';
  shortdySaturday  = 'Sab';

  longdyNone      = 'Ninguno';
  longdySunday    = 'Domingo';
  longdyMonday    = 'Lunes';
  longdyTuesday   = 'Martes';
  longdyWednesday = 'Miercoles';
  longdyThursday  = 'Jueves';
  longdyFriday    = 'Viernes';
  longdySaturday  = 'Sabado';
{$ENDIF}

{$IFDEF uktlib_language_spanishlatam}
resourcestring
  shortdyNone      = 'Nin';
  shortdySunday    = 'Dom';
  shortdyMonday    = 'Lun';
  shortdyTuesday   = 'Mar';
  shortdyWednesday = 'Mie';
  shortdyThursday  = 'Jue';
  shortdyFriday    = 'Vie';
  shortdySaturday  = 'Sab';
  
  longdyNone      = 'Ninguno';
  longdySunday    = 'Domingo';
  longdyMonday    = 'Lunes';
  longdyTuesday   = 'Martes';
  longdyWednesday = 'Miercoles';
  longdyThursday  = 'Jueves';
  longdyFriday    = 'Viernes';
  longdySaturday  = 'Sabado';
{$ENDIF}

{$IFDEF uktlib_language_french}
resourcestring
  shortdyNone      = 'Ninguno';
  shortdySunday    = 'Dimanche';
  shortdyMonday    = 'Lundi';
  shortdyTuesday   = 'Mardi';
  shortdyWednesday = 'Mercredi';
  shortdyThursday  = 'Jeudi';
  shortdyFriday    = 'Vendredi';
  shortdySaturday  = 'Somedi';

  longdyNone      = 'Ninguno';
  longdySunday    = 'Dimanche';
  longdyMonday    = 'Lundi';
  longdyTuesday   = 'Mardi';
  longdyWednesday = 'Mercredi';
  longdyThursday  = 'Jeudi';
  longdyFriday    = 'Vendredi';
  longdySaturday  = 'Somedi';
{$ENDIF}

{$IFDEF uktlib_language_italian}
resourcestring
  shortdyNone      = 'Nessuno';
  shortdySunday    = 'Domenica';
  shortdyMonday    = 'Lunedi';
  shortdyTuesday   = 'Martedi';
  shortdyWednesday = 'Mercoledi';
  shortdyThursday  = 'Giovedi';
  shortdyFriday    = 'Venerdi';
  shortdySaturday  = 'Sabato';

  longdyNone      = 'Nessuno';
  longdySunday    = 'Domenica';
  longdyMonday    = 'Lunedi';
  longdyTuesday   = 'Martedi';
  longdyWednesday = 'Mercoledi';
  longdyThursday  = 'Giovedi';
  longdyFriday    = 'Venerdi';
  longdySaturday  = 'Sabato';
{$ENDIF}

implementation

end.
