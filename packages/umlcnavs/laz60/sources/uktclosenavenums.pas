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

unit uktclosenavenums;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dummy;

type

  TSDVNavigatorCloseModes =
  (
      (* {0} *)
      None,           // it's not assigned
      (* {1} *)
      OKCancel,       // close container, selecting item
      (* {2} *)
      SelectMany,     // close container, selecting item
      (* {3} *)
      Exit            // close container and don't select item
  );

implementation

end.

