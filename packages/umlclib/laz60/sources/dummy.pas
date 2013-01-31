(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the UMLCat Developer's Component Library.        *
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

unit dummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(**
 ** This unit doesn't do anything.
 ** Helps avoid to check for the semicolon
 ** in "uses" section of ther files.
 ** Example:
 **
 ** uses
 **  Windows, Messages;
 **  Classes, SysUtils;
 **  //MyControls,
 **  dummy;
 **)

implementation

end.

