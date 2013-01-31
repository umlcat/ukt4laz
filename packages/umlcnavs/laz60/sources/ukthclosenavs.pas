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

unit ukthclosenavs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktclosenavenums,
  uktfixedbtnpanels,
  uktclosenavs,
  dummy;

type

(* TSDVCustomHorizontalCloseNavigator *)

  TSDVCustomHorizontalCloseNavigator = class(TSDVCustomCloseNavigator)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)

  public
    (* Public declarations *)

  end;

(* TSDVHorizontalCloseNavigator *)

  TSDVHorizontalCloseNavigator = class(TSDVCustomHorizontalCloseNavigator)
  published
    (* Published declarations *)

    (* TSDVCustomCloseNavigator: *)


    (* TSDVCustomHorizontalCloseNavigator: *)

  end;

implementation

end.

