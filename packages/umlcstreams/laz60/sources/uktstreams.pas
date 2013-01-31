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

unit uktstreams;

interface
uses
  SysUtils, Classes,
  uktnormobjects,
  uktcomponents,
  dummy;

const
  sterOK = 00;

type

(* TCustomSDVStream *)

  TCustomSDVStream = class(TSDVNormalizedComponent)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function IsInput(): Boolean; virtual; abstract;
    function IsOutput(): Boolean; virtual; abstract;

    function IsConnected(): Boolean; virtual; abstract;

    function Connect(): Boolean; virtual; abstract;
    function Disconnect(): Boolean; virtual; abstract;
  end;

implementation

(* TCustomSDVStream *)

end.


