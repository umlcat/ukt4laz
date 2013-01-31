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

unit uktTagScanners;

interface
uses
  SysUtils, Classes,
  uktStreams,
  uktScannerStates, uktScanners,
  dummy;

type

(* TCustomSDVTagScanner *)

  TCustomSDVTagScanner = class(TCustomSDVScanner)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    constructor Create(AOwner: TComponent); override;
  end;

implementation

(* TCustomSDVTagScanner *)

constructor TCustomSDVTagScanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FInternalOptions := tagopDefaultOptions;
end;

end.
