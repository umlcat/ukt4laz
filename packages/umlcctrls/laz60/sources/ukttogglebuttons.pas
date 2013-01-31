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

unit ukttogglebuttons;

interface
uses
(*.IFDEF MSWINDOWS*)
  //Windows,
  //Messages,
  Graphics,
  Controls, 
  //StdCtrls,
  Forms,
  Buttons,
(*.ENDIF*)
  SysUtils, 
  Classes,
  dummy;

type

(* ISDVToggledButton *)

  ISDVToggledButton = interface
    function getCaptionToggled: string;
    function getCaptionUnToggled: string;

    function getGlyphToggled: TBitmap;
    function getGlyphUnToggled: TBitmap;

    function getToggled: Boolean;

    procedure setCaptionToggled(const Value: string);
    procedure setCaptionUnToggled(const Value: string);

    procedure setGlyphToggled(const Value: TBitmap);
    procedure setGlyphUnToggled(const Value: TBitmap);

    procedure setToggled(const Value: Boolean);

    procedure RefreshButton;

    procedure Activated;
    procedure Click;

    property Toggled: Boolean
      read getToggled write setToggled;

    property GlyphToggled: TBitmap
      read getGlyphToggled write setGlyphToggled;
    property GlyphUnToggled: TBitmap
      read getGlyphUnToggled write setGlyphUnToggled;

    property CaptionToggled: string
      read getCaptionToggled write setCaptionToggled;
    property CaptionUnToggled: string
      read getCaptionUnToggled write setCaptionUnToggled;
  end;

implementation

end.
