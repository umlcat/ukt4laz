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

unit uktmsgdlgarrays;

interface
uses
  Classes,
{$IFDEF MSWINDOWS}
  StdCtrls, ExtCtrls,
{$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type
  TButtonsArray = array[TMsgDlgButton] of TButton;
  TIncButtonsArray = array[Ord(mbYes)..Ord(mbIgnore)] of TButton;
  TImagesArray  = array[TMsgDlgType] of TImage;

implementation

end.

