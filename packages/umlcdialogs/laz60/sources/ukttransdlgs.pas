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

unit ukttransdlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  ComCtrls, Forms,
{$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  dummy;

  function TranslateString
    (const ATitle, ASrcMsg, ADestMsg: string;
     var Answer: string): Boolean;
  function TranslateStrings
    (const ATitle, ASrcMsg, ADestMsg: string;
     var Answer: TStrings): Boolean;

implementation

{$ifdef FPC}
uses
  lazuktfrmtransstr,
  lazuktfrmtransstrings;
{$else}
uses
  vcluktfrmtransstr,
  vcluktfrmtransstrings;
{$endif}

function TranslateString
  (const ATitle, ASrcMsg, ADestMsg: string;
   var Answer: string): Boolean;
begin
  with TuktfrmTransStr.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      lblSource.Caption := ASrcMsg;
      lblDest.Caption := ADestMsg;

      edSource.Text := Answer;
      edDest.Text := Answer;

      ShowModal;
    finally
      Result := (Option = mbOK);

      if (Result)
        then Answer := edDest.Text;
      Free;
    end;
  end;
end;

function TranslateStrings
  (const ATitle, ASrcMsg, ADestMsg: string;
   var Answer: TStrings): Boolean;
begin
  with TuktfrmTransStrings.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;

      lblSource.Caption := ASrcMsg;
      lblDest.Caption := ADestMsg;

      mmSource.Lines.Clear;
      mmSource.Lines.AddStrings(Answer);

      mmDest.Lines.Clear;
      mmDest.Lines.AddStrings(Answer);

      ShowModal;
    finally
      Result := (Option = mbOK);

      if (Result)
        then Answer.Assign(mmDest.Lines);
      Free;
    end;
  end;
end;

end.
