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

unit uktoptiondlgs;

interface
uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Forms,
{$ENDIF}
  uktmsgdlgtypes,
  dummy;

  function InputOption
    (const ATitle, AMessage: string;
     const Options: array of string;
     var   Answer: Integer): Boolean;

implementation

{$ifdef FPC}
uses
  lazuktfrmoptiondlg;
{$else}
uses
  vcluktfrmoptiondlg;
{$endif}

function InputOption
  (const ATitle, AMessage: string;
   const Options: array of string;
   var   Answer: Integer): Boolean;
var Index: Integer;
begin
  Answer := -1;
  with TfrmOptionDlg.Create(Application) do
  begin
    try
      Option  := mbCancel;
      Caption := ATitle;
      lbMessage.Caption := AMessage;

      with lbOptions.Items do
      begin
        Clear;
        for Index := Low(Options) to High(Options) do
          Add(Options[Index]);
      end;

      if (lbOptions.Items.Count > 0)
        then lbOptions.ItemIndex := Answer;

      ShowModal;
    finally
      Result := (Option = mbOK);

      if Result
        then Answer := lbOptions.ItemIndex;
      Free;
    end;
  end;
end;

end.
