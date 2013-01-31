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

unit lazuktfrmtransstrings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktmsgdlgarrays,
  dummy;

type
  TuktfrmTransStrings = class(TForm)
    lblSource: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    mmSource: TMemo;
    sbStatusBar: TStatusBar;
    mmDest: TMemo;
    lblDest: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

   Option:  TMsgDlgButton;
  end;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

procedure TuktfrmTransStrings.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure TuktfrmTransStrings.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmTransStrings.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

initialization
  {$IFDEF FPC}
  {$I 'lazuktfrmtransstrings.lrs'}
  {$ENDIF}

end.
