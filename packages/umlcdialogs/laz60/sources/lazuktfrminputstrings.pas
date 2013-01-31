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

unit lazuktfrminputstrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type

  { TuktfrmInputStrings }

  TuktfrmInputStrings = class(TForm)
    AnswerMemo: TMemo;
    btnCancel: TButton;
    btnOK: TButton;
    lblLabel: TLabel;
    MainStatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    Option:  TMsgDlgButton;
  end;

var
  uktfrmInputStrings: TuktfrmInputStrings;

implementation

{$R *.lfm}

{ TuktfrmInputStrings }

procedure TuktfrmInputStrings.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure TuktfrmInputStrings.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure TuktfrmInputStrings.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure TuktfrmInputStrings.FormShow(Sender: TObject);
begin
  //
end;

end.

