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

unit lazuktfrminputkeyvalueoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  dummy;

type

  { Tfrminputkeyvalueoptions }

  Tfrminputkeyvalueoptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    rgOptions: TRadioGroup;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    Option:  TMsgDlgButton;
  end;

var
  frminputkeyvalueoptions: Tfrminputkeyvalueoptions;

implementation

{$R *.lfm}

{ Tfrminputkeyvalueoptions }

procedure Tfrminputkeyvalueoptions.btnOKClick(Sender: TObject);
begin
  Option := mbOK;
end;

procedure Tfrminputkeyvalueoptions.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure Tfrminputkeyvalueoptions.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

end.

