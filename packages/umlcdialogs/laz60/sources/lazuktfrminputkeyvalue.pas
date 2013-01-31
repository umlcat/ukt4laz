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

unit lazuktfrminputkeyvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  {$IFDEF FPC}
  LResources, StdCtrls,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktonlystringkeyvaluelists,
  dummy;

resourcestring
  resKeyLabel_Caption   = 'Key:';
  resValueLabel_Caption = 'Value:';

type

  { Tfrminputkeyvalue }

  Tfrminputkeyvalue = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    KeyEdit: TEdit;
    KeyLabel: TLabel;
    sbStatusBar: TStatusBar;
    ValueEdit: TEdit;
    ValueLabel: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    Option:  TMsgDlgButton;

    FKey:    string;
    FValue:  string;
  end;

var
  frminputkeyvalue: Tfrminputkeyvalue;

implementation

{$R *.lfm}

{ Tfrminputkeyvalue }

procedure Tfrminputkeyvalue.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure Tfrminputkeyvalue.btnOKClick(Sender: TObject);
begin
  Option := mbOK;

  FKey    := KeyEdit.Text;
  FValue  := ValueEdit.Text;
end;

procedure Tfrminputkeyvalue.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure Tfrminputkeyvalue.FormShow(Sender: TObject);
begin
  KeyLabel.Caption   := resKeyLabel_Caption;
  ValueLabel.Caption := resValueLabel_Caption;

  KeyEdit.Text   := FKey;
  ValueEdit.Text := FValue;
end;

end.

