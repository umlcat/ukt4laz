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

unit lazuktfrminputkeytypevalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms,
  Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktonlystringkeytypevaluelists,
  lazuktresinputkeytypevalue,
  dummy;

type

  { Tfrminputkeytypevalue }

  Tfrminputkeytypevalue = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    KeyEdit: TEdit;
    TypeEdit: TEdit;
    KeyLabel: TLabel;
    ValueEdit: TEdit;
    TypeLabel: TLabel;
    sbStatusBar: TStatusBar;
    ValueLabel: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    FIsActivated: Boolean;

    Option:  TMsgDlgButton;

    FKey:    string;
    FTypeID: string;
    FValue:  string;
  end;

var
  frminputkeytypevalue: Tfrminputkeytypevalue;

implementation

{$R *.lfm}

{ Tfrminputkeytypevalue }

procedure Tfrminputkeytypevalue.FormCreate(Sender: TObject);
begin
  FIsActivated := false;
end;

procedure Tfrminputkeytypevalue.btnOKClick(Sender: TObject);
begin
  Option := mbOK;

  FKey    := KeyEdit.Text;
  FTypeID := TypeEdit.Text;
  FValue  := ValueEdit.Text;
end;

procedure Tfrminputkeytypevalue.FormActivate(Sender: TObject);
begin
  if (not FIsActivated) then
  begin
    btnOK.Caption     := ButtonsCaptionsArray[mbOK];
    btnCancel.Caption := ButtonsCaptionsArray[mbCancel];

    KeyEdit.Text   := FKey;
    TypeEdit.Text  := FTypeID;
    ValueEdit.Text := FValue;
  end;
  FIsActivated := true;
end;

procedure Tfrminputkeytypevalue.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure Tfrminputkeytypevalue.FormShow(Sender: TObject);
begin
  KeyLabel.Caption   := resKeyLabel_Caption;
  TypeLabel.Caption  := resTypeLabel_Caption;
  ValueLabel.Caption := resValueLabel_Caption;

  KeyEdit.Text   := FKey;
  TypeEdit.Text  := FTypeID;
  ValueEdit.Text := FValue;
end;

end.

