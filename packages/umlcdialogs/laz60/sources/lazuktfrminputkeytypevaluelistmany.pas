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

unit lazuktfrminputkeytypevaluelistmany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, CheckLst,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktonlystringkeytypevaluelists,
  dummy;

type

  { Tuktfrminputkeytypevaluelistmany }

  Tuktfrminputkeytypevaluelistmany = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ItemsCheckListBox: TCheckListBox;
    lblMessage: TLabel;
    sbStatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    Option:  TMsgDlgButton;
    SelItems: TStringList;
  end;

var
  uktfrminputkeytypevaluelistmany: Tuktfrminputkeytypevaluelistmany;

implementation

{$R *.lfm}

{ Tuktfrminputkeytypevaluelistmany }

procedure Tuktfrminputkeytypevaluelistmany.FormCreate(Sender: TObject);
begin
  SelItems := TStringList.Create();

  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure Tuktfrminputkeytypevaluelistmany.btnOKClick(Sender: TObject);
var EachIndex, LastIndex: Integer;
  IsChecked: Boolean; AKey: string;
begin
  Option := mbOK;

  SelItems.Clear();

  LastIndex := (ItemsCheckListBox.Count - 1);
  for EachIndex := 0 to LastIndex do
  begin
    IsChecked := ItemsCheckListBox.Checked[EachIndex];
    if (IsChecked) then
    begin
      AKey := string(TObject(ItemsCheckListBox.Items.Objects[EachIndex]));
      SelItems.Add(AKey);
    end;
  end;
end;

procedure Tuktfrminputkeytypevaluelistmany.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

procedure Tuktfrminputkeytypevaluelistmany.FormDestroy(Sender: TObject);
begin
  SelItems.Free();
end;

procedure Tuktfrminputkeytypevaluelistmany.FormShow(Sender: TObject);
//var Found: Boolean; Index: Integer; EachKey: string;
begin
  (*
  Found := false;
  Index := 0;
  while ((Index < ItemsListBox.Items.Count) and (not Found)) do
  begin
    EachKey := string(TObject(ItemsListBox.Items.Objects[Index]));
    Found   := (EachKey = Self.Key);
    Inc(Index);
  end;

  if (Found) then
  begin
    ItemsListBox.Selected[(Index - 1)] := true;
  end;
  *)
end;

end.

