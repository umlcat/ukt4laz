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

unit lazuktfrminputkeytypevaluelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktonlystringkeytypevaluelists,
  dummy;

type

  { Tuktfrminputkeytypevaluelist }

  Tuktfrminputkeytypevaluelist = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    ItemsListBox: TListBox;
    lblMessage: TLabel;
    sbStatusBar: TStatusBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    Option:  TMsgDlgButton;
    Key: string;
  end;

var
  uktfrminputkeytypevaluelist: Tuktfrminputkeytypevaluelist;

implementation

{$R *.lfm}

{ Tuktfrminputkeytypevaluelist }

procedure Tuktfrminputkeytypevaluelist.btnOKClick(Sender: TObject);
var Index: Integer;
begin
  Option := mbOK;
  Index := ItemsListBox.ItemIndex;
  if (Index > -1) then
  begin
    Self.Key := string(TObject(ItemsListBox.Items.Objects[Index]));
  end else
  begin
    Self.Key := '';
  end;
end;

procedure Tuktfrminputkeytypevaluelist.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure Tuktfrminputkeytypevaluelist.FormShow(Sender: TObject);
var Found: Boolean; Index: Integer; EachKey: string;
begin
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
end;

procedure Tuktfrminputkeytypevaluelist.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

end.

