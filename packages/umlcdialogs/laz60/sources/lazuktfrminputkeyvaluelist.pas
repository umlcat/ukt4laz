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

unit lazuktfrminputkeyvaluelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, ComCtrls,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uktmsgdlgtypes,
  uktmsgdlgstrs,
  uktonlystringkeyvaluelists,
  dummy;

type

  { Tuktfrminputkeyvaluelist }

  Tuktfrminputkeyvaluelist = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    lblMessage: TLabel;
    ItemsListBox: TListBox;
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
  uktfrminputkeyvaluelist: Tuktfrminputkeyvaluelist;

implementation

{$IFDEF delphi}
{$R *.dfm}
{$ENDIF}

{$IFDEF FPC}
{$R *.lfm}
{$ENDIF}

{ Tuktfrminputkeyvaluelist }

procedure Tuktfrminputkeyvaluelist.btnOKClick(Sender: TObject);
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

procedure Tuktfrminputkeyvaluelist.FormCreate(Sender: TObject);
begin
  btnOK.Caption     := ButtonsCaptionsArray[mbOK];
  btnCancel.Caption := ButtonsCaptionsArray[mbCancel];
end;

procedure Tuktfrminputkeyvaluelist.FormShow(Sender: TObject);
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

procedure Tuktfrminputkeyvaluelist.btnCancelClick(Sender: TObject);
begin
  Option := mbCancel;
end;

end.

