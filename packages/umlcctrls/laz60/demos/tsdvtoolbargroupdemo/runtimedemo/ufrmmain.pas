unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons,
  sdvtoolbars,
  dummy;

type

  { Tfrmmain }

  Tfrmmain = class(TForm)
    ExitButton: TButton;
    BottomPanel: TPanel;
    MainImageList: TImageList;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  protected
    { protected declarations }

    FBeenActivated: Boolean;

    ToolbarGroup: TSDVToolBarGroup;

    procedure FormActivateFirstTime(Sender: TObject);
    procedure FormActivateAgain(Sender: TObject);
  public
    { public declarations }
  end;

var
  frmmain: Tfrmmain;

implementation

{$R *.lfm}

{ Tfrmmain }

procedure Tfrmmain.FormCreate(Sender: TObject);
begin
  FBeenActivated := false;
end;

procedure Tfrmmain.FormActivateFirstTime(Sender: TObject);
var AToolBarRow: TSDVToolBarRow;
    AToolBar: TSDVToolBar;
    AToolButton: TSDVToolButton;
    ABitmap: TCustomBitmap;
    ACount: Integer;
begin
  ToolbarGroup := TSDVToolBarGroup.Create(Self);
  ToolbarGroup.Top := 0;
  ToolbarGroup.Left := 0;
  ToolbarGroup.Align := TAlign.alTop;

  Self.InsertControl(ToolbarGroup);
  ToolbarGroup.Activated := true;

  AToolBarRow := ToolbarGroup.ToolBarRowAt(0);

  AToolBar := AToolBarRow.InsertToolBar();
  AToolBar.Name := 'File';

  ABitmap := TCustomBitmap.Create();
  ACount := MainImageList.Count;
  if (ACount > 0) then
  begin
    MainImageList.GetBitmap(1, ABitmap);
  end;

  AToolButton := AToolBar.InsertToolButton();
  AToolButton.Caption := 'Hello';
  //AToolButton.Glyph.;
end;

procedure Tfrmmain.FormActivateAgain(Sender: TObject);
begin
  //
end;

procedure Tfrmmain.FormActivate(Sender: TObject);
begin
  if (FBeenActivated) then
  begin
    FormActivateAgain(Sender);
  end else
  begin
    FormActivateFirstTime(Sender);
  end;
  FBeenActivated := true;
end;

procedure Tfrmmain.ExitButtonClick(Sender: TObject);
begin
  Self.Close();
end;

end.

