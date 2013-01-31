unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls,
  sdvarrownavs,
  dummy;

type

  { Tfrmmain }

  Tfrmmain = class(TForm)
    ExitButton: TButton;
    MainStatusBar: TStatusBar;
    TopPanel: TPanel;
    ArrowNavigatorPanel: TPanel;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  protected
    { protected declarations }

    ArrowNavigator: TCustomSDVArrowNavigator;
  public
    { public declarations }

    FBeenActivated: Boolean;

    procedure FormActivateFirst(Sender: TObject);
    procedure FormActivateAgain(Sender: TObject);
  end;

var
  frmmain: Tfrmmain;

implementation

{$R *.lfm}

{ Tfrmmain }

procedure Tfrmmain.ExitButtonClick(Sender: TObject);
begin
  Self.Close();
end;

procedure Tfrmmain.FormActivate(Sender: TObject);
begin
  if (FBeenActivated) then
  begin
    FormActivateAgain(Sender);
  end else
  begin
    FBeenActivated := true;
    FormActivateFirst(Sender);
  end;
end;

procedure Tfrmmain.FormCreate(Sender: TObject);
begin
  FBeenActivated := false;
end;

procedure Tfrmmain.FormActivateFirst(Sender: TObject);
begin
  ArrowNavigator := TCustomSDVArrowNavigator.Create(Self);
  ArrowNavigator.Top := 0;
  ArrowNavigator.Left := 0;
  ArrowNavigator.Align := (* Controls. *) TAlign.alClient;

  ArrowNavigatorPanel.InsertControl(ArrowNavigator);
end;

procedure Tfrmmain.FormActivateAgain(Sender: TObject);
begin
  //
end;

end.

