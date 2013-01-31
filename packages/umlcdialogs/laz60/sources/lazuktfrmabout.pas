unit lazuktfrmabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls,
  uktdlgctrls,
  lazuktresabout,
  dummy;

type

  { Tfrmabout }

  Tfrmabout = class(TForm)
    ExitButton: TButton;
    CommentsLabel: TLabel;
    CopyrightLabel: TLabel;
    ProductNameLabel: TLabel;
    CompanyLabel: TLabel;
    VersionLabel: TLabel;
    pnCtrls: TPanel;
    ProgramIcon1: TImage;
    SummaryMemo: TMemo;
    PageControl1: TPageControl;
    SummaryTabSheet: TTabSheet;
    CopyrightTabSheet: TTabSheet;
    procedure ExitButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  protected
    { private declarations }

    FContainer:  TSDVDialogComponent;
    FIsActivated: Boolean;

    procedure FormActivateFirstTime(Sender: TObject);
  public
    { public declarations }

    property Container: TSDVDialogComponent
      read FContainer write FContainer default NIL;
  end;

  procedure Execute
    (const ATitle, ACompany: string; Summary: TStrings);

var
  frmabout: Tfrmabout;

implementation

{$R *.lfm}

{ Tfrmabout }

procedure Tfrmabout.ExitButtonClick(Sender: TObject);
begin
  Self.Close();
end;

procedure Tfrmabout.FormActivate(Sender: TObject);
begin
  // performs same goal as "FormShow",
  // but, may be not be available to delphi,
  // or cbuilder versions
  if (not FIsActivated) then
  begin
    FormActivateFirstTime(Sender);
  end;
  FIsActivated := true;
end;

procedure Tfrmabout.FormCreate(Sender: TObject);
begin
  FIsActivated := false;
end;

procedure Tfrmabout.FormActivateFirstTime(Sender: TObject);
begin
  ExitButton.Caption        := resExitButton_Caption;
  SummaryTabSheet.Caption   := resSummaryTabSheet_Caption;
  CopyrightTabSheet.Caption := resCopyrightTabSheet_Caption;
end;

procedure Execute
  (const ATitle, ACompany: string; Summary: TStrings);
var ThisForm: Tfrmabout;
begin
  try
    Application.CreateForm(Tfrmabout, ThisForm);
    Application.ProcessMessages();

    ThisForm.Caption := ATitle;

    ThisForm.SummaryMemo.Lines.Clear();
    ThisForm.SummaryMemo.Lines.Assign(Summary);

    ThisForm.ShowModal();
  finally
    ThisForm.Free();
    ThisForm := nil;
  end;
end;

end.

