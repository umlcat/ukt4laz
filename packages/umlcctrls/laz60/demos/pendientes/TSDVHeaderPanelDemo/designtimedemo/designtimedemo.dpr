program designtimedemo;

uses
  Forms,
  ufrmMan in 'ufrmMan.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
