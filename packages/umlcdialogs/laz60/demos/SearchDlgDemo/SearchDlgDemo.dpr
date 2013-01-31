program SearchDlgDemo;

uses
  Forms,
  ufrmSearchDlgDemo in 'ufrmSearchDlgDemo.pas' {frmSearchDlgDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSearchDlgDemo, frmSearchDlgDemo);
  Application.Run;
end.
