program RuntimeDemo;

uses
  Forms,
  UFMain in 'UFMain.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
