program DesignTimeDemo;

uses
  Forms,
  UFMain in 'UFMain.pas' {FMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
