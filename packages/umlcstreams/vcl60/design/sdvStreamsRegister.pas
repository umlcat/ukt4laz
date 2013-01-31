unit sdvStreamsRegister;

interface
uses
{$IFDEF VER140}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  Classes,
  sdvFileStreams,
  sdvAnsiCharStreams,
  sdvShortAnsiStrStreams,
  sdvAnsiTextStreams,
  sdvStrFileStreams,
  sdvStrListStreams;

  procedure Register;

implementation

{$R sdvFileStreams.dcr}
{$R sdvAnsiCharStreams.dcr}
{$R sdvAnsiTextStreams.dcr}
{$R sdvShortAnsiStrStreams.dcr}
{$R sdvStrFileStreams.dcr}
{$R sdvStrListStreams.dcr}

procedure Register;
const PaletteName = 'Star Developer Streams';
begin
  RegisterComponents(PaletteName, [TSDVFileStream]);
  RegisterComponents(PaletteName, [TSDVStrFileStream]);
  RegisterComponents(PaletteName, [TSDVStringListStream]);

  RegisterComponents(PaletteName, [TSDVAnsiCharStream]);
  RegisterComponents(PaletteName, [TSDVShortAnsiStringStream]);
  RegisterComponents(PaletteName, [TSDVAnsiTextStream]);
  RegisterComponents(PaletteName, [TSDVAnsiSourceStream]);
end;

end.
