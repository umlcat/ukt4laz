unit sdvstreamsregister;

interface
uses
  Classes,
{$IFDEF DELPHI}
{$IFDEF VER140}
  DesignIntf, DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
{$ENDIF}
{$IFDEF FPC}  
  LResources,
{$ENDIF} 
  sdvfilestreams,
  sdvansicharstreams,
  sdvshortansistrstreams,
  sdvansitextstreams,
  sdvstrfilestreams,
  sdvstrliststreams,
  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
{$R 'sdvfilestreams.dcr'}
{$R 'sdvansicharstreams.dcr'}
{$R 'sdvansitextstreams.dcr'}
{$R 'sdvshortansistrstreams.dcr'}
{$R 'sdvstrfilestreams.dcr'}
{$R 'sdvstrliststreams.dcr'}
{$ENDIF} 

procedure Register;
const PaletteName = 'UMLCat Streams';
begin
  RegisterComponents(PaletteName, [TSDVFileStream]);
  RegisterComponents(PaletteName, [TSDVStrFileStream]);
  RegisterComponents(PaletteName, [TSDVStringListStream]);

  RegisterComponents(PaletteName, [TSDVAnsiCharStream]);
  RegisterComponents(PaletteName, [TSDVShortAnsiStringStream]);
  RegisterComponents(PaletteName, [TSDVAnsiTextStream]);
  RegisterComponents(PaletteName, [TSDVAnsiSourceStream]);
end;

initialization
{$IFDEF FPC} 
{$I 'sdvfilestreams.lrs'}
{$I 'sdvansicharstreams.lrs'}
{.I 'sdvshortansistrstreams.lrs'}
{$I 'sdvansitextstreams.lrs'}
{$I 'sdvstrfilestreams.lrs'}
{$I 'sdvstrliststreams.lrs'}
{$ENDIF} 
end.
