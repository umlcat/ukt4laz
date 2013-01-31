unit ukttoolsregister;

interface
uses
  Classes,
  uktfilefilters,
  uktbookmngrs,
  uktmsgctrls,
  uktundomngrs,
  uktdocs,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
{$R 'uktfilefilters.dcr'}
{$R 'uktbookmngrs.dcr'}

{$R 'uktmsgctrls.dcr'}

{$R 'uktundomngrs.dcr'}
{$R 'uktdocs.dcr'}
{$ENDIF}

procedure Register;
const
  PaletteTools = 'UMLCat Tools';
begin
  RegisterComponents(PaletteTools, [TSDVFileFiltersContainer]);
  RegisterComponents(PaletteTools, [TSDVBookMarkMngr]);

  RegisterComponents(PaletteTools, [TSDVMsgServer]);
  RegisterComponents(PaletteTools, [TSDVSingleClientMsgServer]);
  RegisterComponents(PaletteTools, [TSDVMsgClient]);
  RegisterComponents(PaletteTools, [TSDVSingleServerMsgClient]);

  RegisterComponents(PaletteTools, [TSDVUnDoManager]);

  RegisterComponents(PaletteTools, [TSDVDocument]);
  RegisterComponents(PaletteTools, [TSDVDelegateDocument]);
end;

initialization
{$IFDEF FPC}
{$I 'uktfilefilters.lrs'}
{$I 'uktbookmngrs.lrs'}

{$I 'uktmsgctrls.lrs'}

{$I 'uktundomngrs.lrs'}
{$I 'uktdocs.lrs'}
{$ENDIF}
end.

