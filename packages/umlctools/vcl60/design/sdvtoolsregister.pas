unit sdvToolsRegister;

interface
uses
  Classes,
  sdvFileFilters,
  sdvBookMngrs,
  sdvMsgCtrls,
  sdvUndoMngrs,
  sdvDocs;

  procedure Register;

implementation

{.R 'sdvfilefilters.dcr'}
{.R 'sdvbookmngrs.dcr'}

{.R 'sdvmsgctrls.dcr'}

{.R 'sdvundomngrs.dcr'}
{.R 'sdvdocs.dcr'}

procedure Register;
const
  PaletteTools = 'Star Developer Tools';
begin
  //RegisterComponents(PaletteTools, [TSDVFileFiltersContainer]);
  //RegisterComponents(PaletteTools, [TSDVBookMarkMngr]);

  //RegisterComponents(PaletteTools, [TSDVMsgServer]);
  //RegisterComponents(PaletteTools, [TSDVSingleMsgServer]);
  //RegisterComponents(PaletteTools, [TSDVMsgClient]);

  //RegisterComponents(PaletteTools, [TSDVUnDoManager]);

  //RegisterComponents(PaletteTools, [TSDVDocument]);
end;

end.



