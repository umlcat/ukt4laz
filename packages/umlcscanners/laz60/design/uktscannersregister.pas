unit uktscannersregister;

interface
uses
  Classes,
{$IFDEF FPC}  
  LResources,
{$ENDIF}
  uktmacros,
  ukttagdictionaries,
  ukttagpages,
  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
{$R 'uktmacros.dcr'}
{$R 'ukttagdictionaries.dcr'}
{$R 'ukttagpages.dcr'}
{$R 'ukttagtreeviews.dcr'}
{$ENDIF} 

procedure Register;
const
  PaletteScannersName = 'UMLCat Scanners';
begin
  RegisterComponents(PaletteScannersName, [TSDVMacrosContainer]);
  RegisterComponents(PaletteScannersName, [TSDVTagDictionary]);
  RegisterComponents(PaletteScannersName, [TSDVTagPage]);
end;

initialization
{$IFDEF FPC} 
{$I 'uktmacros.lrs'}
{$I 'ukttagdictionaries.lrs'}
{$I 'ukttagpages.lrs'}
{$ENDIF}
end.
