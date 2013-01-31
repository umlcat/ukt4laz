unit uktdialogsregister;

interface
uses
  Classes,
  {$IFDEF DELPHI}  
  DesignEditors,
  DesignIntf,
  {$ENDIF} 
  {$IFDEF FPC}  
  LResources,
  {$ENDIF}
  uktaboutdlgs,
  //uktsrchdlgs,
  uktsrchtxtdlgs,
  uktsrchflddlgs;

  procedure Register;

implementation

{$IFDEF DELPHI}
{$R 'uktaboutdlgs.dcr'}
{$R 'uktsrchtxtdlgs.dcr'}
{$R 'uktsrchflddlgs.dcr'}
{$ENDIF} 

procedure Register;
const Palette  = 'UMLCat Dialogs';
begin
  RegisterComponents(Palette, [TSDVAboutDialog]);

  RegisterComponents(Palette, [TSDVSearchTextDialog]);
  RegisterComponents(Palette, [TSDVReplaceTextDialog]);

  RegisterComponents(Palette, [TSDVSearchFieldDialog]);
  RegisterComponents(Palette, [TSDVReplaceFieldDialog]);
end;

initialization
{$IFDEF FPC}
{$I 'uktaboutdlgs.lrs'}
{$I 'uktsrchtxtdlgs.lrs'}
{$I 'uktsrchflddlgs.lrs'}
{$ENDIF} 
end.
