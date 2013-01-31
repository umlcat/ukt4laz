unit ukttreesregister;

interface
uses
  Classes,
{$IFDEF FPC}  
  LResources,
{$ENDIF}
  ukttreecntrs,
  uktmsgtreecntrs,
  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
{$R 'ukttreecntrs.dcr'}
{$R 'uktmsgtreecntrs.dcr'}
{$ENDIF} 

procedure Register;
const
  PaletteTree = 'UMLCat Trees';
begin
  RegisterComponents(PaletteTree, [TSDVTreeContainer]);
  RegisterComponents(PaletteTree, [TSDVMsgTreeContainer]);
end;

initialization
{$IFDEF FPC} 
{$I 'ukttreecntrs.lrs'}
{$I 'uktmsgtreecntrs.lrs'}
{$ENDIF} 
end.



