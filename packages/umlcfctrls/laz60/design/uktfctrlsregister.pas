unit uktfctrlsregister;

interface
uses
  Classes,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  uktfmngrs,
  uktfedits,
  uktfmaskeds,
  uktfmemos,
  uktfctrls,
  //uktdbfctrls;
  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
  {$R 'uktfmngrs.dcr'}
  {$R 'uktfctrls.dcr'}
  {.R 'uktdbfctrls.dcr'}
{$ENDIF}

procedure Register;
const
  PaletteCtrls = 'UMLCat Focused Controls';
begin
  RegisterComponents(PaletteCtrls, [TSDVFocusMngr]);

  RegisterComponents(PaletteCtrls, [TSDVFocusEdit, TSDVFocusMemo, TSDVFocusMaskEdit]);
  RegisterComponents(PaletteCtrls, [TSDVFocusIntegerEdit, TSDVFocusFloatEdit]);

  (*
  RegisterComponents(PaletteCtrls, [TSDVFocusDBEdit, TSDVFocusDBMemo, TSDVFocusDBDropDown]);
  *)
end;


initialization
{$IFDEF FPC}
  {$I 'uktfmngrs.lrs'}
  {$I 'uktfctrls.lrs'}
  {.I 'uktdbfctrls.lrs'}
{$ENDIF}
end.
