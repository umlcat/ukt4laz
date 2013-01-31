unit uktctrlsregister;

interface
uses
  Classes,
{$IFDEF FPC}  
  LResources,
{$ENDIF}
  uktcheckboxes,
  uktedits,
  uktmemos,
  uktmaskeds,
  ukttreeviews,
  uktstatetreeviews,
  uktpanels,
  uktpgctrls,
  uktspeedbtns,
  uktbitbtns,
  ukttogglebitbtns,
  ukttogglespeedbtns,

  //uktgradpanels,

  uktpaneltabctrls,

  uktcomboboxes,
  uktcolorcombos,
  uktitemcombos,

  uktstmemos,
  uktcomboedits,

  uktarrownavs,
  //uktdbarrownavs,

  uktheaderpanels,

  uktdropbtns,
  uktdropmenubtns,

  uktformcontrols,

  uktpaneltreeviews,
  uktmsgpaneltreeviews,

  dummy;

  procedure Register;

implementation

{$IFDEF DELPHI}
  {$R 'uktcheckboxes.lrs'}
  {$R 'uktedits.lrs'}
  {$R 'uktmemos.lrs'}
  {$R 'uktmaskeds.lrs'}

  {$R 'ukttreeviews.lrs'}
  {$R 'uktstatetreeviews.lrs'}
  {$R 'uktpanels.lrs'}

  {$R 'uktspeedbtns.lrs'}
  {$R 'uktbitbtns.lrs'}

  {$R 'ukttogglebitbtns.lrs'}
  {$R 'ukttogglespeedbtns.lrs'}

  {$R 'uktpgctrls.lrs'}
  {.R 'uktgradpanels.lrs'}

  {$R 'uktpaneltabctrls.lrs'}

  {$R 'uktcomboboxes.lrs'}
  {$R 'uktcolorcombos.lrs'}
  {$R 'uktitemcombos.lrs'}

  {$R 'uktstmemos.lrs'}

  {$R 'uktcomboedits.lrs'}

  {$R 'uktarrownavs.lrs'}
  {.R 'uktdbarrownavs.lrs'}

  {$R 'uktheaderpanels.lrs'}

  {$R 'uktformcontrols.lrs'}

  {$R 'uktdropbtns.lrs'}
  {$R 'uktdropmenubtns.lrs'}
{$ENDIF}

procedure Register;
const
  PaletteTree    = 'UMLCat Trees';
  PaletteCtrls   = 'UMLCat Controls';
  PaletteCBCtrls = 'UMLCat ComboBoxes';
  //PaletteDBCtrls = 'UMLCat Data Controls';
begin
  RegisterComponents(PaletteCtrls, [TSDVCheckbox]);
  RegisterComponents(PaletteCtrls, [TSDVEdit]);
  RegisterComponents(PaletteCtrls, [TSDVMemo]);
  RegisterComponents(PaletteCtrls, [TSDVMaskEdit]);

  RegisterComponents(PaletteCtrls, [TSDVTreeView]);
  RegisterComponents(PaletteCtrls, [TSDVStateTreeView]);

  RegisterComponents(PaletteCtrls, [TSDVPanel]);

  RegisterComponents(PaletteCtrls, [TSDVPageControl]);

  RegisterComponents(PaletteCtrls, [TSDVSpeedButton]);
  RegisterComponents(PaletteCtrls, [TSDVBitBtn]);

  RegisterComponents(PaletteCtrls, [TSDVToggledBitBtn]);
  RegisterComponents(PaletteCtrls, [TSDVToggledSpeedButton]);

  //RegisterComponents(PaletteCtrls, [TSDVGradientPanel]);

  RegisterComponents(PaletteCtrls, [TSDVPanelTabControl]);

  RegisterComponents(PaletteCBCtrls, [TSDVCombobox]);

  RegisterComponents(PaletteCBCtrls, [TSDVDayCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVMonthCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVTimeCombobox]);

  RegisterComponents(PaletteCBCtrls, [TSDVLanguageCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVConfigCombobox]);

  RegisterComponents(PaletteCBCtrls, [TSDVDOSColorCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVWin16ColorCombobox]);

  RegisterComponents(PaletteCBCtrls, [TSDVObjectCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVDataCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVIntegerCombobox]);
  RegisterComponents(PaletteCBCtrls, [TSDVDoubleIntegerCombobox]);

  RegisterComponents(PaletteCtrls, [TSDVStaticMemo]);

  RegisterComponents(PaletteCtrls, [TSDVComboEdit]);

  RegisterComponents(PaletteCtrls, [TSDVDelegateArrowNavigator]);
  //RegisterComponents(PaletteDBCtrls, [TSDVDBArrowNavigator]);

  RegisterComponents(PaletteCtrls, [TSDVHeaderPanel]);

  RegisterComponents(PaletteCtrls, [TSDVFormPanel]);

  RegisterComponents(PaletteCtrls, [TSDVDropDownButton]);
  RegisterComponents(PaletteCtrls, [TSDVDropDownMenuButton]);

//  RegisterComponents(PaletteCtrls, [TSDVCollectionListBox]);
//  RegisterComponents(PaletteCtrls, [TSDVListBoxCollectionContainer]);

  RegisterComponents(PaletteTree, [TSDVPanelTreeView]);
  RegisterComponents(PaletteTree, [TSDVMsgPanelTreeView]);
end;

initialization
{$IFDEF FPC} 
  {$I 'uktcheckboxes.lrs'}
  {$I 'uktedits.lrs'}
  {$I 'uktmemos.lrs'}
  {$I 'uktmaskeds.lrs'}

  {$I 'ukttreeviews.lrs'}
  {$I 'uktstatetreeviews.lrs'}
  {$I 'uktpanels.lrs'}

  {$I 'uktspeedbtns.lrs'}
  {$I 'uktbitbtns.lrs'}

  {$I 'ukttogglebitbtns.lrs'}
  {$I 'ukttogglespeedbtns.lrs'}

  {$I 'uktpgctrls.lrs'}
  {.I 'uktgradpanels.lrs'}

  {$I 'uktpaneltabctrls.lrs'}

  {$I 'uktcomboboxes.lrs'}
  {$I 'uktcolorcombos.lrs'}
  {$I 'uktitemcombos.lrs'}

  {$I 'uktstmemos.lrs'}

  {$I 'uktcomboedits.lrs'}

  {$I 'uktarrownavs.lrs'}
  {.I 'uktdbarrownavs.lrs'}

  {$I 'uktheaderpanels.lrs'}

  {$I 'uktdropbtns.lrs'}
  {$I 'uktdropmenubtns.lrs'}

  {$I 'uktformcontrols.lrs'}

  {$I 'uktpaneltreeviews.lrs'}
  {$I 'uktmsgpaneltreeviews.lrs'}
{$ENDIF}
end.

