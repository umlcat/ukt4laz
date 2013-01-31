(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the UMLCat's Component Library.                  *
 *                                                                        *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution, *
 *  for details about the copyright.                                      *
 *                                                                        *
 *  This program is distributed in the hope that it will be useful,       *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 *                                                                        *
 **************************************************************************
**)

unit uktpublictreeviews;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  {$IFDEF MSWINDOWS}
  CommCtrl,
  COMCtrls,
  {$ENDIF}
  uktactivatedcontrols,
  uktctrls,
  dummy;

(**
 ** The "TCustomTreeview" class declared in the "COMCtrls" unit,
 ** has some properties (and events) declared as "protected",
 ** that other "TCustom*" similar control classes,
 ** declare them as "public".
 **
 ** Therefore, the "TCustomPublicTreeView" control class,
 ** redeclare that properties (and events).
 **)

type
 (* TCustomPublicTreeView *)

   TCustomPublicTreeView = class(TCustomTreeview)
   private
     (* Private declarations *)
   protected
     (* Protected declarations *)
   public
     (* Public declarations *)

     (* UnPublished declarations *)

     property ParentColor default false;  // default value
     property TabStop default true;       // default value

     property HideSelection; // protected -> public
     property Images;        // protected -> public
     property Items;         // protected -> public
     property ReadOnly;      // protected -> public
     property RowSelect;     // protected -> public
     property ShowButtons;   // protected -> public
     property ShowLines;     // protected -> public
     property StateImages;   // protected -> public

     property OnChanging;    // protected -> public
     property OnClick;       // protected -> public
     property OnCollapsed;   // protected -> public
     property OnCollapsing;  // protected -> public
     property OnExpanded;    // protected -> public
     property OnExpanding;   // protected -> public

     property OnSelectionChanged;    // protected -> public
   end;

implementation

end.

