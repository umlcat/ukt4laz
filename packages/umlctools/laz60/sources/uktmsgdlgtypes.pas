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

unit uktmsgdlgtypes;

interface
uses
  uktmsgdlgstrs,
  dummy;

type
  TMsgDlgButton =
    ( mbYes,
      mbNo,
      mbYesToAll,
      mbNoToAll,
      mbOK,
      mbCancel,
      mbAbort,
      mbReTry,
      mbIgnore
    );

  TMsgDlgButtons = set of TMsgDlgButton;

  TMsgDlgType =
   ( mtWarning,
     mtError,
     mtInformation,
     mtConfirmation,
     mtHelp,
     mtAbout,
     mtCustomized
    );

const
  FormCaptionsArray: array[TMsgDlgType] of string =
  ( resWarning,
    resError,
    resInformation,
    resConfirmation,
    resHelp,
    resAbout,
    resCustomized
  );

  ButtonsCaptionsArray: array[TMsgDlgButton] of string =
    ( resYes,
      resNo,
      resYesToALL,
      resNoToAll,
      resOK,
      resCancel,
      resAbort,
      resReTry,
      resIgnore
    );

implementation

end.
