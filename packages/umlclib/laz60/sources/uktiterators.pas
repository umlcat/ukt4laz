(**
 **************************************************************************
 *                                                                        *
 *  This file is part of the uktat Developer's Component Library.        *
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

unit uktiterators;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  uktnormobjects,
  dummy;

(**
 ** This unit implements an abstract class,
 ** in order to support the "Iterator" Design Pattern.
 **
 ** This iterator is intended to be used with a "while" loop,
 ** instead of a "for" loop, since the "for" loop used,
 ** in the GoF book is not the same in Pascal.
 **
 ** Example:
 ** ...
 ** var MyIterator: ^IteratorClass;
 ** ...
 ** MyIterator := MyCollection.GetIterator();
 **
 ** // Allocate iterator resources
 ** MyIterator^.Start();
 **
 ** // perform loop
 ** while (MyIterator^.Next()) do
 ** begin
 **   DoSomething(MyIterator^.Current());
 ** end;
 **
 ** // Deallocate iterator resources
 ** MyIterator^.Finish();
 ** ...
 ** Dispose(MyIterator);
 ** ...
 **)

type

  (* TSDVIIterator *)

  TSDVIIterator = interface
    procedure Start();
    procedure Stop();

    function Next(): Boolean;

    function Current(): TObject;
  end;

  (* TSDVAbstractIterator *)

  TSDVAbstractIterator = class(TSDVHalfNormalizedObject)
  private
    (* Private declarations *)
  protected
    (* Protected declarations *)
  public
    (* Public declarations *)

    procedure Start(); virtual; abstract;
    procedure Stop(); virtual; abstract;

    function Next(): Boolean; virtual; abstract;

    function Current(): TObject; virtual; abstract;
  end;


implementation

end.

