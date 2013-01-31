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

unit ukttagclasses;

interface
uses
  Messages, Windows, SysUtils, Classes, Math, Graphics,
  uktTagProps,
  dummy;

const
  EoLn = #13#10;

type

(* TSDVTagMode *)

  TSDVTagMode = (tgSingle, tgBlock);

(* TSDVTagPropertiesEditorProc *)

  TSDVTagObject = class; // forward;

  TSDVTagPropertiesEditorFunc =
    (*@*)function (const Value: TSDVTagObject): Boolean;

(* TSDVTagObject *)

  TSDVTagObject = class(TObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)

    FKeyword: string;
    FEditor: TSDVTagPropertiesEditorFunc;

    FProperties: TSDVTagProperties;
  public
    (* public declarations *)

    constructor Create; virtual;
    destructor Destroy; override;

    function HasEditor: Boolean;

    function IsSystem: Boolean; dynamic; abstract;
    function TagMode: TSDVTagMode; dynamic; abstract;
    function TagCaption: string; dynamic; abstract;

    property Editor: TSDVTagPropertiesEditorFunc
      read FEditor write FEditor;
    property Keyword: string
      read FKeyword write FKeyword;
    property Properties: TSDVTagProperties
      read FProperties write FProperties;
  end;

(* TSDVSingleTagObject *)

  TSDVSingleTagObject = class(TSDVTagObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function TagMode: TSDVTagMode; override;

    function ObjectToHTML: string; dynamic; abstract;
    function ObjectToEHTML: string; dynamic; abstract;
  end;

(* TSDVBlockTagObject *)

  TSDVBlockTagObject = class(TSDVTagObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function TagMode: TSDVTagMode; override;

    function ObjectToHTMLStart: string; dynamic; abstract;
    function ObjectToHTMLFinish: string; dynamic; abstract;

    function ObjectToEHTMLStart: string; dynamic; abstract;
    function ObjectToEHTMLFinish: string; dynamic; abstract;
  end;

(* TSDVEntityTagObject *)

  TSDVEntityTagObject = class(TSDVSingleTagObject)
  private
    (* private declarations *)
  protected
    (* protected declarations *)
  public
    (* public declarations *)

    function IsSystem: Boolean; override;
    function TagMode: TSDVTagMode; override;
  end;

implementation

(* TSDVTagObject *)

constructor TSDVTagObject.Create;
begin
  inherited;
  FEditor := nil;
  FProperties := TSDVTagProperties.Create(TSDVTagProperty);
end;

destructor TSDVTagObject.Destroy;
begin
  FProperties.Free;
  FEditor := nil;
  inherited;
end;

function TSDVTagObject.HasEditor: Boolean;
begin
  Result := Assigned(FEditor);
end;

(* TSDVSingleTagObject *)

function TSDVSingleTagObject.TagMode: TSDVTagMode;
begin
  Result := tgSingle;
end;

(* TSDVBlockTagObject *)

function TSDVBlockTagObject.TagMode: TSDVTagMode;
begin
  Result := tgBlock;
end;

(* TSDVEntityTagObject *)

function TSDVEntityTagObject.IsSystem: Boolean;
begin
  Result := FALSE;
end;

function TSDVEntityTagObject.TagMode: TSDVTagMode;
begin
  Result := tgSingle;
end;

end.
