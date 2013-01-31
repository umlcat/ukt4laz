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

unit uktobjtypes;

  // Encapsulate standard and simple types into objects,
  // like Java

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uktnormobjects,
  uktbooleans,
  dummy;

type

{ TSDVPrimitiveTypeObject }

  TSDVPrimitiveTypeObject = class(TSDVHalfNormalizedObject)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }

    function AsText: string; override;
  end;

{ TSDVBooleanObject }

  TSDVBooleanObject = class(TSDVPrimitiveTypeObject)
  private
    { private declarations }
  protected
    { protected declarations }

    FValue: Boolean;

    function getValue: Boolean;
    procedure setValue(const AValue: Boolean);
  public
    { public declarations }

    function AsText: string; override;

    property Value: Boolean read getValue write setValue;
  end;

{ TSDVCharObject }

  TSDVCharObject = class(TSDVPrimitiveTypeObject)
  private
    { private declarations }
  protected
    { protected declarations }

    FValue: Char;

    function getValue: Char;
    procedure setValue(const AValue: Char);
  public
    { public declarations }

    function AsText: string; override;

    property Value: Char read getValue write setValue;
  end;

{ TSDVStringObject }

  TSDVStringObject = class(TSDVPrimitiveTypeObject)
  private
    { private declarations }
  protected
    { protected declarations }

    FValue: String;

    function getValue: String;
    procedure setValue(const AValue: String);
  public
    { public declarations }

    function AsText: string; override;

    property Value: String read getValue write setValue;
  end;

{ TSDVShortIntObject }

  TSDVShortIntObject = class(TSDVPrimitiveTypeObject)
  private
    { private declarations }
  protected
    { protected declarations }

    FValue: ShortInt;

    function getValue: ShortInt;
    procedure setValue(const AValue: ShortInt);
  public
    { public declarations }

    function AsText: string; override;

    property Value: ShortInt read getValue write setValue;
  end;

implementation

{ TSDVPrimitiveTypeObject }

function TSDVPrimitiveTypeObject.AsText: string;
begin
  Result := '';
end;

{ TSDVBooleanObject }

function TSDVBooleanObject.getValue: Boolean;
begin
  Result := FValue;
end;

procedure TSDVBooleanObject.setValue(const AValue: Boolean);
begin
  FValue := AValue;
end;

function TSDVBooleanObject.AsText: string;
begin
  Result := uktbooleans.BoolToStr(FValue);
end;

{ TSDVCharObject }

function TSDVCharObject.getValue: Char;
begin
  Result := FValue;
end;

procedure TSDVCharObject.setValue(const AValue: Char);
begin
  FValue := AValue;
end;

function TSDVCharObject.AsText: string;
begin
  Result := FValue;
end;

{ TSDVStringObject }

function TSDVStringObject.getValue: String;
begin
  Result := FValue;
end;

procedure TSDVStringObject.setValue(const AValue: String);
begin
  FValue := AValue;
end;

function TSDVStringObject.AsText: string;
begin
  Result := FValue;
end;

{ TSDVShortIntObject }

function TSDVShortIntObject.getValue: ShortInt;
begin
  Result := FValue;
end;

procedure TSDVShortIntObject.setValue(const AValue: ShortInt);
begin
  FValue := AValue;
end;

function TSDVShortIntObject.AsText: string;
begin
  Result := IntToStr(FValue);
end;

end.

