(*****************************************************************************
 *                                                                           *
 *  This file is part of the UMLCat Component Library.                       *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 **)

unit uktxmltagansiparsers;

interface

uses
  //Windows,
  //Controls, Forms,
  SysUtils, Classes,
  uktxmltagTokens,
  uktxmltagansisymbols,
  uktxmltagansiscanners;

type

{ TXMLTagState }

  TXMLTagState =
    (xmltsNone, xmltsKeyword, xmltsAttribute, xmltsAssign, xmltsValue);

{ TCustomXMLTagANSIParser }

  TCustomXMLTagANSIParser = class(TComponent)
  private
    { private declarations }
  protected
    { protected declarations }

    FScanner: TCustomXMLTagANSIScanner;
    FHasKeyword: Boolean;

    function getScanner: TCustomXMLTagANSIScanner; virtual;

    procedure setScanner(const Value: TCustomXMLTagANSIScanner); virtual;

    procedure Notification
      (AComponent: TComponent; Operation: TOperation); override;
  public
    { public declarations }

    function Start: Boolean; dynamic;
    function Next
      (var PropName, PropValue: string; var IsKeyword: Boolean): Boolean; dynamic;
    function Finish: Boolean; dynamic;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Scanner: TCustomXMLTagANSIScanner
      read getScanner write setScanner;
  end;

{ TXMLTagANSIParser }

  TXMLTagANSIParser = class(TCustomXMLTagANSIParser)
  published
    { published declarations }

    { TCustomXMLTagANSIParser: }

    property Scanner;
  end;

implementation

{ TCustomXMLTagANSIParser }

function TCustomXMLTagANSIParser.getScanner: TCustomXMLTagANSIScanner;
begin
  Result := FScanner;
end;

procedure TCustomXMLTagANSIParser.setScanner(const Value: TCustomXMLTagANSIScanner);
begin
  if (Value <> FScanner) then
  begin
    FScanner := Value;
  end;
end;

function TCustomXMLTagANSIParser.Start: Boolean;
begin
  Result := Assigned(FScanner) and FScanner.Start;
  FHasKeyword := FALSE;
end;

function TCustomXMLTagANSIParser.Next
  (var PropName, PropValue: string; var IsKeyword: Boolean): Boolean;
var State: TXMLTagState; CanContinue: Boolean;
    TagSymbol: txmltagANSIsymbol;
begin
  Result := Assigned(FScanner); IsKeyword := FALSE;
  if (Result) then
  begin
    State := xmltsNone; CanContinue := TRUE;
    repeat
      Result := Scanner.Next;
      if (Result) then
      begin
        TagSymbol := FScanner.CurrentSymbol;
        //Application.ProcessMessages;

        case (TagSymbol.Token) of
          xmltagtkAssign:
          begin
            State := xmltsAssign;
            // nombre de propiedad reconocida
            // property*s name matched
          end;

          xmltagtkValue:
          begin
            Result := (State = xmltsAssign);
            if (Result) then
            begin
              PropValue := SysUtils.LowerCase(TagSymbol.Text);
              CanContinue := FALSE;
              State := xmltsValue;
              // valor de propiedad reconocida
              // property*s value matched
            end;
          end;

          xmltagtkAttribute:
          begin
            CanContinue := (not FHasKeyword);
            if (CanContinue) then
            begin
              PropName  := SysUtils.LowerCase(TagSymbol.Text);
              PropValue := SysUtils.LowerCase(TagSymbol.Text);
              FHasKeyword := TRUE;
              CanContinue := FALSE;
              IsKeyword := TRUE;

              State := xmltsKeyword;
              // palabraclave reconocida
              // keyword matched
            end else
            begin
              Result := (State = xmltsNone) or (State = xmltsValue);
              if (Result) then
              begin
                PropName  := SysUtils.LowerCase(TagSymbol.Text);
                PropValue := '';
                CanContinue := TRUE;

                State := xmltsKeyword;
                // nombre de propiedad reconocida
                // property*s name matched
              end;
            end;
          end;
          else ;
        end;
      end;
    until (not Result) or (not CanContinue);
  end;
end;

function TCustomXMLTagANSIParser.Finish: Boolean;
begin
  Result := Assigned(FScanner) and FScanner.Finish;
end;

procedure TCustomXMLTagANSIParser.Notification
  (AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (FScanner = AComponent)
      then FScanner := nil;
  end;
end;

constructor TCustomXMLTagANSIParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScanner := nil;
end;

destructor TCustomXMLTagANSIParser.Destroy;
begin
  {Your Code...}
  inherited Destroy;
end;

end.
