{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsguktdialogs60;

interface

uses
  uktdialogsregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uktdialogsregister', @uktdialogsregister.Register);
end;

initialization
  RegisterPackage('dsguktdialogs60', @Register);
end.
