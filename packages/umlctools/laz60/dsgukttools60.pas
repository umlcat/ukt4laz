{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsgukttools60;

interface

uses
  ukttoolsregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ukttoolsregister', @ukttoolsregister.Register);
end;

initialization
  RegisterPackage('dsgukttools60', @Register);
end.
