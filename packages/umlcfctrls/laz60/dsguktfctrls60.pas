{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsguktfctrls60;

interface

uses
  uktfctrlsregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uktfctrlsregister', @uktfctrlsregister.Register);
end;

initialization
  RegisterPackage('dsguktfctrls60', @Register);
end.
