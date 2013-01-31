{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsguktstreams60;

interface

uses
  uktstreamsregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uktstreamsregister', @uktstreamsregister.Register);
end;

initialization
  RegisterPackage('dsguktstreams60', @Register);
end.
