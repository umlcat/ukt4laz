{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsgukttrees60;

interface

uses
  ukttreesregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ukttreesregister', @ukttreesregister.Register);
end;

initialization
  RegisterPackage('dsgukttrees60', @Register);
end.
