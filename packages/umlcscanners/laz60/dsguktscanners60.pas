{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dsguktscanners60;

interface

uses
  uktscannersregister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uktscannersregister', @uktscannersregister.Register);
end;

initialization
  RegisterPackage('dsguktscanners60', @Register);
end.
