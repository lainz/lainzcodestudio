{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lcs_package;

interface

uses
  lcs_inifile, lcs_registerall, lcs_registry, lcs_string, lcs_table, 
  lcs_textfile, lcs_zip, lua53, SynHighlighterLua, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SynHighlighterLua', @SynHighlighterLua.Register);
end;

initialization
  RegisterPackage('lcs_package', @Register);
end.
