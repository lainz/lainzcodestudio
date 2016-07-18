unit lcs_registerall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure RegisterAll(L: Plua_State);

implementation

uses
  lcs_string, lcs_inifile, lcs_registry, lcs_textfile;

procedure RegisterAll(L: Plua_State);
begin
  RegisterString(L);
  RegisterINIFile(L);
  RegisterRegistry(L);
  RegisterTextFile(L);
end;

end.

