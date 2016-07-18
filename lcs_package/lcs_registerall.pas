unit lcs_registerall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure RegisterAll(L: Plua_State);

implementation

uses
  lcs_string, lcs_inifile, lcs_registry;

procedure RegisterAll(L: Plua_State);
begin
  RegisterString(L);
  RegisterINIFile(L);
  RegisterRegistry(L);
end;

end.

