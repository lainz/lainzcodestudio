unit lcs_application;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, Lua53;

procedure RegisterApplication(L: Plua_State);

function ApplicationExit(L: Plua_State): integer; cdecl;
function ApplicationMinimize(L: Plua_State): integer; cdecl;
function ApplicationRestore(L: Plua_State): integer; cdecl;
function ApplicationSleep(L: Plua_State): integer; cdecl;

implementation

procedure RegisterApplication(L: Plua_State);

  procedure RegisterFunction(n: string; f: lua_CFunction);
  var
    reg: luaL_Reg;
  begin
    reg.Name := PChar(n);
    reg.func := f;
    luaL_setfuncs(L, reg, 0);
  end;

begin
  lua_newtable(L);
  RegisterFunction('Exit', @ApplicationExit);
  RegisterFunction('Minimize', @ApplicationMinimize);
  RegisterFunction('Restore', @ApplicationRestore);
  RegisterFunction('Sleep', @ApplicationSleep);
  lua_setglobal(L, 'Application');
end;

function ApplicationExit(L: Plua_State): integer; cdecl;
begin
  Application.Terminate;
  Result := 0;
end;

function ApplicationMinimize(L: Plua_State): integer; cdecl;
begin
  Application.Minimize;
  Result := 0;
end;

function ApplicationRestore(L: Plua_State): integer; cdecl;
begin
  Application.Restore;
  Result := 0;
end;

function ApplicationSleep(L: Plua_State): integer; cdecl;
begin
  Sleep(lua_tointeger(L, -1));
  Result := 0;
end;

end.

