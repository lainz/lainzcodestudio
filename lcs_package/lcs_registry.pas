unit lcs_registry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, Lua53;

procedure RegisterRegistry(L: Plua_State);

function RegistryCreateKey(L: Plua_State): integer; cdecl;
function RegistryDeleteKey(L: Plua_State): integer; cdecl;
function RegistryDeleteValue(L: Plua_State): integer; cdecl;
function RegistryDoesKeyExist(L: Plua_State): integer; cdecl;
function RegistryGetAccess(L: Plua_State): integer; cdecl;
function RegistryGetKeyNames(L: Plua_State): integer; cdecl;
function RegistryGetValue(L: Plua_State): integer; cdecl;
function RegistryGetValueNames(L: Plua_State): integer; cdecl;
function RegistryGetValueType(L: Plua_State): integer; cdecl;
function RegistrySetValue(L: Plua_State): integer; cdecl;

implementation

procedure RegisterRegistry(L: Plua_State);

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
  RegisterFunction('CreateKey', @RegistryCreateKey);
  RegisterFunction('DeleteKey', @RegistryDeleteKey);
  RegisterFunction('DeleteValue', @RegistryDeleteValue);
  RegisterFunction('DoesKeyExist', @RegistryDoesKeyExist);
  RegisterFunction('GetAccess', @RegistryGetAccess);
  RegisterFunction('GetKeyNames', @RegistryGetKeyNames);
  RegisterFunction('GetValue', @RegistryGetValue);
  RegisterFunction('GetValueNames', @RegistryGetValueNames);
  RegisterFunction('GetValueType', @RegistryGetValueType);
  RegisterFunction('SetValue', @RegistrySetValue);
  lua_setglobal(L, 'Registry');
end;

function GetRootKey(i: integer): HKEY;
begin
  case i of
    0: Result := HKEY_CLASSES_ROOT;
    1: Result := HKEY_CURRENT_CONFIG;
    2: Result := HKEY_CURRENT_USER;
    3: Result := HKEY_LOCAL_MACHINE;
    4: Result := HKEY_USERS;
    else
      Result := HKEY_CLASSES_ROOT;
  end;
end;

function RegistryCreateKey(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -2));
  reg.CreateKey(lua_tostring(L, -1));
  reg.Free;
  Result := 0;
end;

function RegistryDeleteKey(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -2));
  reg.DeleteKey(lua_tostring(L, -1));
  reg.Free;
  Result := 0;
end;

function RegistryDeleteValue(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -3));
  reg.OpenKey(lua_tostring(L, -2), True);
  reg.DeleteValue(lua_tostring(L, -1));
  reg.Free;
  Result := 0;
end;

function RegistryDoesKeyExist(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -2));
  lua_pushboolean(L, reg.OpenKeyReadOnly(lua_tostring(L, -1)));
  reg.Free;
  Result := 1;
end;

function RegistryGetAccess(L: Plua_State): integer; cdecl;
begin

end;

function RegistryGetKeyNames(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
  s: TStringList;
  i: integer;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -2));
  reg.OpenKeyReadOnly(lua_tostring(L, -1));
  s := TStringList.Create;
  reg.GetKeyNames(s);
  reg.Free;

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;
  Result := 1;
end;

function RegistryGetValue(L: Plua_State): integer; cdecl;
begin

end;

function RegistryGetValueNames(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
  s: TStringList;
  i: integer;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -2));
  reg.OpenKeyReadOnly(lua_tostring(L, -1));
  s := TStringList.Create;
  reg.GetValueNames(s);
  reg.Free;

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;
  Result := 1;
end;

function RegistryGetValueType(L: Plua_State): integer; cdecl;
begin

end;

function RegistrySetValue(L: Plua_State): integer; cdecl;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.RootKey := GetRootKey(lua_tointeger(L, -5));
  reg.OpenKey(lua_tostring(L, -4), True);
  case lua_tointeger(L, -1) of
    1: reg.WriteString(lua_tostring(L, -3), lua_tostring(L, -2));
    2: reg.WriteExpandString(lua_tostring(L, -3), lua_tostring(L, -2));
    3: ;
    4: reg.WriteInteger(lua_tostring(L, -3), lua_tointeger(L, -2));
    7: ;
  end;
  reg.Free;
  Result := 0;
end;

end.

