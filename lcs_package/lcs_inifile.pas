unit lcs_inifile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles, Lua53;

procedure RegisterINIFile(L: Plua_State);

function INIFileDeleteSection(L: Plua_State): integer; cdecl;
function INIFileDeleteValue(L: Plua_State): integer; cdecl;
function INIFileGetSectionNames(L: Plua_State): integer; cdecl;
function INIFileGetValue(L: Plua_State): integer; cdecl;
function INIFileGetValueNames(L: Plua_State): integer; cdecl;
function INIFileSetValue(L: Plua_State): integer; cdecl;

implementation

procedure RegisterINIFile(L: Plua_State);

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
  RegisterFunction('DeleteSection', @INIFileDeleteSection);
  RegisterFunction('DeleteValue', @INIFileDeleteValue);
  RegisterFunction('GetSectionNames', @INIFileGetSectionNames);
  RegisterFunction('GetValue', @INIFileGetValue);
  RegisterFunction('GetValueNames', @INIFileGetValueNames);
  RegisterFunction('SetValue', @INIFileSetValue);
  lua_setglobal(L, 'INIFile');
end;

function INIFileDeleteSection(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(lua_tostring(L, -2));
  ini.EraseSection(lua_tostring(L, -1));
  ini.Free;
  Result := 0;
end;

function INIFileDeleteValue(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(lua_tostring(L, -3));
  ini.DeleteKey(lua_tostring(L, -2), lua_tostring(L, -1));
  ini.Free;
  Result := 0;
end;

function INIFileGetSectionNames(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
  s: TStringList;
  i: integer;
begin
  ini := TIniFile.Create(lua_tostring(L, -1));
  s := TStringList.Create;
  ini.ReadSections(s);
  ini.Free;

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;
  Result := 1;
end;

function INIFileGetValue(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
  res: string;
begin
  ini := TIniFile.Create(lua_tostring(L, -3));
  res := ini.ReadString(lua_tostring(L, -2), lua_tostring(L, -1), EmptyStr);
  ini.Free;
  lua_pushstring(L, res);
  Result := 1;
end;

function INIFileGetValueNames(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
  s: TStringList;
  i: integer;
begin
  ini := TIniFile.Create(lua_tostring(L, -2));
  s := TStringList.Create;
  ini.ReadSection(lua_tostring(L, -1), s);
  ini.Free;

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;
  Result := 1;
end;

function INIFileSetValue(L: Plua_State): integer; cdecl;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(lua_tostring(L, -4));
  ini.WriteString(lua_tostring(L, -3), lua_tostring(L, -2), lua_tostring(L, -1));
  ini.Free;
  Result := 0;
end;

end.

