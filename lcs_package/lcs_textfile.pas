unit lcs_textfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure RegisterTextFile(L: Plua_State);

function TextFileReadToString(L: Plua_State): integer; cdecl;
function TextFileReadToTable(L: Plua_State): integer; cdecl;
function TextFileWriteFromString(L: Plua_State): integer; cdecl;
function TextFileWriteFromTable(L: Plua_State): integer; cdecl;

implementation

procedure RegisterTextFile(L: Plua_State);

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
  RegisterFunction('ReadToString', @TextFileReadToString);
  RegisterFunction('ReadToTable', @TextFileReadToTable);
  RegisterFunction('WriteFromString', @TextFileWriteFromString);
  RegisterFunction('WriteFromTable', @TextFileWriteFromTable);
  lua_setglobal(L, 'TextFile');
end;

function TextFileReadToString(L: Plua_State): integer; cdecl;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.LoadFromFile(lua_tostring(L, -1));
  lua_pushstring(L, s.Text);
  s.Free;
  Result := 1;
end;

function TextFileReadToTable(L: Plua_State): integer; cdecl;
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  s.LoadFromFile(lua_tostring(L, -1));

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;
  Result := 1;
end;

function TextFileWriteFromString(L: Plua_State): integer; cdecl;
var
  s: TStringList;
  filename, Text: string;
  append: boolean;
begin
  filename := lua_tostring(L, -3);
  Text := lua_tostring(L, -2);
  append := lua_toboolean(L, -1);

  s := TStringList.Create;

  if append then
  begin
    s.LoadFromFile(filename);
    s.Add(Text);
  end
  else
  begin
    s.Text := Text;
  end;

  s.SaveToFile(filename);
  s.Free;

  Result := 0;
end;

function TextFileWriteFromTable(L: Plua_State): integer; cdecl;
var
  s: TStringList;
  filename: string;
  append: boolean;
begin
  filename := lua_tostring(L, -3);
  append := lua_toboolean(L, -1);
  s := TStringList.Create;

  if append then
    s.LoadFromFile(filename);

  lua_settop(L, -2);

  lua_pushnil(L);
  while (lua_next(L, -2) <> 0) do
  begin
    s.Add(lua_tostring(L, -1));
    lua_pop(L, 1);
  end;

  s.SaveToFile(filename);
  s.Free;
  Result := 0;
end;

end.

