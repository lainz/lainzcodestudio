unit lcs_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure RegisterTable(L: Plua_State; S: TStrings);

function TableCount(L: Plua_State): integer; cdecl;

implementation

procedure RegisterTable(L: Plua_State; S: TStrings);

  procedure RegisterFunction(n: string; f: lua_CFunction);
  var
    reg: luaL_Reg;
  begin
    reg.Name := PChar(n);
    reg.func := f;
    luaL_setfuncs(L, reg, 0);
  end;

var
  s1: TStringList;
begin
  lua_newtable(L);
  RegisterFunction('Count', @TableCount);
  lua_setglobal(L, 'Table');

  s1 := TStringList.Create;
  s1.Add('Table.Concat = table.concat;');
  s1.Add('Table.Insert = table.insert;');
  s1.Add('Table.Remove = table.remove;');
  s1.Add('Table.Sort = table.sort;');
  s.Insert(0, s1.Text);
  s1.Free;
end;

function TableCount(L: Plua_State): integer; cdecl;
var
  i: integer;
begin
  i := 0;
  lua_pushnil(L);
  while (lua_next(L, -2) <> 0) do
  begin
    Inc(i);
    lua_pop(L, 1);
  end;
  lua_pushinteger(L, i);
  Result := 1;
end;

end.

