unit lcs_string;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LazUTF8, Lua53;

procedure RegisterString(L: Plua_State);

//function AbbreviateFilePath(L: Plua_State): integer; cdecl;
function StringAsc(L: Plua_State): integer; cdecl;
function StringChar(L: Plua_State): integer; cdecl;
function StringCompare(L: Plua_State): integer; cdecl;
function StringCompareFileVersions(L: Plua_state): integer; cdecl;
function StringCompareNoCase(L: Plua_State): integer; cdecl;
function StringConcat(L: Plua_State): integer; cdecl;
function StringFind(L: Plua_State): integer; cdecl;
//function StringGetFormattedSize(L: Plua_State): integer; cdecl;
function StringLeft(L: Plua_State): integer; cdecl;
function StringLength(L: Plua_State): integer; cdecl;
function StringLower(L: Plua_State): integer; cdecl;
//function StringMakePath(L: Plua_State): integer; cdecl;
function StringMid(L: Plua_State): integer; cdecl;
function StringRepeat(L: Plua_State): integer; cdecl;
function StringReplaceLua(L: Plua_State): integer; cdecl;
function StringReverseFind(L: Plua_State): integer; cdecl;
function StringRight(L: Plua_State): integer; cdecl;
//function StringSplitPath(L: Plua_State): integer; cdecl;
function StringToNumber(L: Plua_State): integer; cdecl;
//function StringTrimLeft(L: Plua_State): integer; cdecl;
//function StringTrimRight(L: Plua_State): integer; cdecl;
function StringUpper(L: Plua_State): integer; cdecl;

implementation

procedure RegisterString(L: Plua_State);

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
  RegisterFunction('Asc', @StringAsc);
  RegisterFunction('Char', @StringChar);
  RegisterFunction('Compare', @StringCompare);
  RegisterFunction('CompareFileVersions', @StringCompareFileVersions);
  RegisterFunction('CompareNoCase', @StringCompareNoCase);
  RegisterFunction('Concat', @StringConcat);
  RegisterFunction('Find', @StringFind);
  RegisterFunction('Left', @StringLeft);
  RegisterFunction('Length', @StringLength);
  RegisterFunction('Lower', @StringLower);
  RegisterFunction('Mid', @StringMid);
  RegisterFunction('Repeat', @StringRepeat);
  RegisterFunction('Replace', @StringReplaceLua);
  RegisterFunction('ReverseFind', @StringReverseFind);
  RegisterFunction('Right', @StringRight);
  RegisterFunction('ToNumber', @StringToNumber);
  RegisterFunction('Upper', @StringUpper);
  lua_setglobal(L, 'String');
end;

function StringAsc(L: Plua_State): integer; cdecl;
var
  i: integer;
begin
  lua_pushinteger(L, UTF8CharacterToUnicode(lua_tostring(L, -1), i));
  Result := 1;
end;

function StringChar(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UnicodeToUTF8(lua_tointeger(L, -1)));
  Result := 1;
end;

function StringCompare(L: Plua_State): integer; cdecl;
var
  s1, s2: string;
  r: integer;
begin
  s1 := lua_tostring(L, -2);
  s2 := lua_tostring(L, -1);

  if (s1 = s2) then
    r := 0
  else if (s1 < s2) then
    r := -1
  else
    r := 1;

  lua_pushinteger(L, r);
  Result := 1;
end;

function StringCompareFileVersions(L: Plua_state): integer; cdecl;

  function Compare(s1, s2: TStringList; index: integer): integer;
  begin
    if (s1[index] = s2[index]) then
      Result := 0
    else if (s1[index] < s2[index]) then
      Result := -1
    else
      Result := 1;
  end;

var
  i, j, res: integer;
  s1, s2: TStringList;
begin
  s1 := TStringList.Create;
  s2 := TStringList.Create;
  s1.Delimiter := '.';
  s2.Delimiter := '.';
  s1.DelimitedText := lua_tostring(L, -2);
  s2.DelimitedText := lua_tostring(L, -1);

  if (s1.Count = s2.Count) then
    j := s1.Count
  else if (s1.Count < s2.Count) then
    j := s1.Count
  else
    j := s2.Count;

  for i := 0 to j - 1 do
  begin
    res := Compare(s1, s2, i);
    if res <> 0 then
      break;
  end;

  s1.Free;
  s2.Free;

  lua_pushinteger(L, res);
  Result := 1;
end;

function StringCompareNoCase(L: Plua_State): integer; cdecl;
var
  s1, s2: string;
  r: integer;
begin
  s1 := UTF8LowerCase(lua_tostring(L, -2));
  s2 := UTF8LowerCase(lua_tostring(L, -1));

  if (s1 = s2) then
    r := 0
  else if (s1 < s2) then
    r := -1
  else
    r := 1;

  lua_pushinteger(L, r);
  Result := 1;
end;

function StringConcat(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, string(lua_tostring(L, -2)) + string(lua_tostring(L, -1)));
  Result := 1;
end;

function StringFind(L: Plua_State): integer; cdecl;
var
  s1, s2: string;
  index, res: integer;
begin
  s1 := lua_tostring(L, -4);
  s2 := lua_tostring(L, -3);
  index := integer(lua_tointeger(L, -2));
  if lua_toboolean(L, -1) then
    res := UTF8Pos(s2, s1, index)
  else
    res := UTF8Pos(UTF8LowerCase(s2), UTF8LowerCase(s1), index);
  lua_pushinteger(L, res);
  Result := 1;
end;

function StringLeft(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UTF8LeftStr(lua_tostring(L, -2), lua_tointeger(L, -1)));
  Result := 1;
end;

function StringLength(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, UTF8Length(lua_tostring(L, -1)));
  Result := 1;
end;

function StringLower(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UTF8LowerCase(lua_tostring(L, -1)));
  Result := 1;
end;

function StringMid(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UTF8Copy(lua_tostring(L, -3), lua_tointeger(L, -2),
    lua_tointeger(L, -1)));
  Result := 1;
end;

function StringRepeat(L: PLua_State): integer; cdecl;
var
  i, j: int64;
  s, ls: string;
begin
  ls := lua_tostring(L, -2);
  j := integer(lua_tointeger(L, -1));
  s := '';
  i := 0;
  while (i < j) do
  begin
    s += ls;
    Inc(i);
  end;
  lua_pushstring(L, s);
  Result := 1;
end;

function StringReplaceLua(L: Plua_State): integer; cdecl;
begin
  if lua_toboolean(L, -1) then
    lua_pushstring(L, StringReplace(lua_tostring(L, -4), lua_tostring(L, -3),
      lua_tostring(L, -2), [rfReplaceAll]))
  else
    lua_pushstring(L, StringReplace(lua_tostring(L, -4), lua_tostring(L, -3),
      lua_tostring(L, -2), [rfReplaceAll, rfIgnoreCase]));
  Result := 1;
end;

function StringReverseFind(L: Plua_State): integer; cdecl;

  function UTF8ReverseString(const AText: string): string;
  var
    i: integer;
  begin
    Result := '';
    for i := UTF8Length(AText) - 1 downto 0 do
      Result := Result + UTF8Copy(AText, i + 1, 1);
  end;

var
  s1, s2: string;
  res: integer;
begin
  s1 := UTF8ReverseString(lua_tostring(L, -3));
  s2 := UTF8ReverseString(lua_tostring(L, -2));
  if lua_toboolean(L, -1) then
    res := UTF8Pos(s2, s1)
  else
    res := UTF8Pos(UTF8LowerCase(s2), UTF8LowerCase(s1));
  if res <> 0 then
    res := UTF8Length(s1) - UTF8Length(s2) - res + 2;
  lua_pushinteger(L, res);
  Result := 1;
end;

function StringRight(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UTF8RightStr(lua_tostring(L, -2), lua_tointeger(L, -1)));
  Result := 1;
end;

function StringToNumber(L: Plua_State): integer; cdecl;
begin
  lua_pushnumber(L, lua_tonumber(L, -1));
  Result := 1;
end;

function StringUpper(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, UTF8UpperCase(lua_tostring(L, -1)));
  Result := 1;
end;

end.
