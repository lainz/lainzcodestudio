unit lcs_zip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Zipper, LazUTF8, Lua53;

procedure RegisterZip(L: Plua_State);

function ZipAdd(L: Plua_State): integer; cdecl;
function ZipExtract(L: Plua_State): integer; cdecl;
function ZipGetContents(L: Plua_State): integer; cdecl;

implementation

procedure RegisterZip(L: Plua_State);

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
  RegisterFunction('Add', @ZipAdd);
  RegisterFunction('Extract', @ZipExtract);
  RegisterFunction('GetContents', @ZipGetContents);
  lua_setglobal(L, 'Zip');
end;

function ZipAdd(L: Plua_State): integer; cdecl;
var
  zip: TZipper;
  filename: string;
  files: TStringList;
  i: integer;
begin
  filename := lua_tostring(L, -2);
  zip := TZipper.Create;

  files := TStringList.Create;

  lua_pushnil(L);
  while (lua_next(L, -2) <> 0) do
  begin
    files.Add(lua_tostring(L, -1));
    lua_pop(L, 1);
  end;

  for i := 0 to files.Count - 1 do
    zip.Entries.AddFileEntry(files[i], ExtractFileName(files[i]));

  zip.FileName := filename;
  zip.ZipAllFiles;
  zip.Free;
  files.Free;
  Result := 0;
end;

function ZipExtract(L: Plua_State): integer; cdecl;
var
  zip: TUnZipper;
  filename: string;
  destination: string;
  files: TStringList;
  i: integer;
begin
  filename := lua_tostring(L, -3);
  destination := lua_tostring(L, -1);
  zip := TUnZipper.Create;
  zip.FileName := filename;
  zip.OutputPath := destination;

  files := TStringList.Create;

  lua_settop(L, -2);

  lua_pushnil(L);
  while (lua_next(L, -2) <> 0) do
  begin
    files.Add(lua_tostring(L, -1));
    lua_pop(L, 1);
  end;

  zip.UnZipFiles(files);
  zip.Free;
  Result := 0;
end;

function ZipGetContents(L: Plua_State): integer; cdecl;
var
  zip: TUnZipper;
  i, j: integer;
  Name: string;
  includefoldernames: boolean;
begin
  zip := TUnZipper.Create;
  zip.FileName := lua_tostring(L, -2);
  zip.Examine;

  includefoldernames := lua_toboolean(L, -1);

  lua_newtable(L);
  j := 0;
  for i := 0 to zip.Entries.Count - 1 do
  begin
    Name := zip.Entries[i].ArchiveFileName;
    if includefoldernames then
    begin
      lua_pushstring(L, zip.Entries[i].ArchiveFileName);
      lua_rawseti(L, -2, i);
    end
    else
    if UTF8Copy(Name, UTF8Length(Name), 1) <> '/' then
    begin
      lua_pushstring(L, zip.Entries[i].ArchiveFileName);
      lua_rawseti(L, -2, j);
      Inc(j);
    end;
  end;

  zip.Free;
  Result := 1;
end;

end.

