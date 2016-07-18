unit lcs_folder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, Lua53;

procedure RegisterFolder(L: Plua_State);

function FolderCreate(L: Plua_State): integer; cdecl;
function FolderDelete(L: Plua_State): integer; cdecl;
function FolderDeleteTree(L: Plua_State): integer; cdecl;
function FolderDoesExist(L: Plua_State): integer; cdecl;
function FolderFind(L: Plua_State): integer; cdecl;
function FolderGetCurrent(L: Plua_State): integer; cdecl;
function FolderRename(L: Plua_State): integer; cdecl;
function FolderSetCurrent(L: Plua_State): integer; cdecl;

implementation

procedure RegisterFolder(L: Plua_State);

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
  RegisterFunction('Create', @FolderCreate);
  RegisterFunction('Delete', @FolderDelete);
  RegisterFunction('DeleteTree', @FolderDeleteTree);
  RegisterFunction('DoesExist', @FolderDoesExist);
  RegisterFunction('Find', @FolderFind);
  RegisterFunction('GetCurrent', @FolderGetCurrent);
  RegisterFunction('Rename', @FolderRename);
  RegisterFunction('SetCurrent', @FolderSetCurrent);
  lua_setglobal(L, 'Folder');
end;

function FolderCreate(L: Plua_State): integer; cdecl;
begin
  CreateDirUTF8(lua_tostring(L, -1));
  Result := 0;
end;

function FolderDelete(L: Plua_State): integer; cdecl;
begin
  RemoveDirUTF8(lua_tostring(L, -1));
  Result := 0;
end;

function FolderDeleteTree(L: Plua_State): integer; cdecl;
begin
  DeleteDirectory(lua_tostring(L, -1), False);
  Result := 0;
end;

function FolderDoesExist(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, DirectoryExistsUTF8(lua_tostring(L, -1)));
  Result := 1;
end;

function FolderFind(L: Plua_State): integer; cdecl;
var
  s: TStringList;
  i: integer;
  path, mask: string;
  subdirs: boolean;
begin
  path := lua_tostring(L, -3);
  mask := lua_tostring(L, -2);
  subdirs := lua_toboolean(L, -1);

  s := TStringList.Create;
  FindAllDirectories(s, path, subdirs);

  for i := s.Count - 1 downto 0 do
    if (UTF8Pos(mask, s[i]) = 0) then
      s.Delete(i);

  lua_newtable(L);
  for i := 0 to s.Count - 1 do
  begin
    lua_pushstring(L, s[i]);
    lua_rawseti(L, -2, i);
  end;

  s.Free;

  Result := 1;
end;

function FolderGetCurrent(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, GetCurrentDirUTF8);
  Result := 1;
end;

function FolderRename(L: Plua_State): integer; cdecl;
begin
  RenameFileUTF8(lua_tostring(L, -2), lua_tostring(L, -1));
  Result := 0;
end;

function FolderSetCurrent(L: Plua_State): integer; cdecl;
begin
  SetCurrentDirUTF8(lua_tostring(L, -1));
  Result := 0;
end;

end.

