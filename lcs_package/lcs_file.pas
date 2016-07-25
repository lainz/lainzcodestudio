unit lcs_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Process, UTF8Process, LCLIntF, Lua53;

procedure RegisterFile(L: Plua_State);

function FileDoesExist(L: Plua_State): integer; cdecl;
function FileGetShortName(L: Plua_State): integer; cdecl;
function FileOpenEmail(L: Plua_State): integer; cdecl;
function FileOpenURL(L: Plua_State): integer; cdecl;
function FilePrint(L: Plua_State): integer; cdecl;
function FileRun(L: Plua_State): integer; cdecl;

implementation

{$IFDEF WINDOWS}
uses Windows;

{$ENDIF}

procedure RegisterFile(L: Plua_State);

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
  RegisterFunction('DoesExist', @FileDoesExist);
  RegisterFunction('GetShortName', @FileGetShortName);
  RegisterFunction('ExploreFolder', @FileOpenURL);
  RegisterFunction('Open', @FileOpenURL);
  RegisterFunction('OpenEmail', @FileOpenEmail);
  RegisterFunction('OpenURL', @FileOpenURL);
  RegisterFunction('Print', @FilePrint);
  RegisterFunction('Run', @FileRun);
  lua_setglobal(L, 'File');
end;

function FileDoesExist(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, FileExistsUTF8(lua_tostring(L, -1)));
  Result := 1;
end;

function FileGetShortName(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, ExtractShortPathNameUTF8(lua_tostring(L, -1)));
  Result := 1;
end;

function FileOpenEmail(L: Plua_State): integer; cdecl;
begin
  OpenURL('mailto:' + lua_tostring(L, -1));
  Result := 0;
end;

function FileOpenURL(L: Plua_State): integer; cdecl;
begin
  OpenURL(lua_tostring(L, -1));
  Result := 0;
end;

function FilePrint(L: Plua_State): integer; cdecl;
{$IFDEF WINDOWS}
var
  s: WideString;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  s := lua_tostring(L, -1);
  ShellExecuteW(0, PWideChar('print'), PWideChar(s), PWideChar(''),
    PWideChar(''), SW_SHOWNORMAL);
  {$ENDIF}
  Result := 0;
end;

function FileRun(L: Plua_State): integer; cdecl;
var
  process: TProcessUTF8;
begin
  process := TProcessUTF8.Create(nil);
  process.CurrentDirectory := lua_tostring(L, -3);
  process.Parameters.Delimiter := ' ';
  process.Parameters.DelimitedText := (lua_tostring(L, -4));
  process.Executable := lua_tostring(L, -5);

  case lua_tointeger(L, -2) of
    0: process.ShowWindow := swoHide;
    1: process.ShowWindow := swoShowNormal;
    3: process.ShowWindow := swoShowMaximized;
    6: process.ShowWindow := swoShowMinimized;
  end;

  if lua_toboolean(L, -1) then
    process.Options := [poWaitOnExit];
  process.Execute;
  if process.WaitOnExit then
    lua_pushinteger(L, process.ExitCode)
  else
    lua_pushinteger(L, 0);
  process.Free;
  Result := 1;
end;

end.
