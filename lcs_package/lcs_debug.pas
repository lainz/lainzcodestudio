unit lcs_debug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua53;

procedure RegisterDebug(L: Plua_State);

function DebugClear(L: Plua_State): integer; cdecl;
function DebugPrint(L: Plua_State): integer; cdecl;
function DebugSendToFile(L: Plua_State): integer; cdecl;
function DebugShowWindow(L: Plua_State): integer; cdecl;

implementation

uses
  lcs_debugform;

procedure RegisterDebug(L: Plua_State);

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
  RegisterFunction('Clear', @DebugClear);
  RegisterFunction('Print', @DebugPrint);
  RegisterFunction('SendToFile', @DebugSendToFile);
  RegisterFunction('ShowWindow', @DebugShowWindow);
  lua_setglobal(L, 'Debug');
end;

function DebugClear(L: Plua_State): integer; cdecl;
begin
  frmDebug.memoDebug.Clear;
  Result := 0;
end;

function DebugPrint(L: Plua_State): integer; cdecl;
var
  Text: string;
begin
  Text := lua_tostring(L, -1);
  frmDebug.memoDebug.Lines.AddText(Text);
  Result := 0;
end;

function DebugSendToFile(L: Plua_State): integer; cdecl;
var
  filename: string;
  overwrite: boolean;
  s: TStringList;
begin
  filename := lua_tostring(L, -2);
  overwrite := lua_toboolean(L, -1);
  if overwrite then
    frmDebug.memoDebug.Lines.SaveToFile(filename)
  else
  begin
    s := TStringList.Create;
    s.LoadFromFile(filename);
    s.AddText(frmDebug.memoDebug.Lines.Text);
    s.SaveToFile(filename);
    s.Free;
  end;
  Result := 0;
end;

function DebugShowWindow(L: Plua_State): integer; cdecl;
var
  Show: boolean;
begin
  Show := lua_toboolean(L, -1);
  if Show then
    frmDebug.Show
  else
    frmDebug.Close;
  Result := 0;
end;

end.

