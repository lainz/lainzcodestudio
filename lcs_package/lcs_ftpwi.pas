unit lcs_ftpwi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ftpsend, Lua53;

procedure RegisterFTPWI(L: Plua_State);

function FTPWIDownload(L: Plua_State): integer; cdecl;
function FTPWIGetFileSize(L: Plua_State): integer; cdecl;

implementation

const
  FTPScheme = 'ftp://';

procedure RegisterFTPWI(L: Plua_State);

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
  RegisterFunction('Download', @FTPWIDownload);
  RegisterFunction('GetFileSize', @FTPWIGetFileSize);
  lua_setglobal(L, 'FTPWI');
end;

function FTPWIDownload(L: Plua_State): integer; cdecl;
var
  Host, Source: string;
  FoundPos: integer;
  URL, Filename, Username, Password, Port: string;
begin
  URL := lua_tostring(L, -5);
  Filename := lua_tostring(L, -4);
  Username := lua_tostring(L, -3);
  Password := lua_tostring(L, -2);
  Port := lua_tostring(L, -1);

  // Strip out scheme info:
  if LeftStr(URL, length(FTPScheme)) = FTPScheme then
    URL := Copy(URL, length(FTPScheme) + 1, length(URL));

  // Crude parsing:
  FoundPos := pos('/', URL);
  Host := LeftStr(URL, FoundPos - 1);
  Source := Copy(URL, FoundPos + 1, Length(URL));

  FtpGetFile(Host, Port, Source, Filename, Username, Password);
  Result := 0;
end;

function FTPWIGetFileSize(L: Plua_State): integer; cdecl;
var
  Host, Source: string;
  FoundPos: integer;
  URL, User, Pass, Port: string;
  VSize: int64 = -1;
begin
  URL := lua_tostring(L, -4);
  User := lua_tostring(L, -3);
  Pass := lua_tostring(L, -2);
  Port := lua_tostring(L, -1);

  // Strip out scheme info:
  if LeftStr(URL, length(FTPScheme)) = FTPScheme then
    URL := Copy(URL, length(FTPScheme) + 1, length(URL));

  // Crude parsing:
  FoundPos := pos('/', URL);
  Host := LeftStr(URL, FoundPos - 1);
  Source := Copy(URL, FoundPos + 1, Length(URL));

  with TFTPSend.Create do
    try
      if User <> '' then
      begin
        Username := User;
        Password := Pass;
      end;
      TargetHost := Host;
      TargetPort := Port;
      if not Login then
        Exit;
      VSize := FileSize(Source);
      Logout;
    finally
      Free;
    end;
  lua_pushinteger(L, VSize);
  Result := 1;
end;

end.
