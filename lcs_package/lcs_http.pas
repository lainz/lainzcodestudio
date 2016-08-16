unit lcs_http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Lua53;

procedure RegisterHTTP(L: Plua_State);

function HTTPDownload(L: Plua_State): integer; cdecl;
function HTTPGetFileSize(L: Plua_State): integer; cdecl;

implementation

procedure RegisterHTTP(L: Plua_State);

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
  RegisterFunction('Download', @HTTPDownload);
  RegisterFunction('DownloadSecure', @HTTPDownload);
  RegisterFunction('GetFileSize', @HTTPGetFileSize);
  RegisterFunction('GetFileSizeSecure', @HTTPGetFileSize);
  lua_setglobal(L, 'HTTP');
end;

function HTTPDownload(L: Plua_State): integer; cdecl;
var
  client: TFPHTTPClient;
  url, filename, username, password: string;
begin
  client := TFPHTTPClient.Create(nil);
  url := lua_tostring(L, -4);
  filename := lua_tostring(L, -3);
  username := lua_tostring(L, -2);
  password := lua_tostring(L, -1);
  if username <> '' then
    client.UserName := username;
  if password <> '' then
    client.Password := password;
  client.AllowRedirect := True;
  try
    client.Get(url, filename);
  finally
    client.Free;
  end;
  Result := 0;
end;

function HTTPGetFileSize(L: Plua_State): integer; cdecl;
var
  client: TFPHTTPClient;
  s, url, username, password: string;
  i: integer;
  VSize: int64 = 0;
begin
  client := TFPHTTPClient.Create(nil);
  url := lua_tostring(L, -3);
  username := lua_tostring(L, -2);
  password := lua_tostring(L, -1);
  if username <> '' then
    client.UserName := username;
  if password <> '' then
    client.Password := password;
  client.AllowRedirect := True;
  try
    client.HTTPMethod('HEAD', url, nil, [200]);
    for i := 0 to pred(client.ResponseHeaders.Count) do
    begin
      s := UpperCase(client.ResponseHeaders[i]);
      if Pos('CONTENT-LENGTH:', s) > 0 then
      begin
        VSize := StrToIntDef(Copy(s, Pos(':', s) + 1, Length(s)), -1);
        Break;
      end;
    end;
  finally
    client.Free;
  end;
  lua_pushinteger(L, VSize);
  Result := 1;
end;

end.

