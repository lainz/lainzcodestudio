unit lcs_http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Lua53;

procedure RegisterHTTP(L: Plua_State);

function HTTPDownload(L: Plua_State): integer; cdecl;
function HTTPGetFileSize(L: Plua_State): integer; cdecl;

type

  { THTTPGetFileSize }

  THTTPGetFileSize = class
  public
    constructor Create(L: Plua_State);
    procedure DoDataReceived(Sender: TObject; const ContentLength, CurrentPos: int64);
  end;

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
  GetFileSize: THTTPGetFileSize;
begin
  GetFileSize := THTTPGetFileSize.Create(L);
  GetFileSize.Free;
  Result := 1;
end;

{ THTTPGetFileSize }

constructor THTTPGetFileSize.Create(L: Plua_State);
var
  client: TFPHTTPClient;
  s, url, username, password: string;
  i: integer;
  VSize: int64 = -1;
  LStringStream: TStringStream;
begin
  LStringStream := TStringStream.Create('');
  try
    client := TFPHTTPClient.Create(nil);
    url := lua_tostring(L, -3);
    username := lua_tostring(L, -2);
    password := lua_tostring(L, -1);
    if username <> '' then
      client.UserName := username;
    if password <> '' then
      client.Password := password;
    client.AllowRedirect := True;
    client.OnDataReceived := @DoDataReceived;
    client.ResponseHeaders.NameValueSeparator := ':';
    try
      client.HTTPMethod('GET', url, LStringStream, []);
    except
    end;
    VSize := StrToIntDef(client.ResponseHeaders.Values['CONTENT-LENGTH'], -1);
  finally
    client.Free;
    LStringStream.Free;
  end;
  lua_pushinteger(L, VSize);
end;

procedure THTTPGetFileSize.DoDataReceived(Sender: TObject;
  const ContentLength, CurrentPos: int64);
begin
  if ContentLength > 0 then
  begin
    Abort;
  end;
end;

end.
