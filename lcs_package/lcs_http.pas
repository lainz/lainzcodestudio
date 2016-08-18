unit lcs_http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Lua53;

procedure RegisterHTTP(L: Plua_State);

function HTTPDownload(L: Plua_State): integer; cdecl;
function HTTPGetFileSize(L: Plua_State): integer; cdecl;
function HTTPSubmit(L: Plua_State): integer; cdecl;

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
  RegisterFunction('Submit', @HTTPSubmit);
  RegisterFunction('SubmitSecute', @HTTPSubmit);
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

function HTTPSubmit(L: Plua_State): integer; cdecl;
var
  client: TFPHTTPClient;
  method: integer;
  url, username, password, res: string;
  s: TStringList;
begin
  try
    s := TStringList.Create;
    client := TFPHTTPClient.Create(nil);
    url := lua_tostring(L, -5);
    method := lua_tointeger(L, -3);
    username := lua_tostring(L, -2);
    password := lua_tostring(L, -1);

    if username <> '' then
      client.UserName := username;
    if password <> '' then
      client.Password := password;
    client.AllowRedirect := True;
    client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');

    lua_pushnil(L);
    while (lua_next(L, -5) <> 0) do
    begin
      s.Add(lua_tostring(L, -2) + '=' + lua_tostring(L, -1));
      lua_pop(L, 1);
    end;

    s.QuoteChar := char('');
    s.Delimiter := '&';
    s.StrictDelimiter := True;
    res := '';

    case method of
      1:
      begin
        res := client.Get(url + '?' + s.DelimitedText);
      end;
      0:
      begin
        res := client.FormPost(url, s);
      end;
    end;

  finally
    client.Free;
    s.Free;
  end;
  lua_pushstring(L, res);
  Result := 1;
end;

{ THTTPGetFileSize }

constructor THTTPGetFileSize.Create(L: Plua_State);
var
  client: TFPHTTPClient;
  url, username, password: string;
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
