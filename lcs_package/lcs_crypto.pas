unit lcs_crypto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Blowfish, Lua53;

procedure RegisterCrypto(L: Plua_State);

function CryptoBlowfishDecrypt(L: Plua_State): integer; cdecl;
function CryptoBlowfishDecryptString(L: Plua_State): integer; cdecl;
function CryptoBlowfishEncrypt(L: Plua_State): integer; cdecl;
function CryptoBlowfishEncryptString(L: Plua_State): integer; cdecl;

implementation

procedure RegisterCrypto(L: Plua_State);

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
  RegisterFunction('BlowfishDecrypt', @CryptoBlowfishDecrypt);
  RegisterFunction('BlowfishDecryptString', @CryptoBlowfishDecryptString);
  RegisterFunction('BlowfishEncrypt', @CryptoBlowfishEncrypt);
  RegisterFunction('BlowfishEncryptString', @CryptoBlowfishEncryptString);
  lua_setglobal(L, 'Crypto');
end;

function CryptoBlowfishDecrypt(L: Plua_State): integer; cdecl;
var
  Source, destination, key: string;
  s1, s2: TMemoryStream;
  dc: TBlowfishDecryptStream;
begin
  Source := lua_tostring(L, -3);
  destination := lua_tostring(L, -2);
  key := lua_tostring(L, -1);

  s1 := TMemoryStream.Create;
  s1.LoadFromFile(Source);
  s2 := TMemoryStream.Create;

  dc := TBlowfishDecryptStream.Create(key, s1);
  s2.CopyFrom(dc, s1.Size);
  dc.Free;

  s2.SaveToFile(destination);
  s2.Free;
  s1.Free;

  Result := 0;
end;

function CryptoBlowfishDecryptString(L: Plua_State): integer; cdecl;
var
  Text, key: string;
  s1, s2: TStringStream;
  dc: TBlowfishDecryptStream;
begin
  Text := lua_tostring(L, -2);
  key := lua_tostring(L, -1);

  s1 := TStringStream.Create(Text);
  s2 := TStringStream.Create('');

  dc := TBlowfishDecryptStream.Create(key, s1);
  s2.CopyFrom(dc, s1.Size);
  dc.Free;

  lua_pushstring(L, s2.DataString);
  s2.Free;
  s1.Free;

  Result := 1;
end;

function CryptoBlowfishEncrypt(L: Plua_State): integer; cdecl;
var
  Source, destination, key: string;
  s1, s2: TMemoryStream;
  ec: TBlowfishEncryptStream;
begin
  Source := lua_tostring(L, -3);
  destination := lua_tostring(L, -2);
  key := lua_tostring(L, -1);

  s1 := TMemoryStream.Create;
  s1.LoadFromFile(Source);
  s2 := TMemoryStream.Create;

  ec := TBlowfishEncryptStream.Create(key, s2);
  ec.CopyFrom(s1, s1.Size);
  ec.Free;

  s2.SaveToFile(destination);
  s2.Free;
  s1.Free;

  Result := 0;
end;

function CryptoBlowfishEncryptString(L: Plua_State): integer; cdecl;
var
  Text, key: string;
  s1, s2: TStringStream;
  ec: TBlowfishEncryptStream;
begin
  Text := lua_tostring(L, -2);
  key := lua_tostring(L, -1);

  s1 := TStringStream.Create(Text);
  s2 := TStringStream.Create('');

  ec := TBlowfishEncryptStream.Create(key, s2);
  ec.CopyFrom(s1, s1.Size);
  ec.Free;

  lua_pushstring(L, s2.DataString);
  s2.Free;
  s1.Free;

  Result := 1;
end;

end.

