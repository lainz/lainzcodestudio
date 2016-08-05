unit lcs_dialog;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, Controls, Lua53;

procedure RegisterDialog(L: Plua_State);

function DialogInput(L: Plua_State): integer; cdecl;

implementation

uses
  lcs_dialog_input;

procedure RegisterDialog(L: Plua_State);

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
  RegisterFunction('Input', @DialogInput);
  lua_setglobal(L, 'Dialog');
end;

function DialogInput(L: Plua_State): integer; cdecl;
begin
  frm_lcs_dialog_input.Caption := lua_tostring(L, -3);
  frm_lcs_dialog_input.lblPrompt.Caption := lua_tostring(L, -2);
  frm_lcs_dialog_input.editText.Text := lua_tostring(L, -1);
  frm_lcs_dialog_input.ActiveDefaultControl := frm_lcs_dialog_input.editText;
  case frm_lcs_dialog_input.ShowModal of
    mrOk: lua_pushstring(L, frm_lcs_dialog_input.editText.Text);
    mrCancel: lua_pushstring(L, 'CANCEL');
  end;
  Result := 1;
end;

end.

