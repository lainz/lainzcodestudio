unit umain;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Lua53, SynHighlighterLua, SynEdit;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBuildAndRun: TButton;
    ListBox1: TListBox;
    SynEdit1: TSynEdit;
    SynLuaSyn1: TSynLuaSyn;
    procedure btnBuildAndRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  lcs_registerall;

{$R *.lfm}

{ TfrmMain }

function Alloc({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  try
    Result := ptr;
    ReallocMem(Result, nSize);
  except
    Result := nil;
  end;
end;

function print(L: Plua_State): integer; cdecl;
begin
  frmMain.ListBox1.Items.Add(lua_tostring(L, -1));
  Result := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SynEdit1.Font.Height := Canvas.GetTextHeight('Fpc');
  ListBox1.Font.Height := Canvas.GetTextHeight('Fpc');
  SynLuaSyn1.ActiveDot := True;
end;

procedure TfrmMain.btnBuildAndRunClick(Sender: TObject);
var
  L: Plua_State;
  s: string;
  err: boolean;
begin
  ListBox1.Clear;
  L := lua_newstate(@alloc, nil);
  try
    luaL_openlibs(L);
    lua_register(L, 'print', @print);
    RegisterAll(L);
    s := SynEdit1.Text;
    err := (luaL_loadbuffer(L, PChar(s), Length(s), 'Lainz Code Studio') <> 0) or
      (lua_pcall(L, 0, 0, 0) <> 0);
    if err then
    begin
      ListBox1.Items.Add(lua_tostring(L, -1));
    end;
  finally
    lua_close(L);
  end;
end;

end.
