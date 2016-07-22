unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBuildAndRun: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    SynLuaSyn1: TSynLuaSyn;
    procedure btnBuildAndRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
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
  frmMain.ListBox1.Items.AddText(lua_tostring(L, -1));
  Result := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SynEdit1.Font.Height := Canvas.GetTextHeight('Fpc');
  ListBox1.Font.Height := Canvas.GetTextHeight('Fpc');
  SynLuaSyn1.ActiveDot := True;
  Caption := Application.Title;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

procedure TfrmMain.MenuItem3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TfrmMain.MenuItem5Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.MenuItem7Click(Sender: TObject);
begin
  OpenURL('https://github.com/lainz/lainzcodestudio/wiki');
end;

procedure TfrmMain.btnBuildAndRunClick(Sender: TObject);
var
  L: Plua_State;
  S: TStringList;
  err: boolean;
begin
  ListBox1.Clear;
  L := lua_newstate(@alloc, nil);
  try
    luaL_openlibs(L);
    lua_register(L, 'print', @print);
    S := TStringList.Create;
    RegisterAll(L, S);
    S.Add(SynEdit1.Text);
    err := (luaL_loadbuffer(L, PChar(S.Text), Length(S.Text), 'Lainz Code Studio') <>
      0) or (lua_pcall(L, 0, 0, 0) <> 0);
    if err then
    begin
      ListBox1.Items.Add(lua_tostring(L, -1));
    end;
  finally
    S.Free;
    lua_close(L);
  end;
end;

end.
