unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnRun: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SynEdit1: TSynEdit;
    SynLuaSyn1: TSynLuaSyn;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure SynEditCutToClipboard(Sender: TObject);
    procedure SynEditSelectAll(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure SynEditCopyToClipboard(Sender: TObject);
    procedure SynEditPasteFromClipboard(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { private declarations }
    FileName: string;
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

procedure TfrmMain.MenuItem12Click(Sender: TObject);
begin
  MenuItem13.Enabled := SynEdit1.SelAvail;
  MenuItem14.Enabled := SynEdit1.SelAvail;
end;

procedure TfrmMain.MenuItem18Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('if () then' + LineEnding + LineEnding +
    'else' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem19Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('while () do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem20Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('repeat' + LineEnding + LineEnding + 'until');
end;

procedure TfrmMain.MenuItem21Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('for i=0,10,1 do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem22Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('for i,v in ipairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem23Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('for k in pairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem24Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('function f()' + LineEnding + '  return' +
    LineEnding + 'end');
end;

procedure TfrmMain.MenuItem25Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('--');
end;

procedure TfrmMain.MenuItem26Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('--[[' + LineEnding + ']]');
end;

procedure TfrmMain.MenuItem27Click(Sender: TObject);
begin
  SynEdit1.InsertTextAtCaret('if () then' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem28Click(Sender: TObject);
begin
  if FileName <> '' then
  begin
    SynEdit1.Lines.SaveToFile(FileName);
  end
  else
  begin
    if SaveDialog1.Execute then
    begin
      FileName := SaveDialog1.FileName;
      SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
    end;
  end;
end;

procedure TfrmMain.SynEditCutToClipboard(Sender: TObject);
begin
  SynEdit1.CutToClipboard;
end;

procedure TfrmMain.SynEditSelectAll(Sender: TObject);
begin
  SynEdit1.SelectAll;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmMain.MenuItem3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FileName := SaveDialog1.FileName;
    SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TfrmMain.MenuItem5Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmMain.MenuItem7Click(Sender: TObject);
begin
  OpenURL('https://github.com/lainz/lainzcodestudio/wiki');
end;

procedure TfrmMain.SynEditCopyToClipboard(Sender: TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TfrmMain.SynEditPasteFromClipboard(Sender: TObject);
begin
  SynEdit1.PasteFromClipboard;
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem8.Enabled := SynEdit1.SelAvail;
  MenuItem10.Enabled := SynEdit1.SelAvail;
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
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
