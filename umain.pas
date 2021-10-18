unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF, Controls, SynGutterBase, SynEditMarks, SynEditMarkupSpecialLine,
  Graphics, ActnList, Buttons;

type
  TScriptState = (ssRunning, ssPaused, ssStepInto, ssStepOver, ssFreeRun);
  TScriptStates = set of TScriptState;
  TScriptDbgStates = set of ssStepInto..ssFreeRun;

  { TfrmMain }

  TfrmMain = class(TForm)
    actFreeRun: TAction;
    actStop: TAction;
    actPause: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    btnRun: TButton;
    ImageList2: TImageList;
    lblScriptState: TLabel;
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
    Editor: TSynEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SynLuaSyn1: TSynLuaSyn;
    procedure actFreeRunExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure EditorGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
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

    procedure ToggleBreakpoint(Line: LongInt);
    procedure ScriptFinalize(AThrStat: Integer);
    procedure CaretPos(ALine, ACol: LongInt; ACenter: Boolean = True);
    procedure ShowScriptState;

    function BkptAtLine(ALine: LongInt): TSynEditMark;
    function HasBkptAtLine(ALine: LongInt): Boolean;

    function DoCompile: Boolean;
    procedure DoRun(AStep: TScriptDbgStates);
    procedure DoResume(AStep: TScriptDbgStates);
    procedure DoStop(AReset: Boolean);
    procedure ShowError(AErrorMsg: String);
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  lcs_registerall, Contnrs, SysUtils, Math, StrUtils;

{$R *.lfm}

const
  // Special line colors
  FG_ACTIVE = clWhite;
  BG_ACTIVE = clBlue;
  FG_BKPT = clWhite;
  BG_BKPT = clRed;
  FG_ACTIVE_ON_BKPT = clWhite;
  BG_ACTIVE_ON_BKPT = clMaroon;


var
  // Just for the scope
  Script: record
    State: TScriptStates;
    StopRq, ResetRq: Boolean;
    L, Lt: Plua_State;
    LOfs, SrcLine, CallDepth, ReqDepth: LongInt;
    S: TStringList;
  end;

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

procedure DbgHook(L: Plua_State; ar: Plua_Debug); cdecl;
var
  MustYield: Boolean;
begin
  MustYield := False;
  case ar^.event of
    LUA_HOOKCALL:
      Inc(Script.CallDepth);
    LUA_HOOKRET:
      Dec(Script.CallDepth);
    LUA_HOOKLINE:
      begin
        Application.ProcessMessages;
        Script.SrcLine := ar^.currentline - Script.LOfs;
        MustYield := Script.StopRq or Script.ResetRq;
        if
          not MustYield and
          not (ssFreeRun in Script.State) and
          (Script.SrcLine > 0)
        then
        begin
          if (ssStepOver in Script.State) then
            MustYield := (Script.CallDepth <= Script.ReqDepth)
          else if (ssStepInto in Script.State) or
              frmMain.HasBkptAtLine(Script.SrcLine)
            then
              MustYield := True;
        end;
      end;
  end;
  if MustYield and lua_isyieldable(L) then
    lua_yield(L, 0);
end;

function print(L: Plua_State): integer; cdecl;
const
  Sep = ' '; // or ''?
var
  I, N, T: Integer;
  S, Si: String;
begin
  S := '';
  N := lua_gettop(L);
  for I := 1 to N do
  begin
    T := lua_type(L, I);
    case T of
      LUA_TSTRING, LUA_TNUMBER:
        Si := lua_tostring(L, I);
      LUA_TNIL:
        Si := 'nil';
      LUA_TBOOLEAN:
        if lua_toboolean(L, I) then
          Si := 'true' else
          Si := 'false';
      otherwise
    	Si := '(' + lua_typename(L, T) + ')';
    end;
    if S = '' then
      S := Si else
      S := S + Sep + Si;
  end;
  frmMain.ListBox1.Items.AddText(S);
  Result := 0;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Editor.Font.Height := Canvas.GetTextHeight('Fpc');
  ListBox1.Font.Height := Canvas.GetTextHeight('Fpc');
  SynLuaSyn1.ActiveDot := True;
  Caption := Application.Title;
  ShowScriptState;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
end;

procedure TfrmMain.MenuItem12Click(Sender: TObject);
begin
  MenuItem13.Enabled := Editor.SelAvail;
  MenuItem14.Enabled := Editor.SelAvail;
end;

procedure TfrmMain.MenuItem18Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('if () then' + LineEnding + LineEnding +
    'else' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem19Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('while () do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem20Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('repeat' + LineEnding + LineEnding + 'until');
end;

procedure TfrmMain.MenuItem21Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for i=0,10,1 do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem22Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for i,v in ipairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem23Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('for k in pairs() do' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem24Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('function f()' + LineEnding + '  return' +
    LineEnding + 'end');
end;

procedure TfrmMain.MenuItem25Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('--');
end;

procedure TfrmMain.MenuItem26Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('--[[' + LineEnding + ']]');
end;

procedure TfrmMain.MenuItem27Click(Sender: TObject);
begin
  Editor.InsertTextAtCaret('if () then' + LineEnding + LineEnding + 'end');
end;

procedure TfrmMain.MenuItem28Click(Sender: TObject);
begin
  if FileName <> '' then
  begin
    Editor.Lines.SaveToFile(FileName);
  end
  else
  begin
    if SaveDialog1.Execute then
    begin
      FileName := SaveDialog1.FileName;
      Editor.Lines.SaveToFile(SaveDialog1.FileName);
    end;
  end;
end;

procedure TfrmMain.EditorGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
begin
  ToggleBreakpoint(Line);
end;

procedure TfrmMain.SynEditCutToClipboard(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TfrmMain.SynEditSelectAll(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    Editor.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmMain.MenuItem3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FileName := SaveDialog1.FileName;
    Editor.Lines.SaveToFile(SaveDialog1.FileName);
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
  Editor.CopyToClipboard;
end;

procedure TfrmMain.SynEditPasteFromClipboard(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem8.Enabled := Editor.SelAvail;
  MenuItem10.Enabled := Editor.SelAvail;
end;

procedure TfrmMain.ToggleBreakpoint(Line: LongInt);
var
  Mark: TSynEditMark;
begin
  Mark := BkptAtLine(Line);
  if Mark <> Nil   then
    repeat
      Editor.Marks.Remove(Mark);
      Mark.Free;
      Mark := BkptAtLine(Line);
    until Mark = Nil
  else
  begin
    Mark := TSynEditMark.Create(Editor);
    Mark.Line := Line;
    Mark.BookmarkNumber := -99; // brkpt
    Mark.ImageList := ImageList2;
    Mark.ImageIndex := 10;
    Mark.Visible := True;
    Editor.Marks.Add(Mark);
  end;
  Editor.Refresh;
end;

procedure TfrmMain.ScriptFinalize(AThrStat: Integer);
begin
  Script.State := [];
  Script.StopRq := False;
  Script.ResetRq := False;
  if LUA_YIELD_ < AThrStat then
    ShowError(lua_tostring(Script.Lt, -1));
  Script.SrcLine := -1;
  lua_close(Script.L);
  Script.S.Free;
  ShowScriptState;
  Editor.Refresh;
end;

procedure TfrmMain.CaretPos(ALine, ACol: LongInt; ACenter: Boolean);
begin
  if ACenter then
    if (ALine < Editor.TopLine + 2) or
      (ALine > Editor.TopLine + Editor.LinesInWindow - 2)
    then
      Editor.TopLine := ALine - (Editor.LinesInWindow div 2);
  Editor.CaretY := ALine;
  Editor.CaretX := ACol;
end;

procedure TfrmMain.ShowScriptState;
var
  Running, Paused, FreeRun: Boolean;
begin
  Running := (ssRunning in Script.State);
  Paused := (ssPaused in Script.State);
  FreeRun := (ssFreeRun in Script.State);

  actRun.Enabled := not Running or Paused;
  actFreeRun.Enabled := not Running or Paused;
  actPause.Enabled := Running or not Paused;
  actStop.Enabled := Running;

  actStepInto.Enabled := not FreeRun;
  actStepOver.Enabled := not FreeRun;

  if Paused then
    lblScriptState.Caption := Format('Paused at line %d', [script.SrcLine])
  else if FreeRun then
    lblScriptState.Caption := 'Running (no brkpt)...'
  else if Running then
    lblScriptState.Caption := 'Running...'
  else
    lblScriptState.Caption := 'Not running';
end;

function TfrmMain.BkptAtLine(ALine: LongInt): TSynEditMark;
var
  Marks: TSynEditMarkLine;
  Mark: TSynEditMark;
  I: Integer;
begin
  Result := Nil;
  Marks := Editor.Marks.Line[ALine];
  if Assigned(Marks) then
    for I := 0 to Pred(Marks.Count) do
    begin
      Mark := Marks[I];
      if Mark.BookmarkNumber < 0 then
      begin
        Result := Mark;
        Break;
      end;
    end;
end;

function TfrmMain.HasBkptAtLine(ALine: LongInt): Boolean;
begin
  Result := BkptAtLine(ALine) <> Nil;
end;

function TfrmMain.DoCompile: Boolean;
begin
  ListBox1.Clear;
  Script.L := lua_newstate(@alloc, nil);
  luaL_openlibs(Script.L);
  lua_register(Script.L, 'print', @print);
  Script.S := TStringList.Create;
  RegisterAll(Script.L, Script.S);
  Script.S.Text := Script.S.Text;  // split lines
  Script.LOfs := Script.S.Count; // offset of 1-st line
  Script.S.Add(Editor.Text);
  Script.Lt := lua_newthread(Script.L); // for yield/resume
  lua_sethook(Script.Lt, @DbgHook, LUA_MASKLINE + LUA_MASKCALL + LUA_MASKRET, 0);
  Result := (luaL_loadbuffer(Script.Lt, PChar(Script.S.Text),
    Length(Script.S.Text), 'Lainz Code Studio') = 0);
  if not Result then
    ShowError(lua_tostring(Script.Lt, -1)); // invalid source line
end;

procedure TfrmMain.DoRun(AStep: TScriptDbgStates);
begin
  if ssPaused in Script.State then
    DoResume(AStep)

  else if not(ssRunning in Script.State) then
  begin
    if DoCompile then
      DoResume(AStep);
  end;
end;

procedure TfrmMain.DoResume(AStep: TScriptDbgStates);
var
  stat: Integer;
begin
  Script.StopRq := False;
  Script.ResetRq := False;
  if ssStepOver in AStep then
    Script.ReqDepth := Max(1, Script.CallDepth);
  Script.State := AStep + [ssRunning];
  ShowScriptState;
  stat := lua_resume(Script.Lt, Script.Lt, 0);
  if stat = LUA_YIELD_ then
  begin
    Exclude(Script.State, ssFreeRun);
    Include(Script.State, ssPaused);
    CaretPos(Script.SrcLine, 1);

    ShowScriptState;

    if Script.StopRq or Script.ResetRq then
    begin
      Script.StopRq := False;
      if Script.ResetRq then
        ScriptFinalize(stat);
    end
  end
  else
    ScriptFinalize(stat);
  Editor.Refresh;
end;

procedure TfrmMain.DoStop(AReset: Boolean);
begin
  Script.StopRq := True;
  Script.ResetRq := AReset;
end;

procedure TfrmMain.ShowError(AErrorMsg: String);
begin
  ListBox1.Items.Add(Format('Line %d: ', [Script.SrcLine]) + AErrorMsg);
end;

procedure TfrmMain.btnRunClick(Sender: TObject);
begin
  DoRun([]);
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  DoRun([]);
end;

procedure TfrmMain.actRunUpdate(Sender: TObject);
begin

end;

procedure TfrmMain.actFreeRunExecute(Sender: TObject);
begin
  DoRun([ssFreeRun]);
end;

procedure TfrmMain.actPauseExecute(Sender: TObject);
begin
  Script.StopRq := True;
end;

procedure TfrmMain.actStepIntoExecute(Sender: TObject);
begin
  DoRun([ssStepInto]);
end;

procedure TfrmMain.actStepOverExecute(Sender: TObject);
begin
  DoRun([ssStepOver]);
end;

procedure TfrmMain.actStopExecute(Sender: TObject);
begin
  if ssPaused in Script.State then
    ScriptFinalize(0) else
    Script.ResetRq := True;
end;

procedure TfrmMain.EditorSpecialLineColors(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if HasBkptAtLine(Line) then
  begin
    Special := True;
    if Line = Script.SrcLine then
    begin
      FG := FG_ACTIVE_ON_BKPT;
      BG := BG_ACTIVE_ON_BKPT;
    end else
    begin
      FG := FG_BKPT;
      BG := BG_BKPT;
    end;
  end
  else if Line = Script.SrcLine then
  begin
    Special := True;
    FG := FG_ACTIVE; // clWhite;
    BG := BG_ACTIVE; // clBlue;
  end
  else
    Special := False;
end;

end.
