unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, Menus, Dialogs, Lua53, SynHighlighterLua, SynEdit,
  LCLIntF, Controls, SynGutterBase, SynEditMarks, SynEditMarkupSpecialLine,
  Graphics, ActnList, Buttons, uwatches, ustack;

type
  TScriptState = (ssRunning, ssPaused, ssStepInto, ssStepOver, ssFreeRun);
  TScriptStates = set of TScriptState;
  TScriptDbgStates = set of ssStepInto..ssFreeRun;

  { TfrmMain }

  TfrmMain = class(TForm)
    actFreeRun: TAction;
    actRefreshWatches: TAction;
    actShowStack: TAction;
    actShowWatches: TAction;
    actToggleBkpt: TAction;
    actWatch: TAction;
    actStop: TAction;
    actPause: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actRun: TAction;
    ActionList1: TActionList;
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
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
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
    procedure actRefreshWatchesExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actShowStackExecute(Sender: TObject);
    procedure actShowWatchesExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actToggleBkptExecute(Sender: TObject);
    procedure actWatchExecute(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    function GetVarContents(AId: String): String;
    function LuaVarToString(L: Plua_State): String;
    function GetStackContents(L: Plua_State; AVarArgs, ATemps: Boolean): String;
    procedure RefreshWatches;
    procedure RefreshStack;
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
  // Identifier characters
  ID_FIRST = ['A'..'Z', 'a'..'z', '_'];
  ID_SYMBOL = ID_FIRST + ['0'..'9'];
  ID_DELIMITERS = [#9..#127] - ID_SYMBOL;

  // Special line colors
  FG_ACTIVE = clWhite;
  BG_ACTIVE = clBlue;
  FG_BKPT = clWhite;
  BG_BKPT = clRed;
  FG_ACTIVE_ON_BKPT = clWhite;
  BG_ACTIVE_ON_BKPT = clMaroon;

  PRINT_SEP = ' '; // (or ''?) print() separator
  MAX_TABLE_N = 32; // Max table elements to show

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
  MustYield, AtBkpt: Boolean;
begin
  Application.ProcessMessages;
  MustYield := False;
  case ar^.event of
    LUA_HOOKCALL:
      Inc(Script.CallDepth);
    LUA_HOOKRET:
      Dec(Script.CallDepth);
    LUA_HOOKLINE:
      begin
        Script.SrcLine := ar^.currentline - Script.LOfs;
        MustYield := Script.StopRq or Script.ResetRq;
        if
          not MustYield and
          not (ssFreeRun in Script.State) and
          (Script.SrcLine > 0)
        then
        begin
          AtBkpt := frmMain.HasBkptAtLine(Script.SrcLine);
          if (ssStepOver in Script.State) then
            MustYield := (Script.CallDepth <= Script.ReqDepth) or AtBkpt
          else
            MustYield := (ssStepInto in Script.State) or AtBkpt;
        end;
      end;
  end;
  if MustYield and lua_isyieldable(L) then
    lua_yield(L, 0);
end;

function StackToStr(L: Plua_State; ASep: String): String;
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
      S := S + ASep + Si;
  end;
  Result := S;
end;

function print(L: Plua_State): Integer; cdecl;
begin
  frmMain.ListBox1.Items.AddText(StackToStr(L, PRINT_SEP));
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
  if Mark <> Nil then
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
  actPause.Enabled := Running and not Paused;
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
  Script.CallDepth := 0;
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
  RefreshWatches;
  RefreshStack;
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

function TfrmMain.GetVarContents(AId: String): String;

  function EvaLua(L: Plua_State; AExp: String): Integer;
  begin
    luaL_loadstring(L, PChar('return ' + AExp));
    lua_pcall(L, 0, 1, 0);
    Result := lua_type(L, -1);
  end;

begin
  Result := '';
  if not (ssRunning in Script.State) then
    Exit;
  EvaLua(Script.L, AId);
  try
    Result := LuaVarToString(Script.L);
  finally
    lua_pop(Script.L, 1);
  end;
end;

function TfrmMain.LuaVarToString(L: Plua_State): String;
var
  T: Integer;
  S: String;

  function AddQuoted(S: String): String;
  var
    C: Char;
  begin
    Result := '"';
    for C in S do
      if C in [#0, #7, #8, #9, #10, #11, #12, #13, '"', '''', '\'] then
        case C of
           #0: Result := Result + '\0';
           #7: Result := Result + '\a';
           #8: Result := Result + '\b';
           #9: Result := Result + '\t';
          #10: Result := Result + '\n';
          #11: Result := Result + '\v';
          #12: Result := Result + '\f';
          #13: Result := Result + '\r';
        otherwise
          Result := Result + '\' + C;
        end
      else if C < ' ' then
        Result := Result + '\' + RightStr('000' + IntToStr(Ord(C)), 3)
      else
        Result := Result + C;
    Result := Result + '"';
  end;

  function TblToString(L: Plua_State; V: Integer): String;
  var
    N: Integer;
  begin
    Result := '{';
    N := 0;
    lua_pushnil(L);
    while lua_next(L, -2) <> 0 do
      try
        if N < 1 then
        else if N < MAX_TABLE_N then
          Result := Result + ', '
        else
        begin // do not print after n-th element
          Result := Result + ', ...';
          lua_pop(L, 1);
          Break;
        end;
        Inc(N);

        lua_pushvalue(L, -2);
        try
          Result := Result + lua_tostring(L, -1) + ' => ';
        finally
          lua_pop(L, 1);
        end;
        if lua_istable(L, -1) then
          Result := Result + TblToString(L, V + 1)
        else if lua_isstring(L, -1) then
          Result := Result + AddQuoted(lua_tostring(L, -1))
        else
          Result := Result + lua_tostring(L, -1);

      finally
        lua_pop(L, 1);
      end;
    Result := Result + '}';
  end;

begin
  Result := '';
  T := lua_type(L, -1);
  case T of
    LUA_TSTRING:
      S := AddQuoted(lua_tostring(L, -1));
    LUA_TNUMBER:
      S := lua_tostring(L, -1);
    LUA_TNIL:
      S := 'nil';
    LUA_TBOOLEAN:
      if lua_toboolean(L, -1) then
        S := 'true' else
        S := 'false';
    LUA_TTABLE:
        S := TblToString(L, 1);
    otherwise
      S := '(' + lua_typename(L, T) + ')';
  end;
  Result := S;
end;

function TfrmMain.GetStackContents(L: Plua_State; AVarArgs, ATemps: Boolean
  ): String;
var
  ar: lua_Debug;
  I: Integer;
  LName: String;
  PLName: PChar;

  procedure L1(AInc: Integer; Temps: Boolean);
  begin
    I := AInc;
    while True do
    begin
      PLName := lua_getlocal(L, @ar, I);
      if PLName = Nil then
        Break
      else
        try
          LName := StrPas(PLName);
          if not Temps and (LName[1] = '(') then
            Continue;
          Result := Result +
            LName + ' = ' + LuaVarToString(L) + LineEnding;
        finally
          I := I + AInc;
          lua_pop(Script.Lt, 1);
        end;
    end;
  end;

begin
  Result := '';
  if not (ssRunning in Script.State) then
    Exit;
  if lua_getstack(L, 0, @ar) <> 1 then
    Exit;
  if AVarArgs then
    L1(-1, True);
  L1(1, ATemps);
end;

procedure TfrmMain.RefreshWatches;
var
  SID, Cont: String;
  I: Integer;
begin
  with frmWatches do
    for I := 0 to Pred(moWatches.Lines.Count) do
    begin
      SID := Trim(ExtractWord(1, moWatches.Lines[I], ['=']{ID_DELIMITERS}));
      if (SID = '') or not (SID[1] in ID_FIRST) then
        Continue;
      if (ssRunning in Script.State) then
        Cont := GetVarContents(SID) else
        Cont := '(not running)';
      moWatches.Lines[I] := SID + ' = ' + Cont;
    end;
end;

procedure TfrmMain.RefreshStack;
begin
  with frmStack do
    moStack.Text := GetStackContents(Script.Lt, True, False);
end;

procedure TfrmMain.actRunExecute(Sender: TObject);
begin
  DoRun([]);
end;

procedure TfrmMain.actRunUpdate(Sender: TObject);
begin

end;

procedure TfrmMain.actShowStackExecute(Sender: TObject);
begin
  if frmStack.Visible then
    frmStack.Hide else frmStack.Show;
  actShowStack.Checked := frmStack.Visible;
end;

procedure TfrmMain.actShowWatchesExecute(Sender: TObject);
begin
  if frmWatches.Visible then
    frmWatches.Hide else frmWatches.Show;
  actShowWatches.Checked := frmWatches.Visible;
end;

procedure TfrmMain.actFreeRunExecute(Sender: TObject);
begin
  DoRun([ssFreeRun]);
end;

procedure TfrmMain.actPauseExecute(Sender: TObject);
begin
  Script.StopRq := True;
end;

procedure TfrmMain.actRefreshWatchesExecute(Sender: TObject);
begin
  RefreshWatches;
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

procedure TfrmMain.actToggleBkptExecute(Sender: TObject);
begin
  ToggleBreakpoint(Editor.CaretY);
end;

procedure TfrmMain.actWatchExecute(Sender: TObject);
var
  S, SID, Cont: String;
  I: Integer = 0;
begin
  S := Trim(Editor.SelText);
  if S = '' then
  begin
    S := Trim(InputBox('Watch', 'Variable', ''));
  end;
  if S = '' then
    Exit;
  for I := 1 to 10 do
  begin
    SID := ExtractWord(I, S, ID_DELIMITERS);
    if SID = '' then
      Break;
    if not (SID[1] in ID_FIRST) then
      Continue;
    Cont := GetVarContents(SID);
    if not frmWatches.Visible then
      actShowWatches.Execute;
    frmWatches.moWatches.Lines.Add(SID + ' = ' + Cont);
  end;
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

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ssRunning in Script.State then
    ScriptFinalize(0);
end;

end.
