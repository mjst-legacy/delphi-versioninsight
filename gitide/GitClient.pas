{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is GitClient.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2011 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitClient;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections;

type
  TGitItem = class;
  TGitBlameItem = class;

  TGitHistoryItem = class(TObject)
  private
    FAuthor: string;
    FAuthorEmail: string;
    FBlameItems: TObjectList<TGitBlameItem>;
    FBody: string;
    FDate: TDateTime;
    FHash: string;
    FSubject: string;
    FParent: TGitItem;
    function GetBlameCount: Integer;
    function GetBlameItems(AIndex: Integer): TGitBlameItem;
  public
    constructor Create(AParent: TGitItem);
    destructor Destroy; override;
    function GetFile: TBytes;
    procedure LoadBlame;
    property Author: string read FAuthor;
    property AuthorEmail: string read FAuthorEmail;
    property BlameCount: Integer read GetBlameCount;
    property BlameItems[AIndex: Integer]: TGitBlameItem read GetBlameItems;
    property Body: string read FBody;
    property Date: TDateTime read FDate;
    property Hash: string read FHash;
    property Parent: TGitItem read FParent;
    property Subject: string read FSubject;
  end;

  TGitBlameItem = class(TObject)
  private
    FLineStr: string;
    FHistoryIndex: Integer;
    FHistoryItem: TGitHistoryItem;
  public
    property LineStr: string read FLineStr write FLineStr;
    //TODO: Remove HistoryIndex (-2 currently indicates a local change - should be replaced by a special TGitHistoryItem)
    property HistoryIndex: Integer read FHistoryIndex write FHistoryIndex;
    property HistoryItem: TGitHistoryItem read FHistoryItem write FHistoryItem;
  end;

  TGitStatus = (gsAdded, gsModified, gsNormal, gsUnknown);

  TGitClient = class;

  TGitItem = class(TObject)
  private
    FFileName: string;
    FGitClient: TGitClient;
    FHistoryItems: TObjectList<TGitHistoryItem>;
    FStatus: TGitStatus;
    function GetHistoryCount: Integer;
    function GetHistoryItems(AIndex: Integer): TGitHistoryItem;
  public
    constructor Create(AGitClient: TGitClient; const AFileName: string);
    destructor Destroy; override;
    procedure LoadHistory(AOnlyLast: Boolean = False);
    procedure LoadStatus;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[AIndex: Integer]: TGitHistoryItem read GetHistoryItems;
    property Status: TGitStatus read FStatus;
  end;

  TGitCloneCallBack = procedure(Sender: TObject; const AText: string; var Cancel: Boolean) of object;

  TGitClient = class(TObject)
  private
    FCancel: Boolean;
    FCloneCallBack: TGitCloneCallBack;
    FGitExecutable: string;
    procedure ExecuteTextHandler(const Text: string);
  public
    constructor Create;
    function Clone(const ASourcePath, ADestPath: string; ACallBack: TGitCloneCallBack = nil): Boolean;
    function IsPathInWorkingCopy(const APath: string): Boolean;
    function IsVersioned(const AFileName: string): Boolean;
    property GitExecutable: string read FGitExecutable write FGitExecutable;
  end;

implementation

//--- JclBase and JclSysUtils --------------------------------------------------
const
  // line delimiters for a version of Delphi/C++Builder
  NativeLineFeed       = Char(#10);
  NativeCarriageReturn = Char(#13);

function CharIsReturn(const C: Char): Boolean;
begin
  Result := (C = NativeLineFeed) or (C = NativeCarriageReturn);
end;

// memory initialization
procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

const
  ABORT_EXIT_CODE = {$IFDEF MSWINDOWS} ERROR_CANCELLED {$ELSE} 1223 {$ENDIF};

type
  // e.g. TStrings.Append
  TTextHandler = procedure(const Text: string) of object;

function MuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      NativeCarriageReturn:
        OutPos := LfPos;
      NativeLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := NativeCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

function InternalExecute(CommandLine: string; var Output: string; OutputLineCallback: TTextHandler;
  RawOutput: Boolean; AbortPtr: PBoolean): Cardinal;

const
  BufferSize = 255;
type
  TBuffer = array [0..BufferSize] of AnsiChar;

  procedure ProcessLine(const Line: string; LineEnd: Integer);
  begin
    if RawOutput or (Line[LineEnd] <> NativeCarriageReturn) then
    begin
      while (LineEnd > 0) and CharIsReturn(Line[LineEnd]) do
        Dec(LineEnd);
      OutputLineCallback(Copy(Line, 1, LineEnd));
    end;
  end;

  procedure ProcessBuffer(var Buffer: TBuffer; var Line: string; PipeBytesRead: Cardinal);
  var
    CR, LF: Integer;
  begin
    Buffer[PipeBytesRead] := #0;
    Line := Line + string(Buffer);
    if Assigned(OutputLineCallback) then
    repeat
      CR := Pos(NativeCarriageReturn, Line);
      if CR = Length(Line) then
        CR := 0;        // line feed at CR + 1 might be missing
      LF := Pos(NativeLineFeed, Line);
      if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
        LF := CR;       // accept CR as line end
      if LF > 0 then
      begin
        ProcessLine(Line, LF);
        Delete(Line, 1, LF);
      end;
    until LF = 0;
  end;

var
  Buffer: TBuffer;
  Line: string;
  PipeBytesRead: Cardinal;
{$IFDEF MSWINDOWS}
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
begin
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;
  PipeWrite := 0;
  PipeRead := 0;
  Line := '';
  ResetMemory(Buffer, SizeOf(Buffer));
  if not CreatePipe(PipeRead, PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  ResetMemory(StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := PipeWrite;
  StartupInfo.hStdError := PipeWrite;
  UniqueString(CommandLine); // CommandLine must be in a writable memory block
  ProcessInfo.dwProcessId := 0;
  try
    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS,
      nil, nil, StartupInfo, ProcessInfo) then
    begin
      CloseHandle(PipeWrite);
      PipeWrite := 0;
      if AbortPtr <> nil then
        {$IFDEF FPC}
        AbortPtr^ := 0;
        {$ELSE ~FPC}
        AbortPtr^ := False;
        {$ENDIF ~FPC}
      PipeBytesRead := 0;
      while ((AbortPtr = nil) or not LongBool(AbortPtr^)) and
        ReadFile(PipeRead, Buffer, BufferSize, PipeBytesRead, nil) and (PipeBytesRead > 0) do
        ProcessBuffer(Buffer, Line, PipeBytesRead);
      if (AbortPtr <> nil) and LongBool(AbortPtr^) then
        TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
      if (WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0) and
        not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
          Result := $FFFFFFFF;
      CloseHandle(ProcessInfo.hThread);
      ProcessInfo.hThread := 0;
      CloseHandle(ProcessInfo.hProcess);
      ProcessInfo.hProcess := 0;
    end
    else
    begin
      CloseHandle(PipeWrite);
      PipeWrite := 0;
    end;
    CloseHandle(PipeRead);
    PipeRead := 0;
  finally
    if PipeRead <> 0 then
      CloseHandle(PipeRead);
    if PipeWrite <> 0 then
      CloseHandle(PipeWrite);
    if ProcessInfo.hThread <> 0 then
      CloseHandle(ProcessInfo.hThread);
    if ProcessInfo.hProcess <> 0 then
    begin
      TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, Result);
      CloseHandle(ProcessInfo.hProcess);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := nil;
  try
    Pipe := Libc.popen(PChar(Cmd), 'r');
    { TODO : handle Abort }
    repeat
      PipeBytesRead := fread_unlocked(@Buffer, 1, BufferSize, Pipe);
      if PipeBytesRead > 0 then
        ProcessBuffer(Buffer, Line, PipeBytesRead);
    until PipeBytesRead = 0;
    Result := pclose(Pipe);
    Pipe := nil;
    wait(nil);
  finally
    if Pipe <> nil then
      pclose(Pipe);
    wait(nil);
  end;
{$ENDIF UNIX}
  if Line <> '' then
    if Assigned(OutputLineCallback) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      ProcessLine(Line, Length(Line))
    else
      if RawOutput then
        Output := Output + Line
      else
        Output := Output + MuteCRTerminatedLines(Line);
end;

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal; overload;
begin
  Result := InternalExecute(CommandLine, Output, nil, RawOutput, AbortPtr);
end;

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil): Cardinal; overload; overload;
var
  Dummy: string;
begin
  Dummy := '';
  Result := InternalExecute(CommandLine, Dummy, OutputLineCallback, RawOutput, AbortPtr);
end;

//------------------------------------------------------------------------------

function QuoteFileName(const FileName: string): string;
begin
  Result := '"' + FileName + '"';
end;

{ TGitHistoryItem }

constructor TGitHistoryItem.Create(AParent: TGitItem);
begin
  inherited Create;
  FParent := AParent;
  FBlameItems := TObjectList<TGitBlameItem>.Create;
end;

destructor TGitHistoryItem.Destroy;
begin
  FBlameItems.Free;
  inherited Destroy;
end;

function TGitHistoryItem.GetBlameCount: Integer;
begin
  Result := FBlameItems.Count;
end;

function TGitHistoryItem.GetBlameItems(AIndex: Integer): TGitBlameItem;
begin
  Result := FBlameItems[AIndex];
end;

function TGitHistoryItem.GetFile: TBytes;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
  FullFileName: string;
  FileContent: AnsiString;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FParent.FFileName));
    CmdLine := FParent.FGitClient.GitExecutable + ' ls-files ' + QuoteFileName(ExtractFileName(FParent.FFileName)) + ' --full-name';
    Res := Execute(CmdLine, Output);
    FullFileName := Trim(Output);
    CmdLine := FParent.FGitClient.GitExecutable + ' show ' + FHash + ':' + QuoteFileName(FullFileName);
    Output := '';
    Res := Execute(CmdLine, Output);
    FileContent := Output;
    SetLength(Result,  Length(FileContent));
    Move(FileContent[1], Result[0],  Length(FileContent));
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

procedure TGitHistoryItem.LoadBlame;
var
  I, J, P, Idx, Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  BlameItem: TGitBlameItem;
  S, {S2, }CurrentDir, Hash: string;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FParent.FFileName));
    CmdLine := FParent.FGitClient.GitExecutable + ' blame -l ' + FHash + ' ';
    CmdLine := CmdLine + QuoteFileName(ExtractFileName(FParent.FFileName));
    Res := Execute(CmdLine, Output);
  finally
    SetCurrentDir(CurrentDir);
  end;
  FBlameItems.Clear;
  if Res = 0 then
  begin
    OutputStrings := TStringList.Create;
    try
      OutputStrings.Text := Output;
      I := 0;
      while I < OutputStrings.Count do
      begin
        S := OutputStrings[I];
        BlameItem := FBlameItems[FBlameItems.Add(TGitBlameItem.Create)];
        Hash := Copy(S, 1, 40);
        if Pos('^', Hash) = 1 then
          Delete(Hash, 1, 1);
        Idx := -1;
        for J := 0 to FParent.HistoryCount - 1 do
          //better Pos than =, because of ^39chars har
          if Pos(Hash, FParent.HistoryItems[J].Hash) = 1 then
          begin
            Idx := J;
            Break;
          end;
        if (Idx = -1) and (Pos(StringOfChar('0', 39), Hash) = 1) then
          Idx := -2;
        BlameItem.HistoryIndex := Idx;
        if Idx >= 0 then
          BlameItem.HistoryItem := FParent.HistoryItems[J]
        else
          BlameItem.HistoryItem := nil;
        P := Pos(')', S);
        if P > 0 then
        begin
          {
          J := P - 1;
          S2 := '';
          while (J > 0) and (S[J] in ['0'..'9']) do
          begin
            S2 := S[J] + S2;
            Dec(J);
          end;
          }
          Delete(S, 1, P + 1);
          BlameItem.LineStr := S;
        end;
        Inc(I);
      end;
    finally
      OutputStrings.Free;
    end;
  end;
end;

{ TGitItem }

constructor TGitItem.Create(AGitClient: TGitClient; const AFileName: string);
begin
  inherited Create;
  FGitClient := AGitClient;
  FHistoryItems := TObjectList<TGitHistoryItem>.Create;
  FFileName := AFileName;
  FStatus := gsUnknown;
end;

destructor TGitItem.Destroy;
begin
  FHistoryItems.Free;
  inherited Destroy;
end;

function TGitItem.GetHistoryCount: Integer;
begin
  Result := FHistoryItems.Count;
end;

function TGitItem.GetHistoryItems(AIndex: Integer): TGitHistoryItem;
begin
  Result := FHistoryItems[AIndex];
end;

//http://www.kernel.org/pub/software/scm/git/docs/git-log.html

//revision, date, author, comment, label
//?       , %at , %an   , %s%n%b , ?
procedure TGitItem.LoadHistory(AOnlyLast: Boolean = False);
var
  I, Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  HistoryItem: TGitHistoryItem;
  S, CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FFileName));
    CmdLine := FGitClient.GitExecutable + ' log ';
    if AOnlyLast then
      CmdLine := CmdLine + '-1 ';
    CmdLine := CmdLine + '--pretty=format:"H: %H%nAT: %at%nAN: %an%nAE: %ae%nS: %s%nB: %b" ' + QuoteFileName(ExtractFileName(FFileName));
    Res := Execute(CmdLine, Output);
  finally
    SetCurrentDir(CurrentDir);
  end;
  FHistoryItems.Clear;
  if Res = 0 then
  begin
    OutputStrings := TStringList.Create;
    try
      OutputStrings.Text := UTF8ToString(Output);
      I := 0;
      HistoryItem := nil;
      while I < OutputStrings.Count do
      begin
        S := OutputStrings[I];
        if Pos('H: ', OutputStrings[I]) = 1 then
          HistoryItem := FHistoryItems[FHistoryItems.Add(TGitHistoryItem.Create(Self))];
        if Assigned(HistoryItem) then
        begin
          if Pos('H: ', S) = 1 then
          begin
            Delete(S, 1, 3);
            HistoryItem.FHash := S;
          end;
          if Pos('AT: ', S) = 1 then
          begin
            Delete(S, 1, 4);
            HistoryItem.FDate := StrToIntDef(S, 0);
            HistoryItem.FDate := HistoryItem.FDate / 86400 + EncodeDate(1970, 1, 1);
          end;
          if Pos('AN: ', S) = 1 then
          begin
            Delete(S, 1, 4);
            HistoryItem.FAuthor := S;
          end;
          if Pos('AE: ', S) = 1 then
          begin
            Delete(S, 1, 4);
            HistoryItem.FAuthorEmail := S;
          end;
          if Pos('S: ', S) = 1 then
          begin
            Delete(S, 1, 3);
            HistoryItem.FSubject := S;
          end;
          if Pos('B: ', S) = 1 then
          begin
            Delete(S, 1, 3);
            HistoryItem.FBody := S;
          end;
        end;
        Inc(I);
      end;
    finally
      OutputStrings.Free;
    end;
  end;
end;

procedure TGitItem.LoadStatus;
var
  Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  CurrentDir: string;
begin
  FStatus := gsUnknown;
  CurrentDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FFileName));
    CmdLine := FGitClient.GitExecutable + ' diff --name-status ' + QuoteFileName(ExtractFileName(FFileName));
    Res := Execute(CmdLine, Output);
    if (Res = 0) and (Pos('fatal: Not a git repository', Output) > 0) then
      Exit;
    if (Res = 0) and (Trim(Output) <> '') then
    begin
      OutputStrings := TStringList.Create;
      try
        OutputStrings.Text := Output;
        if (OutputStrings.Count > 0) and (Pos(ExtractFileName(FFileName), OutputStrings[0]) > 0) then
        begin
          if Pos('M' + #9, OutputStrings[0]) = 1 then
            FStatus := gsModified;
        end;
      finally
        OutputStrings.Free;
      end;
    end;

    if FStatus = gsUnknown then
    begin
      CmdLine := FGitClient.GitExecutable + ' diff --cached --name-only --diff-filter=A ' + QuoteFileName(ExtractFileName(FFileName));
      Res := Execute(CmdLine, Output);

      if (Res = 0) and (Trim(Output) <> '') then
      begin
        OutputStrings := TStringList.Create;
        try
          OutputStrings.Text := Output;
          if (OutputStrings.Count > 0) and (Pos(ExtractFileName(FFileName), OutputStrings[0]) > 0) then
            FStatus := gsAdded;
        finally
          OutputStrings.Free;
        end;
      end;
    end;

    if FStatus = gsUnknown then
    begin
      CmdLine := FGitClient.GitExecutable + ' ls-files -t ' + QuoteFileName(ExtractFileName(FFileName));
      Res := Execute(CmdLine, Output);

      if (Res = 0) and (Trim(Output) <> '') then
      begin
        OutputStrings := TStringList.Create;
        try
          OutputStrings.Text := Output;
          if (OutputStrings.Count > 0) and (Pos(ExtractFileName(FFileName), OutputStrings[0]) > 0) and
            (Pos('H ', OutputStrings[0]) = 1) then
            FStatus := gsNormal;
        finally
          OutputStrings.Free;
        end;
      end;
    end;
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

{ TGitClient }

constructor TGitClient.Create;
begin
  inherited Create;
  //FGitExecutable := 'c:\Program Files (x86)\Git\bin\git.exe';
end;

function TGitClient.Clone(const ASourcePath, ADestPath: string; ACallBack: TGitCloneCallBack): Boolean;
var
  Res: Integer;
  CmdLine: string;
  CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    ForceDirectories(ADestPath);
    SetCurrentDir(ADestPath);
    CmdLine := GitExecutable + ' clone -v --progress ';
    CmdLine := CmdLine + QuoteFileName(ASourcePath) + ' .';
    FCloneCallBack := ACallBack;
    try
      FCancel := False;
      Res := Execute(CmdLine, ExecuteTextHandler, False, @FCancel);
    finally
      FCloneCallBack := nil;
    end;
    Result := Res = 0;
  finally
    SetCurrentDir(CurrentDir);
  end;
end;

procedure TGitClient.ExecuteTextHandler(const Text: string);
begin
  if Assigned(FCloneCallBack) then
    FCloneCallBack(Self, Text, FCancel);
end;

function TGitClient.IsPathInWorkingCopy(const APath: string): Boolean;
var
  Dir, LastDir: string;
  F: TSearchRec;
  Re, L: Integer;
begin
  Result := False;
  Dir := ExcludeTrailingPathDelimiter(APath);
  LastDir := '';
  L := Length(ExtractFileDrive(APath));
  while (Dir <> '') and (Dir <> LastDir) do
  begin
    LastDir := Dir;
    Re := FindFirst(Dir + '\.git', faAnyFile, F);
    FindClose(F);
    if (Re = 0) and (F.Attr and faDirectory <> 0) then
    begin
      Result := True;
      Break;
    end;
    if Length(Dir) - 1 > L then
      Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Copy(Dir, 1, Length(Dir) - 1)))
    else
      Break;
  end;
end;

function TGitClient.IsVersioned(const AFileName: string): Boolean;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  if FGitExecutable <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFilePath(AFileName));
      CmdLine := FGitExecutable + ' log --max-count=1 ' + QuoteFileName(ExtractFileName(AFileName));
      Res := Execute(CmdLine, Output);
      if (Res = 0) and (Pos('commit ', Output) = 1) then
        Result := True
      else
      begin
        CmdLine := FGitExecutable + ' status ' + QuoteFileName(ExtractFileName(AFileName));
        Res := Execute(CmdLine, Output);
        Result := {(Res = 0) and }(Pos('fatal: Not a git repository', Output) = 0);
      end;
    finally
      SetCurrentDir(CurrentDir);
    end;
  end
  else
    Result := False;
end;

end.
