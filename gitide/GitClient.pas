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
{ Portions created by Uwe Schuster are Copyright © 2010 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitClient;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, IOUtils;

type
  TGitItem = class;
  TGitBlameItem = class;

  TGitHistoryItem = class(TObject)
  private
    FAuthor: string;
    FAuthorEmail: string;
    FBlameItems: TObjectList<TGitBlameItem>;
    FBody: string;
    FChangedFiles: TStringList;
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
    property ChangedFiles: TStringList read FChangedFiles;
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

  TGitStatus = (gsAdded, gsModified, gsNormal, gsUnknown, gsUnversioned, gsDeleted);

  TGitClient = class;

  IGitAsyncUpdate = interface
    ['{E2833FCA-0595-4D85-9D03-19565B9C6433}']
    procedure UpdateHistoryItems(GitItem: TGitItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;
  end;

  TGitItem = class(TObject)
  private
    FAsyncUpdate: IGitAsyncUpdate;
    FFileName: string;
    FGitClient: TGitClient;
    FHistoryItems: TObjectList<TGitHistoryItem>;
    FIsDirectory: Boolean;
    FStatus: TGitStatus;
    FLogLimit: Integer;
    FLogFirstRev: string;
    FLogLastRev: string;
    FIncludeChangedFiles: Boolean;
    function GetHistoryCount: Integer;
    function GetHistoryItems(AIndex: Integer): TGitHistoryItem;
    function GetBaseHash: string;
  public
    constructor Create(AGitClient: TGitClient; const AFileName: string);
    destructor Destroy; override;
    procedure AsyncReloadHistory;
    function GetBaseFile: TBytes;
    procedure LoadHistory(AOnlyLast: Boolean = False);
    procedure LoadStatus;
    property AsyncUpdate: IGitAsyncUpdate read FAsyncUpdate write FAsyncUpdate;
    property BaseHash: string read GetBaseHash;
    property FileName: string read FFileName;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[AIndex: Integer]: TGitHistoryItem read GetHistoryItems;
    property IncludeChangedFiles: Boolean read FIncludeChangedFiles write FIncludeChangedFiles;
    property IsDirectory: Boolean read FIsDirectory;
    property LogLimit: Integer read FLogLimit write FLogLimit;
    property LogFirstRev: string read FLogFirstRev write FLogFirstRev;
    property LogLastRev: string read FLogLastRev write FLogLastRev;
    property Status: TGitStatus read FStatus;
  end;

  TGitStatusList = class(TObject)
  private
    FGitClient: TGitClient;
    FList: TList<TPair<string, TGitStatus>>;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TPair<string, TGitStatus>;
  public
    constructor Create(AGitClient: TGitClient);
    destructor Destroy; override;
    procedure Add(const AFileName: string);
    procedure Clear;
    procedure Load;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TPair<string, TGitStatus> read GetItems; default;
  end;

  TGitCloneCallBack = procedure(Sender: TObject; const AText: string; var Cancel: Boolean) of object;
  TGitStatusCallback = procedure(Sender: TObject; Item: TGitItem; var Cancel: Boolean) of object;
  TGitError = (geSuccess, geEmptyCommitMessage, geUnknown);

  TGitClient = class(TObject)
  private
    FCancel: Boolean;
    FCloneCallBack: TGitCloneCallBack;
    FGitExecutable: string;
    FLastCommitInfoBranch: string;
    FLastCommitInfoHash: string;
    procedure ExecuteTextHandler(const Text: string);
  public
    constructor Create;
    function Add(const AFileName: string): Boolean;
    function Clone(const ASourcePath, ADestPath: string; ACallBack: TGitCloneCallBack = nil): Boolean;
    function Commit(AFileList: TStringList; const AMessage: string; const AUser: string = ''): TGitError;
    function FindRepositoryRoot(const APath: string): string;
    function GetModifications(const APath: string; ACallBack: TGitStatusCallback): Boolean;
    function IsPathInWorkingCopy(const APath: string): Boolean;
    function IsVersioned(const AFileName: string): Boolean;
    function Revert(const AFileName: string): Boolean;
    procedure SaveFileContentToStream(const AFileName, ARevision: string; OutputStream: TStream);
    property GitExecutable: string read FGitExecutable write FGitExecutable;
    property LastCommitInfoBranch: string read FLastCommitInfoBranch;
    property LastCommitInfoHash: string read FLastCommitInfoHash;
  end;

function UTCToTzDateTime(Value: TDateTime): TDateTime;

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
  RawOutput: Boolean; AbortPtr: PBoolean; const CurrentDir: string): Cardinal;

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
  PCurrentDir: PChar;
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
    if CurrentDir <> '' then
      PCurrentDir := PChar(CurrentDir)
    else
      PCurrentDir := nil;
    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS,
      nil, PCurrentDir, StartupInfo, ProcessInfo) then
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
  AbortPtr: PBoolean = nil; const CurrentDir: string = ''): Cardinal; overload;
begin
  Result := InternalExecute(CommandLine, Output, nil, RawOutput, AbortPtr, CurrentDir);
end;

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; const CurrentDir: string = ''): Cardinal; overload;
var
  Dummy: string;
begin
  Dummy := '';
  Result := InternalExecute(CommandLine, Dummy, OutputLineCallback, RawOutput, AbortPtr, CurrentDir);
end;

//------------------------------------------------------------------------------

function UTCToTzDateTime(Value: TDateTime): TDateTime;
var
  TZ: TTimeZoneInformation;
begin
  Result := Value;
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result - (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result - (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result - TZ.Bias / MinsPerDay;
  end;
end;

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
  FChangedFiles := TStringList.Create;
end;

destructor TGitHistoryItem.Destroy;
begin
  FChangedFiles.Free;
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
  //Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
  FullFileName: string;
  FileContent: AnsiString;
begin
  CurrentDir := ExtractFilePath(FParent.FFileName);
  CmdLine := FParent.FGitClient.GitExecutable + ' ls-files ' + QuoteFileName(ExtractFileName(FParent.FFileName)) + ' --full-name';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FullFileName := Trim(Output);
  CmdLine := FParent.FGitClient.GitExecutable + ' show ' + FHash + ':' + QuoteFileName(FullFileName);
  Output := '';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FileContent := AnsiString(Output);
  SetLength(Result, Length(FileContent));
  Move(FileContent[1], Result[0], Length(FileContent));
end;

procedure TGitHistoryItem.LoadBlame;
var
  I, J, P, Idx, Res: Integer;
  CmdLine, Output: string;
  OutputStrings: TStringList;
  BlameItem: TGitBlameItem;
  S, {S2, }CurrentDir, Hash: string;
begin
  CurrentDir := ExtractFilePath(FParent.FFileName);
  CmdLine := FParent.FGitClient.GitExecutable + ' blame -l ' + FHash + ' ';
  CmdLine := CmdLine + QuoteFileName(ExtractFileName(FParent.FFileName));
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
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

type
  TGitHistoryThread = class(TThread)
  private
    FAsyncUpdate: IGitAsyncUpdate;
    FGitItem: TGitItem;
    FLastAdded: Integer;
  protected
    procedure Completed(Sender: TObject);
    procedure Execute; override;
  public
    constructor Create(AGitItem: TGitItem; AsyncUpdate: IGitAsyncUpdate);
  end;

{ TGitHistoryThread }

procedure TGitHistoryThread.Completed(Sender: TObject);
begin
  if (FLastAdded = 0) and (FGitItem.HistoryCount > 0) then
  begin
    FAsyncUpdate.UpdateHistoryItems(FGitItem, FLastAdded, 0, False);
    Inc(FLastAdded);
  end;
  FAsyncUpdate.UpdateHistoryItems(FGitItem, FLastAdded, FGitItem.HistoryCount - 1, True);
  FLastAdded := FGitItem.HistoryCount;
  FAsyncUpdate.Completed;
end;

constructor TGitHistoryThread.Create(AGitItem: TGitItem; AsyncUpdate: IGitAsyncUpdate);
begin
  inherited Create(False);
  FGitItem := AGitItem;
  FreeOnTerminate := True;
  FAsyncUpdate := AsyncUpdate;
  FLastAdded := 0;
  OnTerminate := Completed;
end;

procedure TGitHistoryThread.Execute;
begin
  NameThreadForDebugging('VerIns Git History Updater');
  FGitItem.LoadHistory;
end;

{ TGitItem }

constructor TGitItem.Create(AGitClient: TGitClient; const AFileName: string);
begin
  inherited Create;
  FGitClient := AGitClient;
  FHistoryItems := TObjectList<TGitHistoryItem>.Create;
  FFileName := AFileName;
  FIsDirectory := DirectoryExists(AFileName);
  FStatus := gsUnknown;
  FLogLimit := -1;
  FLogFirstRev := '';
  FLogLastRev := '';
  FIncludeChangedFiles := False;
end;

destructor TGitItem.Destroy;
begin
  FHistoryItems.Free;
  inherited Destroy;
end;

procedure TGitItem.AsyncReloadHistory;
begin
  FHistoryItems.Clear;
  TGitHistoryThread.Create(Self, FASyncUpdate);
end;

function TGitItem.GetBaseFile: TBytes;
var
  //Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
  FullFileName: string;
  FileContent: AnsiString;
begin
  CurrentDir := ExtractFilePath(FFileName);
  CmdLine := FGitClient.GitExecutable + ' ls-files ' + QuoteFileName(ExtractFileName(FFileName)) + ' --full-name';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FullFileName := Trim(Output);
  CmdLine := FGitClient.GitExecutable + ' show ' + ':' + QuoteFileName(FullFileName);
  Output := '';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FileContent := AnsiString(Output);
  SetLength(Result, Length(FileContent));
  Move(FileContent[1], Result[0], Length(FileContent));
end;

function TGitItem.GetBaseHash: string;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  Result := '';
  CurrentDir := ExtractFilePath(FFileName);
  CmdLine := FGitClient.GitExecutable + ' log --max-count=1 --pretty=format:"H: %H" ' + QuoteFileName(ExtractFileName(FFileName));
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
  if (Res = 0) and (Pos('H: ', Output) = 1) then
  begin
    Delete(Output, 1, 3);
    Result := Trim(Output);
  end;
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
  CmdLine, Output, LogFileName: string;
  OutputStrings: TStringList;
  HistoryItem: TGitHistoryItem;
  S, CurrentDir: string;
begin
  if IsDirectory then
  begin
    CurrentDir := FFileName;
    LogFileName := '.';
  end
  else
  begin
    CurrentDir := ExtractFilePath(FFileName);
    LogFileName := ExtractFileName(FFileName);
  end;
  CmdLine := FGitClient.GitExecutable + ' log ';
  if AOnlyLast then
    CmdLine := CmdLine + '-1 '
  else
  if FLogLimit > 0 then
    CmdLine := CmdLine + '-' + IntToStr(FLogLimit) + ' ';
  if (FLogFirstRev <> '') and (FLogLastRev <> '') then
    CmdLine := CmdLine + FLogFirstRev + '...' + FLogLastRev + ' '
  else
  if FLogFirstRev <> '' then
    CmdLine := CmdLine + FLogFirstRev + ' '
  else
  if FLogFirstRev <> '' then
    CmdLine := CmdLine + '...' + FLogLastRev + ' ';
  if FIncludeChangedFiles then
    CmdLine := CmdLine + '--pretty=format:"H: %H%nAT: %at%nAN: %an%nAE: %ae%nS: %s%nB: %b%nF:" --name-status ' + QuoteFileName(LogFileName)
  else
    CmdLine := CmdLine + '--pretty=format:"H: %H%nAT: %at%nAN: %an%nAE: %ae%nS: %s%nB: %b" ' + QuoteFileName(LogFileName);
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
  FHistoryItems.Clear;
  if Res = 0 then
  begin
    OutputStrings := TStringList.Create;
    try
      OutputStrings.Text := UTF8ToString(AnsiString(Output));
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
          end
          else
          if Pos('F:', S) = 1 then
          begin
            Inc(I);
            while (I < OutputStrings.Count) and (Pos('H:', OutputStrings[I]) = 0) do
            begin
              S := OutputStrings[I];
              if (Length(S) > 2) and CharInSet(S[1], ['M', 'A', 'D']) and (S[2] = #9) then
              begin
                Delete(S, 2, 1);
                HistoryItem.FChangedFiles.Add(S);
              end;
              Inc(I);
            end;
            if (I < OutputStrings.Count) and (Pos('H:', OutputStrings[I]) = 1) then
              Dec(I);
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
  CurrentDir := ExtractFilePath(FFileName);
  CmdLine := FGitClient.GitExecutable + ' diff --name-status ' + QuoteFileName(ExtractFileName(FFileName));
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
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
    Res := Execute(CmdLine, Output, False, nil, CurrentDir);

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
    Res := Execute(CmdLine, Output, False, nil, CurrentDir);

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
end;

{ TGitStatusList }

procedure TGitStatusList.Clear;
begin
  FList.Clear;
end;

constructor TGitStatusList.Create(AGitClient: TGitClient);
begin
  inherited Create;
  FGitClient := AGitClient;
  FList := TList<TPair<string, TGitStatus>>.Create;
end;

destructor TGitStatusList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TGitStatusList.Add(const AFileName: string);
begin
  FList.Add(TPair<string, TGitStatus>.Create(AFileName, gsUnknown));
end;

function TGitStatusList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGitStatusList.GetItems(AIndex: Integer): TPair<string, TGitStatus>;
begin
  Result := FList[AIndex];
end;

procedure TGitStatusList.Load;
const
  MaxItemsPerDir = 10;
var
  Res: Integer;
  CmdLine, Output: string;
  S, CurrentDir, ItemsDir, FileName: string;
  LoadedItems: TDictionary<Integer, Integer>;
  DirItems: TList<TPair<Integer, string>>;
  I, J, Idx, DelIdx, LastIndex, Dummy, L: Integer;
  OutputStrings: TStringList;
  Status: TGitStatus;
  Pair: TPair<string, TGitStatus>;
begin
  LoadedItems := TDictionary<Integer, Integer>.Create;
  DirItems := TList<TPair<Integer, string>>.Create;
  try
    LastIndex := 0;
    while LastIndex < Count do
    begin
      DirItems.Clear;
      ItemsDir := '';
      for I := LastIndex to Count - 1 do
        if not LoadedItems.TryGetValue(I, Dummy) then
        begin
          if ItemsDir = '' then
          begin
            ItemsDir := ExtractFilePath(Items[I].Key);
            DirItems.Add(TPair<Integer, string>.Create(I, ExtractFileName(Items[I].Key)));
          end
          else
          if AnsiSameText(ItemsDir, ExtractFilePath(Items[I].Key)) then
            DirItems.Add(TPair<Integer, string>.Create(I, ExtractFileName(Items[I].Key)));
          if DirItems.Count >= MaxItemsPerDir then
            Break;
        end;
      if DirItems.Count > 0 then
      begin
        LastIndex := DirItems.Last.Key;
        for I := 0 to DirItems.Count - 1 do
          LoadedItems.Add(DirItems[I].Key, 0);
        CurrentDir := ItemsDir;
        CmdLine := FGitClient.GitExecutable + ' diff --name-status ';
        for I := 0 to DirItems.Count - 1 do
          CmdLine := CmdLine + ' ' + QuoteFileName(DirItems[I].Value);
        Output := '';
        Res := Execute(CmdLine, Output, False, nil, CurrentDir);
        if Res = 0 then
        begin
          OutputStrings := TStringList.Create;
          try
            OutputStrings.Text := Output;
            I := 0;
            while I < OutputStrings.Count do
            begin
              S := AnsiUpperCase(OutputStrings[I]);
              L := Length(S);
              if L > 2 then
              begin
                if S[1] = 'M' then
                  Status := gsModified
                else
                  Status := gsUnknown;
                DelIdx := -1;
                for J := 0 to DirItems.Count - 1 do
                begin
                  FileName := AnsiUpperCase(DirItems[J].Value);
                  if Pos(FileName, S) = L - Length(FileName) + 1 then
                  begin
                    Idx := DirItems[J].Key;
                    Pair := Items[Idx];
                    Pair.Value := Status;
                    FList[Idx] := Pair;
                    DelIdx := J;
                    Break;
                  end;
                end;
                if DelIdx <> -1 then
                  DirItems.Delete(DelIdx);
              end;
              Inc(I);
            end;
          finally
            OutputStrings.Free;
          end;
        end;

        if DirItems.Count > 0 then
        begin
          CurrentDir := ItemsDir;
          CmdLine := FGitClient.GitExecutable + ' diff --cached --name-only --diff-filter=A ';
          for I := 0 to DirItems.Count - 1 do
            CmdLine := CmdLine + ' ' + QuoteFileName(DirItems[I].Value);
          Output := '';
          Res := Execute(CmdLine, Output, False, nil, CurrentDir);
          if Res = 0 then
          begin
            OutputStrings := TStringList.Create;
            try
              OutputStrings.Text := Output;
              I := 0;
              while I < OutputStrings.Count do
              begin
                S := AnsiUpperCase(OutputStrings[I]);
                L := Length(S);
                if L > 0 then
                begin
                  Status := gsAdded;
                  DelIdx := -1;
                  for J := 0 to DirItems.Count - 1 do
                  begin
                    FileName := AnsiUpperCase(DirItems[J].Value);
                    if Pos(FileName, S) = L - Length(FileName) + 1 then
                    begin
                      Idx := DirItems[J].Key;
                      Pair := Items[Idx];
                      Pair.Value := Status;
                      FList[Idx] := Pair;
                      DelIdx := J;
                      Break;
                    end;
                  end;
                  if DelIdx <> -1 then
                    DirItems.Delete(DelIdx);
                end;
                Inc(I);
              end;
            finally
              OutputStrings.Free;
            end;
          end;
        end;

        if DirItems.Count > 0 then
        begin
          CurrentDir := ItemsDir;
          CmdLine := FGitClient.GitExecutable + ' ls-files -t ';
          for I := 0 to DirItems.Count - 1 do
            CmdLine := CmdLine + ' ' + QuoteFileName(DirItems[I].Value);
          Output := '';
          Res := Execute(CmdLine, Output, False, nil, CurrentDir);
          if Res = 0 then
          begin
            OutputStrings := TStringList.Create;
            try
              OutputStrings.Text := Output;
              I := 0;
              while I < OutputStrings.Count do
              begin
                S := AnsiUpperCase(OutputStrings[I]);
                L := Length(S);
                if (L > 2) and (S[1] = 'H') then
                begin
                  Status := gsNormal;
                  DelIdx := -1;
                  for J := 0 to DirItems.Count - 1 do
                  begin
                    FileName := AnsiUpperCase(DirItems[J].Value);
                    if Pos(FileName, S) = L - Length(FileName) + 1 then
                    begin
                      Idx := DirItems[J].Key;
                      Pair := Items[Idx];
                      Pair.Value := Status;
                      FList[Idx] := Pair;
                      DelIdx := J;
                      Break;
                    end;
                  end;
                  if DelIdx <> -1 then
                    DirItems.Delete(DelIdx);
                end;
                Inc(I);
              end;
            finally
              OutputStrings.Free;
            end;
          end;
        end;

        for I := 0 to DirItems.Count - 1 do
        begin
          Idx := DirItems[I].Key;
          Pair := Items[Idx];
          Pair.Value := gsUnknown;
          FList[Idx] := Pair;
        end;
      end
      else
        Break;
    end;
  finally
    DirItems.Free;
    LoadedItems.Free;
  end;
end;

{ TGitClient }

function TGitClient.Commit(AFileList: TStringList; const AMessage: string; const AUser: string = ''): TGitError;
var
  I, P, Res: Integer;
  CmdLine, Output, TempFileName: string;
  CurrentDir: string;
  MS: TMemoryStream;
  B: TBytes;
begin
  Result := geUnknown;
  if AMessage = '' then
    Result := geEmptyCommitMessage
  else
  if AFileList.Count > 0 then
  begin
    FLastCommitInfoBranch := '';
    FLastCommitInfoHash := '';
    CurrentDir := ExtractFilePath(AFileList[0]);

    if Pos('"', AMessage) > 0 then
    begin
      TempFileName := TPath.GetTempFileName;
      MS := TMemoryStream.Create;
      try
        B := TEncoding.UTF8.GetBytes(AMessage);
        MS.Write(B[Low(B)], Length(B));
        MS.SaveToFile(TempFileName);
      finally
        MS.Free;
      end;
      CmdLine := GitExecutable + ' commit -F ' + QuoteFileName(TempFileName);
    end
    else
    begin
      TempFileName := '';
      CmdLine := GitExecutable + ' commit -m ' + AnsiQuotedStr(AMessage, '"');
    end;
    try
      if AUser <> '' then
        CmdLine := CmdLine + ' --author ' + AnsiQuotedStr(AUser, '"');
      CmdLine := CmdLine + ' -o';
      for I := 0 to AFileList.Count - 1 do
        CmdLine := CmdLine + ' ' + StringReplace(QuoteFileName(AFileList[I]), '\', '/', [rfReplaceAll]);
      Res := Execute(CmdLine, Output, False, nil, CurrentDir);
      if Res = 0 then
      begin
        if Pos('Aborting commit due to empty commit message', Output) > 0 then
          Result := geEmptyCommitMessage
        else
        if Pos('[', Output) = 1 then
        begin
          Result := geSuccess;
          P := Pos(']', Output);
          FLastCommitInfoHash := Copy(Output, P - 7, 7);
          FLastCommitInfoBranch := Copy(Output, 1, P - 9);
          P := Pos('[', FLastCommitInfoBranch);
          if P > 0 then
            Delete(FLastCommitInfoBranch, 1, P);
        end;
      end;
    finally
      if TempFileName <> '' then
        DeleteFile(TempFileName);
    end;
  end;
end;

constructor TGitClient.Create;
begin
  inherited Create;
  //FGitExecutable := 'c:\Program Files (x86)\Git\bin\git.exe';
end;

function TGitClient.Add(const AFileName: string): Boolean;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  CurrentDir := ExtractFilePath(AFileName);
  CmdLine := FGitExecutable + ' add ' + QuoteFileName(ExtractFileName(AFileName));
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
  Result := Res = 0;
end;

function TGitClient.Clone(const ASourcePath, ADestPath: string; ACallBack: TGitCloneCallBack): Boolean;
var
  Res: Integer;
  CmdLine: string;
  CurrentDir: string;
begin
  ForceDirectories(ADestPath);
  CurrentDir := ADestPath;
  CmdLine := GitExecutable + ' clone -v --progress ';
  CmdLine := CmdLine + QuoteFileName(ASourcePath) + ' .';
  FCloneCallBack := ACallBack;
  try
    FCancel := False;
    Res := Execute(CmdLine, ExecuteTextHandler, False, @FCancel, CurrentDir);
  finally
    FCloneCallBack := nil;
  end;
  Result := Res = 0;
end;

procedure TGitClient.ExecuteTextHandler(const Text: string);
begin
  if Assigned(FCloneCallBack) then
    FCloneCallBack(Self, Text, FCancel);
end;

function TGitClient.FindRepositoryRoot(const APath: string): string;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  CurrentDir := APath;
  CmdLine := GitExecutable + ' rev-parse --show-toplevel';
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
  if Res = 0 then
    Result := Trim(Output)
  else
    Result := '';
end;

function TGitClient.GetModifications(const APath: string; ACallBack: TGitStatusCallback): Boolean;
var
  I, Res: Integer;
  CmdLine, Output, S, StatusStr: string;
  OutputStrings: TStringList;
  CurrentDir: string;
  GitItem: TGitItem;
  Cancel, IsDirectory: Boolean;
begin
  Result := Assigned(ACallBack);
  if Result then
  begin
    if DirectoryExists(APath) then
    begin
      CurrentDir := APath;
      IsDirectory := True;
      CmdLine := GitExecutable + ' status -s -uall .';
    end
    else
    begin
      CurrentDir := ExtractFilePath(APath);
      IsDirectory := False;
      CmdLine := GitExecutable + ' status -s -uall ' + QuoteFileName(ExtractFileName(APath));
    end;
    Res := Execute(CmdLine, Output, False, nil, CurrentDir);
    Result := Res = 0;
    if Result then
    begin
      OutputStrings := TStringList.Create;
      try
        OutputStrings.Text := Output;
        Cancel := False;
        for I := 0 to OutputStrings.Count - 1 do
        begin
          S := OutputStrings[I];
          if (Length(S) > 3) and (S[3] = ' ') then
          begin
            S := StringReplace(S, '/', '\', [rfReplaceAll]);
            StatusStr := Trim(Copy(S, 1, 2));
            Assert((StatusStr[1] = 'M') or (StatusStr[1] = 'A') or (StatusStr[1] = 'D') or (StatusStr[1] = '?'));
            if IsDirectory then
              GitItem := TGitItem.Create(Self, IncludeTrailingPathDelimiter(APath) + Copy(S, 4, Length(S)))
            else
              GitItem := TGitItem.Create(Self, ExtractFilePath(APath) + Copy(S, 4, Length(S)));
            if StatusStr[1] = 'M' then
              GitItem.FStatus := gsModified
            else
            if StatusStr[1] = 'A' then
              GitItem.FStatus := gsAdded
            else
            if StatusStr[1] = 'D' then
              GitItem.FStatus := gsDeleted
            else
            if StatusStr[1] = '?' then
              GitItem.FStatus := gsUnversioned
            else
              GitItem.FStatus := gsUnknown;
            ACallBack(Self, GitItem, Cancel);
            if Cancel then
              Break;
          end;
        end;
      finally
        OutputStrings.Free;
      end;
    end;
  end;
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
  CmdLine, Output, CheckFileName: string;
  CurrentDir: string;
begin
  if FGitExecutable <> '' then
  begin
    if DirectoryExists(AFileName) then
    begin
      CurrentDir := AFileName;
      CheckFileName := '.';
    end
    else
    begin
      CurrentDir := ExtractFilePath(AFileName);
      CheckFileName := ExtractFileName(AFileName);
    end;
    CmdLine := FGitExecutable + ' log --max-count=1 ' + QuoteFileName(CheckFileName);
    Res := Execute(CmdLine, Output, False, nil, CurrentDir);
    if (Res = 0) and (Pos('commit ', Output) = 1) then
      Result := True
    else
    begin
      CmdLine := FGitExecutable + ' status ' + QuoteFileName(CheckFileName);
      {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
      Result := {(Res = 0) and }(Pos('fatal: Not a git repository', Output) = 0);
    end;
  end
  else
    Result := False;
end;

function TGitClient.Revert(const AFileName: string): Boolean;
var
  Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
begin
  CurrentDir := ExtractFilePath(AFileName);
  CmdLine := FGitExecutable + ' checkout HEAD ' + QuoteFileName(ExtractFileName(AFileName));
  Res := Execute(CmdLine, Output, False, nil, CurrentDir);
  Result := (Res = 0) and (Trim(Output) = '');
end;

procedure TGitClient.SaveFileContentToStream(const AFileName, ARevision: string;
  OutputStream: TStream);
var
  //Res: Integer;
  CmdLine, Output: string;
  CurrentDir: string;
  FullFileName: string;
  FileContent: AnsiString;
begin
  CurrentDir := ExtractFilePath(AFileName);
  CmdLine := GitExecutable + ' ls-files ' + QuoteFileName(ExtractFileName(AFileName)) + ' --full-name';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FullFileName := Trim(Output);
  CmdLine := GitExecutable + ' show ' + ARevision + ':' + QuoteFileName(FullFileName);
  Output := '';
  {Res := }Execute(CmdLine, Output, False, nil, CurrentDir);
  FileContent := AnsiString(Output);
  if Length(FileContent) > 0 then
    OutputStream.Write(FileContent[1], Length(FileContent));
end;

end.
