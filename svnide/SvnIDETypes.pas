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
{ The Original Code is SvnIDETypes.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2011 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDETypes;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, Graphics, Generics.Collections, ActiveX, svn_client, SvnIDEClient,
  SvnClientUpdate, SvnClientProgress, SvnClient, SvnClientLog, SvnClientLogDialog;

type
  TCustomUpdateThread = class(TThread)
  protected
    FAborted: Boolean;
    FSvnIDEClient: TSvnIDEClient;
    FSyncPath: string;
    FSyncAction: string;
    FSyncConflicted: Boolean;
    FSyncTextColor: TColor;
    FUpdateDialog: TUpdateDialog;
    FExceptionMessage: string;
    procedure AbortCallBack;
    procedure CancelCallback(Sender: TObject; var Cancel: Boolean);
    procedure Add(const Path, Action: string; Conflicted: Boolean; TextColor: TColor);
    procedure SyncAdd;
    procedure SyncCompleted;
    function ConflictCallback(Sender: TObject; var ResultFileName: string;
      var Choice: TSvnWcConflictChoice; description: PSvnWcConflictDescription): Boolean;
    procedure UpdateCallBack(Sender: TObject; const Path, MimeType: string; Action: TSvnWcNotifyAction;
      Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);
  public
    constructor Create(SvnIDEClient: TSvnIDEClient); reintroduce;
  end;

  TMergeThread = class(TCustomUpdateThread)
  protected
    FURL: string;
    FPath: string;
    FRevision1: Integer;
    FRevision2: Integer;
    procedure Execute; override;
  public
    constructor Create(SvnIDEClient: TSvnIDEClient; AURL, APath: string; ARevision1, ARevision2: Integer); reintroduce;
  end;

  TMergeRangeThread = class(TCustomUpdateThread)
  protected
    FURL: string;
    FRangesToMerge: TSvnMergeRevisionList;
    FPegRevision: Integer;
    FTargetWcpath: string;
    FDepth: TSvnDepth;
    FIgnoreAncestry: TSvnBoolean;
    FForce: TSvnBoolean;
    FRecordOnly: TSvnBoolean;
    FDryRun: TSvnBoolean;
    FIgnoreEOL: Boolean;
    FIgnoreSpace: Boolean;
    FIgnoreSpaceAll: Boolean;
    procedure Execute; override;
  public
    constructor Create(SvnIDEClient: TSvnIDEClient; const AURL: string; ARangesToMerge: TSvnMergeRevisionList;
      APegRevision: Integer; const ATargetWcpath: string;
      ADepth: TSvnDepth = svnDepthInfinity; AIgnoreAncestry: TSvnBoolean = False; AForce: TSvnBoolean = False;
      ARecordOnly: TSvnBoolean = False; ADryRun: TSvnBoolean = False; AIgnoreEOL: Boolean = False;
      AIgnoreSpace: Boolean = False; AIgnoreSpaceAll: Boolean = False); reintroduce;
    destructor Destroy; override;
  end;

  TSwitchThread = class(TCustomUpdateThread)
  protected
    FDepth: TSvnDepth;
    FDepthIsSticky: TSvnBoolean;
    FIgnoreExternals: TSvnBoolean;
    FPath: string;
    FPegRevision: TSvnRevNum;
    FRevision: TSvnRevNum;
    FURL: string;
    procedure Execute; override;
  public
    constructor Create(SvnIDEClient: TSvnIDEClient; const APath, AURL: string; APegRevision: TSvnRevNum = -1; ARevision: TSvnRevNum = -1;
      ADepth: TSvnDepth = svnDepthInfinity; ADepthIsSticky: TSvnBoolean = False; AIgnoreExternals: TSvnBoolean = False); reintroduce;
  end;

  TCustomProgressThread = class(TThread)
  protected
    FAborted: Boolean;
    FSyncMax: Integer;
    FSyncPosition: Integer;
    FSyncText: string;
    FSvnProgressDialog: TSvnProgressDialog;
    procedure AbortCallBack;
    procedure SyncCloseDialog;
    procedure SyncUpdateCaption;
    procedure SyncUpdateProgress;
    procedure UpdateCaption(const AText: string);
    procedure UpdateProgress(APosition, AMax: Integer);
  public
    constructor Create;
  end;

  TCompareRevisionThread = class(TCustomProgressThread)
  private
    type
      TCompareFile = class(TObject)
      private
        FFileName: string;
        FRevision1: Integer;
        FRevision2: Integer;
        FStream1: IStream;
        FStream2: IStream;
      public
        constructor Create(const AFileName: string; ARevision1, ARevision2: Integer);
        property FileName: string read FFileName;
        property Revision1: Integer read FRevision1;
        property Revision2: Integer read FRevision2;
        property Stream1: IStream read FStream1 write FStream1;
        property Stream2: IStream read FStream2 write FStream2;
      end;
    var
      FFiles: TObjectList<TCompareFile>;
    procedure SyncCallCompare;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddFile(const AFileName: string; ARevision1, ARevision2: Integer);
  end;

  TSaveRevisionThread = class(TCustomProgressThread)
  private
    FFiles: TStringList;
    FPath: string;
    FRevision: Integer;
  public
    constructor Create(ARevision: Integer; const APath: string);
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddFile(const AFileName: string);
  end;

  TLogDialogHelper = class(TObject, IInterface, IAsyncUpdate)
  private
    FFirst: Integer;
    FLogDialog: TSvnLogDialog;
    FRootPath: string;
    FSvnClient: TSvnClient;
    FSvnItem: TSvnItem;
    FSvnLogFrame: TSvnLogFrame;
    FURL: string;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IAsyncUpdate }
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;
    { CallBacks }
    procedure LoadRevisionsCallBack(FirstRevision, LastRevision: Integer; Count: Integer);
    function FileColorCallBack(Action: Char): TColor;
  public
    constructor Create(SvnClient: TSvnClient; const ARootPath, AURL: string);
    destructor Destroy; override;
    function Show(ASelectedRevisions: TList<Integer>): Boolean; overload;
    procedure Show; overload;
  end;

implementation

uses
  SysUtils, Controls, SvnConst, SvnIDEConst, Forms, ToolsAPI, FileCtrl;

const
  cFileNameTag = 'fn';
  cFileNameTagOpenStr = '<' + cFileNameTag + '>';
  cFileNameTagCloseStr = '</' + cFileNameTag + '>';

{ TCustomUpdateThread }

procedure TCustomUpdateThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCustomUpdateThread.Add(const Path, Action: string; Conflicted: Boolean; TextColor: TColor);
begin
  FSyncPath := StringReplace(Path, '/', '\', [rfReplaceAll]);
  FSyncAction := Action;
  FSyncConflicted := Conflicted;
  FSyncTextColor := TextColor;
  Synchronize(nil, SyncAdd);
end;

procedure TCustomUpdateThread.CancelCallback(Sender: TObject; var Cancel: Boolean);
begin
  Cancel := FAborted;
end;

function TCustomUpdateThread.ConflictCallback(Sender: TObject;
  var ResultFileName: string; var Choice: TSvnWcConflictChoice;
  description: PSvnWcConflictDescription): Boolean;
var
  TextColor: TColor;
begin
  ResultFileName := UTF8ToString(description.path);
  Choice := SvnWcConflictChoosePostpone;
  TextColor := IDEClient.Colors.GetNotifyActionColor(svnWcNotifyUpdateUpdate, svnWcNotifyStateConflicted);
  Add(ResultFileName, sWcNotifyStateConflicted, True, TextColor);
  Result := False;
end;

constructor TCustomUpdateThread.Create(SvnIDEClient: TSvnIDEClient);
begin
  inherited Create(True);
  FSvnIDEClient := SvnIDEClient;
  FAborted := False;
  FreeOnTerminate := True;
end;

procedure TCustomUpdateThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, FSyncConflicted, FSyncTextColor);
end;

procedure TCustomUpdateThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    ShowSvnExceptionMessage(FExceptionMessage);
  FUpdateDialog.Completed;
end;

procedure TCustomUpdateThread.UpdateCallBack(Sender: TObject; const Path,
  MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum;
  var Cancel: Boolean);
var
  ActionStr: string;
  TextColor: TColor;
begin
  TextColor := IDEClient.Colors.GetNotifyActionColor(Action, ContentState);
  if ContentState = svnWcNotifyStateConflicted then
    ActionStr := sWcNotifyStateConflicted
  else
  if ContentState = svnWcNotifyStateMerged then
    ActionStr := sWcNotifyStateMerged
  else
    ActionStr := NotifyActionStr(Action);
  if Action = svnWcNotifyUpdateCompleted then
    Add(Format(sUpdateCompletedAtRevision, [Revision]), ActionStr, False, TextColor)
  else
    Add(Path, ActionStr, False, TextColor);
end;

{ TMergeThread }

constructor TMergeThread.Create(SvnIDEClient: TSvnIDEClient; AURL, APath: string; ARevision1, ARevision2: Integer);
var
  Revision1Str, Revision2Str: string;
begin
  inherited Create(SvnIDEClient);
  FURL := AURL;
  FPath := APath;
  FRevision1 := ARevision1;
  FRevision2 := ARevision2;
  if ARevision1 <> -1 then
    Revision1Str := IntToStr(ARevision1)
  else
    Revision1Str := sHead;
  if ARevision2 <> -1 then
    Revision2Str := IntToStr(ARevision2)
  else
    Revision2Str := sHead;
  FUpdateDialog := GetUpdateDialog('', AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format(sMergeDialogCaption,
    [Revision1Str, Revision2Str, AURL, StringReplace(APath, '/', '\', [rfReplaceAll])]);
  FUpdateDialog.Show;
  Resume;
end;

procedure TMergeThread.Execute;
begin
  NameThreadForDebugging('DelphiSVN Merge');
  try
    FExceptionMessage := '';
    FSvnIDEClient.SvnClient.Merge(FURL, FRevision1, FURL, FRevision2, FPath, UpdateCallBack, CancelCallback);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
  end;
  Synchronize(nil, SyncCompleted);
end;

{ TMergeRangeThread }

constructor TMergeRangeThread.Create(SvnIDEClient: TSvnIDEClient; const AURL: string;
  ARangesToMerge: TSvnMergeRevisionList; APegRevision: Integer; const ATargetWcpath: string;
  ADepth: TSvnDepth = svnDepthInfinity; AIgnoreAncestry: TSvnBoolean = False; AForce: TSvnBoolean = False;
  ARecordOnly: TSvnBoolean = False; ADryRun: TSvnBoolean = False; AIgnoreEOL: Boolean = False;
  AIgnoreSpace: Boolean = False; AIgnoreSpaceAll: Boolean = False);
var
  I: Integer;
  RevisionStr: string;
begin
  inherited Create(SvnIDEClient);
  FURL := AURL;
  FRangesToMerge := TSvnMergeRevisionList.Create;
  FRangesToMerge.Assign(ARangesToMerge);
  FPegRevision := APegRevision;
  FTargetWcpath := ATargetWcpath;
  FDepth := ADepth;
  FIgnoreAncestry := AIgnoreAncestry;
  FForce := AForce;
  FRecordOnly := ARecordOnly;
  FDryRun := ADryRun;
  FIgnoreEOL := AIgnoreEOL;
  FIgnoreSpace := AIgnoreSpace;
  FIgnoreSpaceAll := AIgnoreSpaceAll;
  RevisionStr := '';
  for I := 0 to FRangesToMerge.Count - 1 do
    if RevisionStr <> '' then
      RevisionStr := RevisionStr + ',' + Format('%d-%d', [FRangesToMerge[I].StartRevision, FRangesToMerge[I].EndRevision])
    else
      RevisionStr := Format('%d-%d', [FRangesToMerge[I].StartRevision, FRangesToMerge[I].EndRevision]);
  FDryRun := ADryRun;
  FUpdateDialog := GetUpdateDialog('', AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format(sMergeDialogCaptionRange,
    [RevisionStr, AURL, StringReplace(ATargetWcpath, '/', '\', [rfReplaceAll])]);
  FUpdateDialog.Show;
  Resume;
end;

destructor TMergeRangeThread.Destroy;
begin
  FRangesToMerge.Free;
  inherited Destroy;
end;

procedure TMergeRangeThread.Execute;
begin
  NameThreadForDebugging('DelphiSVN Merge Range');
  try
    FExceptionMessage := '';
    FSvnIDEClient.SvnClient.MergePeg(FURL, FRangesToMerge, FTargetWcpath, FPegRevision, UpdateCallBack, CancelCallback,
      FDepth, FIgnoreAncestry, FForce, FRecordOnly, FDryRun, FIgnoreEOL, FIgnoreSpace, FIgnoreSpaceAll);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
  end;
  Synchronize(nil, SyncCompleted);
end;

{ TSwitchThread }

constructor TSwitchThread.Create(SvnIDEClient: TSvnIDEClient; const APath, AURL: string; APegRevision: TSvnRevNum = -1; ARevision: TSvnRevNum = -1;
  ADepth: TSvnDepth = svnDepthInfinity; ADepthIsSticky: TSvnBoolean = False; AIgnoreExternals: TSvnBoolean = False);
begin
  inherited Create(SvnIDEClient);
  FDepth := ADepth;
  FDepthIsSticky := ADepthIsSticky;
  FIgnoreExternals := AIgnoreExternals;
  FPath := APath;
  FPegRevision := APegRevision;
  FRevision := ARevision;
  FURL := AURL;
  FUpdateDialog := GetUpdateDialog('', AbortCallBack, nil, nil);
  FUpdateDialog.Caption := Format(sSwitchDialogCaption, [SvnIDEClient.SvnClient.SvnPathToNativePath(APath)]);
  FUpdateDialog.Show;
  Resume;
end;

procedure TSwitchThread.Execute;
begin
  NameThreadForDebugging('DelphiSVN Switch');
  try
    FExceptionMessage := '';
    FSvnIDEClient.SvnClient.Switch(FPath, FURL, -1, -1, UpdateCallBack, CancelCallback);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
  end;
  Synchronize(nil, SyncCompleted);
end;

{ TCustomProgressThread }

constructor TCustomProgressThread.Create;
begin
  inherited Create(True);
  FAborted := False;
  FSvnProgressDialog := TSvnProgressDialog.Create(Application);
  FSvnProgressDialog.AbortCallBack := AbortCallBack;
  FSvnProgressDialog.Show;
  FreeOnTerminate := True;
end;

procedure TCustomProgressThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCustomProgressThread.SyncCloseDialog;
begin
  FSvnProgressDialog.Close;
end;

procedure TCustomProgressThread.SyncUpdateCaption;

  function ShortenFileNameInString(const AInput, ATag: string; ACanvas: TCanvas; AMaxWidth: Integer): string;
  var
    P1, P2, TagLength, WidthWithoutFileName: Integer;
    FileName: string;
  begin
    P1 := Pos('<' + ATag + '>', AInput);
    if P1 > 0 then
      P2 := Pos('</' + ATag + '>', AInput)
    else
      P2 := 0;
    if P2 > 0 then
    begin
      Result := AInput;
      TagLength := Length(ATag) + 2;
      FileName := Copy(Result, P1 + TagLength, P2 - P1 - TagLength);
      Delete(Result, P1, P2 - P1 + TagLength + 1);
      if FileName <> '' then
      begin
        WidthWithoutFileName := ACanvas.TextWidth(Result);
        FileName := FileCtrl.MinimizeName(FileName, ACanvas, AMaxWidth - WidthWithoutFileName);
        Insert(FileName, Result, P1);
      end;
    end
    else
      Result := AInput;
  end;

var
  S: string;
begin
  S := ShortenFileNameInString(FSyncText, cFileNameTag, FSvnProgressDialog.lbInfo.Canvas,
    FSvnProgressDialog.ProgressBar1.Width);
  FSvnProgressDialog.lbInfo.Caption := S;
end;

procedure TCustomProgressThread.SyncUpdateProgress;
begin
  FSvnProgressDialog.ProgressBar1.Max := FSyncMax;
  FSvnProgressDialog.ProgressBar1.Position := FSyncPosition;
end;

procedure TCustomProgressThread.UpdateCaption(const AText: string);
begin
  FSyncText := AText;
  Synchronize(SyncUpdateCaption);
end;

procedure TCustomProgressThread.UpdateProgress(APosition, AMax: Integer);
begin
  FSyncPosition := APosition;
  FSyncMax := AMax;
  Synchronize(SyncUpdateProgress);
end;

{ TCompareRevisionThread.TCompareFile }

constructor TCompareRevisionThread.TCompareFile.Create(const AFileName: string; ARevision1,
  ARevision2: Integer);
begin
  FFileName := AFileName;
  FRevision1 := ARevision1;
  FRevision2 := ARevision2;
end;

{ TCompareRevisionThread }

constructor TCompareRevisionThread.Create;
begin
  inherited Create;
  FFiles := TObjectList<TCompareFile>.Create;
end;

destructor TCompareRevisionThread.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TCompareRevisionThread.AddFile(const AFileName: string; ARevision1,
  ARevision2: Integer);
begin
  FFiles.Add(TCompareFile.Create(AFileName, ARevision1, ARevision2));
end;

procedure TCompareRevisionThread.Execute;
var
  I: Integer;
  FileName: string;
  MemStream: TMemoryStream;
begin
  for I := 0 to FFiles.Count - 1 do
  begin
    FileName := StringReplace(FFiles[I].FileName, '/', '\', [rfReplaceAll]);
    UpdateCaption(Format(sRetrievingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, FFiles[I].Revision1]));
    MemStream := TMemoryStream.Create;
    FFiles[I].Stream1 := TStreamAdapter.Create(MemStream, soOwned);
    IDEClient.SvnClient.SaveFileContentToStream(FFiles[I].FileName, FFiles[I].Revision1, MemStream);
    UpdateProgress(I * 2 + 1, FFiles.Count * 2);
    if FAborted then
      Break;
    UpdateCaption(Format(sRetrievingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, FFiles[I].Revision2]));
    MemStream := TMemoryStream.Create;
    FFiles[I].Stream2 := TStreamAdapter.Create(MemStream, soOwned);
    IDEClient.SvnClient.SaveFileContentToStream(FFiles[I].FileName, FFiles[I].Revision2, MemStream);
    UpdateProgress(I * 2 + 2, FFiles.Count * 2);
    if FAborted then
      Break;
  end;
  Synchronize(SyncCallCompare);
end;

procedure TCompareRevisionThread.SyncCallCompare;
var
  I: Integer;
begin
  FSvnProgressDialog.Free;
  if not FAborted then
    for I := 0 to FFiles.Count - 1 do
    begin
      (BorlandIDEServices as IOTACustomDifferenceManager).
        ShowDifference(FFiles[I].Stream1, FFiles[I].Stream2, FFiles[I].FileName + '-' + IntToStr(FFiles[I].Revision1),
          FFiles[I].FileName  + '-' + IntToStr(FFiles[I].Revision2), '', '', dfOTARevision, dfOTARevision, dtOTADefault);
      FFiles[I].Stream1 := nil;
      FFiles[I].Stream2 := nil;
    end;
end;

{ TSaveRevisionThread }

constructor TSaveRevisionThread.Create(ARevision: Integer; const APath: string);
begin
  inherited Create;
  FFiles := TStringList.Create;
  FPath := APath;
  FRevision := ARevision;
end;

destructor TSaveRevisionThread.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TSaveRevisionThread.AddFile(const AFileName: string);
begin
  FFiles.Add(AFileName);
end;

procedure TSaveRevisionThread.Execute;
var
  I: Integer;
  FileName, DestFileName: string;
  FileStream: TFileStream;
begin
  for I := 0 to FFiles.Count - 1 do
  begin
    FileName := StringReplace(FFiles[I], '/', '\', [rfReplaceAll]);
    UpdateCaption(Format(sSavingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, FRevision]));
    DestFileName := IncludeTrailingPathDelimiter(FPath) + ExtractFileName(FileName);
    DestFileName := ChangeFileExt(DestFileName, Format('-%d%s', [FRevision, ExtractFileExt(DestFileName)]));
    FileStream := TFileStream.Create(DestFileName, fmCreate);
    try
      IDEClient.SvnClient.SaveFileContentToStream(FFiles[I], FRevision, FileStream);
    finally
      FileStream.Free;
    end;
    UpdateProgress(I + 1, FFiles.Count);
    if FAborted then
      Break;
  end;
  Synchronize(SyncCloseDialog);
end;

{ TLogDialogHelper }

constructor TLogDialogHelper.Create(SvnClient: TSvnClient; const ARootPath, AURL: string);
begin
  inherited Create;
  FSvnClient := SvnClient;
  FRootPath := ARootPath;
  FURL := AURL;
end;

destructor TLogDialogHelper.Destroy;
begin
  inherited Destroy;
end;

function TLogDialogHelper.FileColorCallBack(Action: Char): TColor;
begin
  Result := IDEClient.Colors.GetLogActionColor(Action);
end;

procedure TLogDialogHelper.Completed;
begin
  if Assigned(FSvnLogFrame) then
    FSvnLogFrame.NextCompleted;
end;

procedure TLogDialogHelper.LoadRevisionsCallBack(FirstRevision, LastRevision,
  Count: Integer);
begin
  if FirstRevision = -1 then
    FFirst := 0;
  FSvnItem.LogLimit := Count;
  FSvnItem.LogFirstRev := FirstRevision;
  FSvnItem.LogLastRev := LastRevision;
  FSvnItem.AsyncUpdate := Self;
  FSvnLogFrame.StartAsync;
  FSvnItem.AsyncReloadHistory;
end;

function TLogDialogHelper.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TLogDialogHelper.Show;
begin
  Show(nil);
end;

function TLogDialogHelper.Show(ASelectedRevisions: TList<Integer>): Boolean;
var
  I, StartIdx: Integer;
  Status: TSvnWcStatus2;
begin
  Status.entry := nil;
  Status.repos_lock := nil;
  FLogDialog := TSvnLogDialog.Create(Application);
  try
    FSvnLogFrame := FLogDialog.LogFrame;
    FSvnLogFrame.FileColorCallBack := FileColorCallBack;
    FSvnLogFrame.LoadRevisionsCallBack := LoadRevisionsCallBack;
    if FRootPath <> '' then
    begin
      FLogDialog.Caption := Format('%s %s', [sLog, FRootPath]);
      FSvnItem := TSvnItem.Create(FSvnClient, nil, FRootPath, True);
    end
    else
    begin
      FLogDialog.Caption := Format('%s %s', [sLog, FURL]);
      FSvnItem := TSvnItem.Create(FSvnClient, FURL);
    end;
    FSvnItem.AsyncUpdate := Self;
    FSvnItem.IncludeChangeFiles := True;
    FSvnItem.LogLimit := DefaultRange;
    Application.ProcessMessages;
    FSvnLogFrame.StartAsync;
    FSvnItem.AsyncReloadHistory;
    Result := FLogDialog.ShowModal = mrOk;
    if Result and Assigned(ASelectedRevisions) then
    begin
      ASelectedRevisions.Clear;
      if FSvnLogFrame.Revisions.SelCount > 0 then
        begin
          StartIdx := FSvnLogFrame.Revisions.Selected.Index;
          for I := StartIdx to FSvnLogFrame.Revisions.Items.Count - 1 do
            if FSvnLogFrame.Revisions.Items[I].Selected then
              ASelectedRevisions.Insert(0, StrToInt(FSvnLogFrame.Revisions.Items[I].Caption));
        end;
    end;
  finally
    FLogDialog.Free;
  end;
end;

procedure TLogDialogHelper.UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex,
  LastNewIndex: Integer; ForceUpdate: Boolean);
var
  I: Integer;
  HistoryItem: TSvnHistoryItem;
  CanUpdate: Boolean;
  BugID: string;
begin
  CanUpdate := (FirstNewIndex = 0) or ((LastNewIndex - FFirst <> 0 ) and ((LastNewIndex - FFirst) Mod 20 = 0));
  if CanUpdate or ForceUpdate then
  begin
    FSvnLogFrame.BeginUpdate;
    try
      for I := FFirst to LastNewIndex do
      begin
        HistoryItem := SvnItem.HistoryItems[I];
        {//TODO: Add bug ID parser
        if FBugIDParser.BugTraqLogRegEx <> '' then
          BugID := FBugIDParser.GetBugID(HistoryItem.LogMessage)
        else
        }
          BugID := '';
        FSvnLogFrame.AddRevisions(HistoryItem.Revision, HistoryItem.Time,
          HistoryItem.Author, HistoryItem.LogMessage, BugID, HistoryItem.ChangeFiles);
      end;
    finally
      FSvnLogFrame.EndUpdate;
    end;
    FFirst := LastNewIndex + 1;
    Application.ProcessMessages;
  end;
end;

function TLogDialogHelper._AddRef: Integer;
begin
  Result := -1;
end;

function TLogDialogHelper._Release: Integer;
begin
  Result := -1;
end;

end.
