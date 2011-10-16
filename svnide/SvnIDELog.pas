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
{ The Original Code is delphisvn: Subversion plugin for CodeGear Delphi.       }
{                                                                              }
{ The Initial Developer of the Original Code is Embarcadero Technologies.      }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights      }
{ reserved.                                                                    }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Ondrej Kelle (tondrej)                                                       }
{ Uwe Schuster (uschuster)                                                     }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
unit SvnIDELog;

interface

uses Classes, SvnIDEMenus, SvnIDEClient;

type
  TBaseLogSvnMenu = class(TSvnMenu)
  protected
    FRootType: TRootType;
    FSvnIDEClient: TSvnIDEClient;
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentLogSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirLogSvnMenu = class(TBaseLogSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TProjectDirLogSvnMenu = class(TBaseLogSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirLogSvnMenu = class(TBaseLogSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

 procedure Register;

implementation

uses SysUtils, SvnIDEConst, ToolsApi, SvnClientLog, SvnClient, DesignIntf, Forms,
  SvnUITypes, SvnIDEUtils, ExtCtrls, Graphics, SvnIDETypes, RegularExpressions,
  SvnIDEIcons;

const
  sPMVLogParent = 'SvnLogParent';
  sLogView = 'LogView';
  sPMVRootDirLog = 'RootDirLog';
  sPMVProjectDirLog = 'ProjectDirLog';
  sPMVExpicitFilesLog = 'ExpicitFilesLog';
  sPMVDirLog = 'DirLog';

var
  LogView: INTACustomEditorView;

type
  TLogView = class(TInterfacedObject, INTACustomEditorView,
    INTACustomEditorView150, IAsyncUpdate)
  protected
    type
      TBugIDParser = class(TObject)
      private
        FBugTraqLogRegEx: string;
        FRegEx1: TRegEx;
        FRegEx2: TRegEx;
        procedure SetBugTraqLogRegEx(const Value: string);
      public
        constructor Create;
        function GetBugID(const ALogMessage: string): string;
        property BugTraqLogRegEx: string read FBugTraqLogRegEx write SetBugTraqLogRegEx;
      end;
    var
      FBugIDParser: TBugIDParser;
      FSvnClient: TSvnClient;
      FSvnLogFrame: TSvnLogFrame;
      FSvnItem: TSvnItem;
      FRootPath: string;
      FFirst: Integer;
    { INTACustomEditorView }
    function GetCanCloneView: Boolean;
    function CloneEditorView: INTACustomEditorView;
    function GetCaption: string;
    function GetEditorWindowCaption: string;
    function GetViewIdentifier: string;
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    procedure CloseAllCalled(var ShouldClose: Boolean);
    procedure SelectView;
    procedure DeselectView;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    { INTACustomEditorView150 }
    function GetImageIndex: Integer;
    function GetTabHintText: string;
    procedure Close(var Allowed: Boolean);
    { AsyncUpdate }
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;
    { CallBacks }
    procedure LoadRevisionsCallBack(FirstRevision, LastRevision: Integer; Count: Integer);
    function FileColorCallBack(Action: Char): TColor;
    procedure ReverseMergeCallBack(const APathName: string; ARevision1, ARevision2: Integer);
    procedure CompareRevisionCallBack(AFileList: TStringList; ARevision1, ARevision2: Integer);
    procedure SaveRevisionCallBack(AFileList: TStringList; ARevision: Integer; const ADestPath: string);
    function UpdateLogMessageCallBack(ARevision: Integer; const ALogMessage: string): Boolean;
    { Misc }
    function GetBugTraqLogRegEx(APath: string): string;
  public
    constructor Create(SvnClient: TSvnClient; const ARootPath: string);
    destructor Destroy; override;
  end;

  TSvnURLToFileNameTranslator = class(TObject)
  private
    FRootURL: string;
    FSvnClient: TSvnClient;
    FURL: string;
    FURLPath: string;
    function GetBaseURL(const AURL1, AURL2: string): string;
  public
    constructor Create(ASvnClient: TSvnClient; const AURLPath: string);
    function GetTranslatedFileName(const ATrailingURLPath: string): string;
  end;

{ TBaseLogSvnMenu }

constructor TBaseLogSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FParent := sPMVLogParent;
  FSvnIDEClient := ASvnIDEClient;
end;

procedure TBaseLogSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  RootPath: string;
  MenuContext: IOTAMenuContext;
begin
  if Supports(MenuContextList[0], IOTAMenuContext, MenuContext) then
  begin
    if FRootType = rtRootDir then
      RootPath := RootDirectory(FSvnIDEClient.SvnClient, MenuContext.Ident)
    else
    if FRootType = rtProjectDir then
      RootPath := ExtractFilePath(MenuContext.Ident)
    else
    if FRootType = rtDir then
      RootPath := IncludeTrailingPathDelimiter(MenuContext.Ident)
    else
      RootPath := '';
    //TODO: check if path is not empty and is versioned
    //TODO: check if there is already a Log view
    // (otherwise you will currently receive an "A component named SvnLogFrame already exists." exception)
    LogView := TLogView.Create(FSvnIDEClient.SvnClient, RootPath);
    (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(LogView);
  end;
end;

{ TParentLogSvnMenu }

constructor TParentLogSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMLog;
  FVerb := sPMVLogParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentLogSvnMenu;
  FHelpContext := 0;
end;

function TParentLogSvnMenu.GetImageIndex: Integer;
begin
  Result := LogImageIndex;
end;

{ TRootDirLogSvnMenu }

constructor TRootDirLogSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirLog;
  FPosition := pmmpRootDirLogSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirLogSvnMenu }

constructor TProjectDirLogSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirLog;
  FPosition := pmmpProjectDirLogSvnMenu;
  FHelpContext := 0;
end;

{ TDirLogSvnMenu }

constructor TDirLogSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMLog;
  FVerb := sPMVDirLog;
  FPosition := pmmpProjectDirLogSvnMenu;
  FHelpContext := 0;
end;

function TDirLogSvnMenu.GetImageIndex: Integer;
begin
  Result := LogImageIndex;
end;

{ TSvnURLToFileNameTranslator }

constructor TSvnURLToFileNameTranslator.Create(ASvnClient: TSvnClient; const AURLPath: string);
begin
  inherited Create;
  FSvnClient := ASvnClient;
  FURL := FSvnClient.FindRepository(AURLPath);
  FRootURL := FSvnClient.FindRepositoryRoot(AURLPath);
  FURLPath := SvnExcludeTrailingPathDelimiter(FSvnClient.NativePathToSvnPath(AURLPath));
end;

function TSvnURLToFileNameTranslator.GetBaseURL(const AURL1, AURL2: string): string;
var
  I, L, L1, L2: Integer;
begin
  L := 0;
  L1 := Length(AURL1);
  L2 := Length(AURL2);
  for I := 1 to L1 do
    if (L2 >= I) and (AURL1[I] = AURL2[I]) then
    begin
      if (AURL1[I] = '/') or (I = L1) or (I = L2) then
        L := I;
    end
    else
      Break;
  if L > 0 then
    Result := Copy(AURL1, 1, L)
  else
    Result := '';
end;

function TSvnURLToFileNameTranslator.GetTranslatedFileName(const ATrailingURLPath: string): string;
var
  BaseURL, BaseURLPath, PathName: string;
begin
  if FURL <> FRootURL then
  begin
    PathName := ATrailingURLPath;
    BaseURL := GetBaseURL(FURL, FRootURL + PathName);
    BaseURLPath := Copy(FURLPath, 1, Length(FURLPath) + Length(BaseURL) - Length(FURL));
    System.Delete(PathName, 1, Length(BaseURL) - Length(FRootURL));
    Result := BaseURLPath + PathName;
  end
  else
    Result := FURLPath + ATrailingURLPath;
end;

{ TLogView }

function TLogView.CloneEditorView: INTACustomEditorView;
begin
  Result := nil;
end;

procedure TLogView.Close(var Allowed: Boolean);
begin

end;

procedure TLogView.CloseAllCalled(var ShouldClose: Boolean);
begin
  ShouldClose := True;
end;

procedure TLogView.CompareRevisionCallBack(AFileList: TStringList; ARevision1,
  ARevision2: Integer);
var
  I: Integer;
  CompareRevisionThread: TCompareRevisionThread;
  FileName: string;
  SvnURLToFileNameTranslator: TSvnURLToFileNameTranslator;
begin
  CompareRevisionThread := TCompareRevisionThread.Create;
  SvnURLToFileNameTranslator := TSvnURLToFileNameTranslator.Create(IDEClient.SvnClient, FRootPath);
  try
    for I := 0 to AFileList.Count - 1 do
    begin
      FileName := SvnURLToFileNameTranslator.GetTranslatedFileName(AFileList[I]);
      CompareRevisionThread.AddFile(FileName, ARevision1, ARevision2);
    end;
  finally
    SvnURLToFileNameTranslator.Free;
  end;
  CompareRevisionThread.Start;
end;

procedure TLogView.Completed;
begin
  if Assigned(FSvnLogFrame) then
    FSvnLogFrame.NextCompleted;
end;

constructor TLogView.Create(SvnClient: TSvnClient; const ARootPath: string);
begin
  inherited Create;
  FSvnClient := SvnClient;
  FRootPath := ARootPath;
  FBugIDParser := TBugIDParser.Create;
  FBugIDParser.BugTraqLogRegEx := GetBugTraqLogRegEx(ARootPath);
end;

procedure TLogView.DeselectView;
begin
  // Not used
end;

destructor TLogView.Destroy;
begin
  FBugIDParser.Free;
  if FSvnItem <> nil then
    FSvnItem.Free;
  inherited;
end;

function TLogView.EditAction(Action: TEditAction): Boolean;
var
  SvnEditAction: TSvnEditAction;
begin
  Result := False;
  if Assigned(FSvnLogFrame) then
  begin
    SvnEditAction := EditActionToSvnEditAction(Action);
    if SvnEditAction <> seaUnknown then
      Result := FSvnLogFrame.PerformEditAction(SvnEditAction);
  end;
end;

function TLogView.FileColorCallBack(Action: Char): TColor;
begin
  Result := IDEClient.Colors.GetLogActionColor(Action);
end;

procedure TLogView.FrameCreated(AFrame: TCustomFrame);
var
  URL, RootURL, RootRelativePath: string;
begin
  FSvnLogFrame := TSvnLogFrame(AFrame);
  FSvnLogFrame.FileColorCallBack := FileColorCallBack;
  FSvnLogFrame.LoadRevisionsCallBack := LoadRevisionsCallBack;
  FSvnLogFrame.ReverseMergeCallBack := ReverseMergeCallBack;
  FSvnLogFrame.CompareRevisionCallBack := CompareRevisionCallBack;
  FSvnLogFrame.SaveRevisionCallBack := SaveRevisionCallBack;
  FSvnLogFrame.UpdateLogMessageCallBack := UpdateLogMessageCallBack;
  FSvnLogFrame.RootPath := ExcludeTrailingPathDelimiter(FRootPath);
  URL := IDEClient.SvnClient.FindRepository(FRootPath);
  RootURL := IDEClient.SvnClient.FindRepositoryRoot(FRootPath);
  if URL <> RootURL then
    RootRelativePath := Copy(URL, Length(RootURL) + 1, MaxInt)
  else
    RootRelativePath := '';
  FSvnLogFrame.RootRelativePath := RootRelativePath;
  FSvnLogFrame.ShowBugIDColumn := FBugIDParser.BugTraqLogRegEx <> '';
  FSvnItem := TSvnItem.Create(FSvnClient, nil, FRootPath, True);
  FSvnItem.AsyncUpdate := Self;
  FSvnItem.IncludeChangeFiles := True;
  FSvnItem.LogLimit := DefaultRange;
  Application.ProcessMessages;
  FSvnLogFrame.StartAsync;
  FSvnItem.AsyncReloadHistory;
  FSvnLogFrame.BaseRevision := IntToStr(IDEClient.SvnClient.GetMaxRevision(FRootPath));
end;

function TLogView.GetBugTraqLogRegEx(APath: string): string;
var
  I: Integer;
  SvnItem: TSvnItem;
begin
  while (Result = '') and FSvnClient.IsPathVersioned(APath) do
  begin
    SvnItem := TSvnItem.Create(FSvnClient, nil, APath);
    try
      SvnItem.PropValDelimiter := #10;
      for I := 0 to SvnItem.PropCount - 1 do
        if SvnItem.PropNames[I] = 'bugtraq:logregex' then // do not localize
        begin
          Result := SvnItem.PropValueFromIndex[I];
          Break;
        end;
    finally
      SvnItem.Free;
    end;
    if Result = '' then
      APath := ExtractFilePath(ExcludeTrailingPathDelimiter(APath));
  end;
end;

function TLogView.GetCanCloneView: Boolean;
begin
  Result := False;
end;

function TLogView.GetCaption: string;
begin
  Result := sLog;
end;

function TLogView.GetEditorWindowCaption: string;
begin
  Result := sLog;
end;

function TLogView.GetEditState: TEditState;
begin
  Result := [];
  if Assigned(FSvnLogFrame) then
    Result := SvnEditStateToEditState(FSvnLogFrame.SvnEditState);
end;

function TLogView.GetFrameClass: TCustomFrameClass;
begin
  Result := TSvnLogFrame;
end;

function TLogView.GetImageIndex: Integer;
begin
  Result := 0;
end;

function TLogView.GetTabHintText: string;
begin
  Result := '';
  //TODO: provide a custom hint for the LogView tab
end;

function TLogView.GetViewIdentifier: string;
begin
  Result := sLogView;
end;

procedure TLogView.LoadRevisionsCallBack(FirstRevision, LastRevision, Count: Integer);
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

procedure TLogView.ReverseMergeCallBack(const APathName: string; ARevision1,
  ARevision2: Integer);
var
  URL, RootURL, Path, PathName: string;
begin
  if APathName = '' then
  begin
    URL := IDEClient.SvnClient.FindRepository(FRootPath);
    Path := SvnExcludeTrailingPathDelimiter(IDEClient.SvnClient.NativePathToSvnPath(FRootPath));
  end
  else
  begin
    URL := IDEClient.SvnClient.FindRepository(FRootPath);
    RootURL := IDEClient.SvnClient.FindRepositoryRoot(FRootPath);
    PathName := APathName;
    if URL <> RootURL then
      Delete(PathName, 1, Length(URL) - Length(RootURL));
    Path := SvnExcludeTrailingPathDelimiter(IDEClient.SvnClient.NativePathToSvnPath(FRootPath));
    URL := URL + PathName;
    Path := Path + PathName;
  end;
  TMergeThread.Create(IDEClient, URL, Path, ARevision1, ARevision2);
end;

procedure TLogView.SaveRevisionCallBack(AFileList: TStringList;
  ARevision: Integer; const ADestPath: string);
var
  I: Integer;
  SaveRevisionThread: TSaveRevisionThread;
  FileName: string;
  SvnURLToFileNameTranslator: TSvnURLToFileNameTranslator;
begin
  SaveRevisionThread := TSaveRevisionThread.Create(ARevision, ADestPath);
  SvnURLToFileNameTranslator := TSvnURLToFileNameTranslator.Create(IDEClient.SvnClient, FRootPath);
  try
    for I := 0 to AFileList.Count - 1 do
    begin
      FileName := SvnURLToFileNameTranslator.GetTranslatedFileName(AFileList[I]);
      SaveRevisionThread.AddFile(FileName);
    end;
  finally
    SvnURLToFileNameTranslator.Free;
  end;
  SaveRevisionThread.Start;
end;

procedure TLogView.SelectView;
begin
  // Not used
end;

procedure TLogView.UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
  ForceUpdate: Boolean);
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
        if FBugIDParser.BugTraqLogRegEx <> '' then
          BugID := FBugIDParser.GetBugID(HistoryItem.LogMessage)
        else
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

function TLogView.UpdateLogMessageCallBack(ARevision: Integer; const ALogMessage: string): Boolean;
var
  URL, RootURL: string;
begin
  Result := False;
  try
    URL := IDEClient.SvnClient.FindRepository(FRootPath);
    RootURL := IDEClient.SvnClient.FindRepositoryRoot(FRootPath);
    IDEClient.SvnClient.SetRevisionProperty(RootURL, ARevision, 'svn:log', ALogMessage);
    Result := True;
  except
    if not HandleSvnException(ExceptObject) then
      raise;
  end;
end;

{ TLogView.TBugIDParser }

constructor TLogView.TBugIDParser.Create;
begin
  FRegEx1 := TRegEx.Create('');
  FRegEx2 := TRegEx.Create('');
end;

function TLogView.TBugIDParser.GetBugID(const ALogMessage: string): string;
var
  Match, MatchNr: TMatch;
  BugIDs: TStringList;
begin
  BugIDs := TStringList.Create;
  try
    BugIDs.Duplicates := dupIgnore;
    BugIDs.Sorted := True;
    Match := FRegEx1.Match(ALogMessage);
    while Match.Success do
    begin
      MatchNr := FRegEx2.Match(Match.Value);
      BugIDs.Add(MatchNr.Value);
      Match := Match.NextMatch;
    end;
    Result := TrimRight(StringReplace(BugIDs.Text, #13#10, ' ', [rfReplaceAll]));
  finally
    BugIDs.Free;
  end;
end;

procedure TLogView.TBugIDParser.SetBugTraqLogRegEx(const Value: string);
var
  P: Integer;
begin
  if FBugTraqLogRegEx <> Value then
  begin
    P := Pos(#10, Value);
    if P > 0 then
    begin
      FBugTraqLogRegEx := Value;
      FRegEx1 := TRegEx.Create(Copy(Value, 1, P - 1));
      FRegEx2 := TRegEx.Create(Copy(Value, P + 1, MaxInt));
    end;
  end;
end;

function GetView: INTACustomEditorView;
begin
  Result := LogView;
end;

procedure Register;
begin
  (BorlandIDEServices as IOTAEditorViewServices).RegisterEditorView(sLogView, GetView);
end;

initialization
finalization
  (BorlandIDEServices as IOTAEditorViewServices).UnRegisterEditorView(sLogView);
end.
