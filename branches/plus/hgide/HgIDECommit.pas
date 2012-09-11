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
unit HgIDECommit;

interface

uses Classes, HgIDEMenus, HgIDEClient, HgClient;

type
  TBaseCommitHgMenu = class(THgMenu)
  protected
    FRootType: TRootType;
    FSvnIDEClient: THgIDEClient;
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  TParentCommitHgMenu = class(THgMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirCommitHgMenu = class(TBaseCommitHgMenu)
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  TProjectDirCommitHgMenu = class(TBaseCommitHgMenu)
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  TExpicitFilesCommitHgMenu = class(TBaseCommitHgMenu)
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  TFileCommitHgMenu = class(TBaseCommitHgMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  TDirCommitHgMenu = class(TBaseCommitHgMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: THgIDEClient);
  end;

  procedure DoCommit(const SvnClient: THgClient; const CommitList: TStringList;
    const Comment: string; const RecentComments: TStringList; ADeleteLocalHistory: Boolean);

  procedure LoadRecentComments(const RecentComments:TStringList);

  procedure SaveRecentComments(const RecentComments:TStringList);

  procedure Register;

implementation

uses SysUtils, ToolsApi, Forms, DesignIntf, ComCtrls, Controls, HgIDEConst,
  HgClientCommitFrame, {svn_client, }FileHistoryAPI, IStreams,
  ActiveX, Dialogs, {SvnIDEClean,} HgIDEMessageView, Registry, HgUITypes,
  HgIDEUtils, Graphics, IOUtils, Types, HgIDEIcons, HgIDEFileStates;

const
  sPMVCommit = 'Commit';
  sSvnCommitView = 'HgCommitView';
  sPMVCommitParent = 'CommitParent';
  sPMVRootDirCommit = 'RootDirCommit';
  sPMVProjectDirCommit = 'ProjectDirCommit';
  sPMVExpicitFilesCommit = 'ExpicitFilesCommit';
  sPMVDirCommit = 'DirCommit';
  sSubversion = 'Subversion';
  sRecentComments = 'RecentComments';
  sComment = 'Comment%d';
  MaxRecentComments = 15;

var
  CommitView: INTACustomEditorView;

type
  TCommit = class(TInterfacedObject, INTACustomEditorView, INTACustomEditorView150)
  protected
    type
      TRefreshProc = procedure(const SvnListItem: TSvnListViewItem) of object;
    var
      FDirectoryList: TStringList;
      FSvnClient: THgClient;
      FSvnCommitFrame: THgCommitFrame;
      FRootType: TRootType;
      FFoundMissing: Boolean;
      FStatusItem: PSvnListViewItem;
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

    { CallBacks }
    procedure ModificationCallBack(Sender: TObject; Item: THgItem;
      var Cancel: Boolean);
    procedure CommitCallBack(const CommitList: TStringList;
      const Comment: string; const RecentComments: TStringList);
    procedure DiffCallBack(const FileName: string);
    function FileColorCallBack(AItem: TSvnListViewItem): TColor;
    function RevertCallBack(const FileName: string; ARecursive: Boolean; var ANewTextStatus: THgStatus): Boolean;
    procedure CloseCallBack;
    function AddCallBack(const FileName: string): Boolean;
    procedure ResolveCallBack(const FileName: string);
    procedure GetFileStatusCallBack(const FileName: string; var SvnListViewItem: TSvnListViewItem);
    procedure StatusCallBack(Sender: TObject; Item: THgItem; var Cancel: Boolean);
    procedure RefreshCallBack;
    procedure ModificationRefreshCallBack(Sender: TObject; Item: THgItem;
      var Cancel: Boolean);
    { Misc }
    procedure PrepareFileList(FrameAdd: TRefreshProc; AModificationCallBack: THgStatusCallback; var AURL: string);
  public
    constructor Create(SvnClient: THgClient; const DirectoryList: TStringList;
      RootType: TRootType);
    destructor Destroy; override;
  end;

{ TBaseCommitHgMenu }

constructor TBaseCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited;
  FCaption := sPMMCommit;
  FVerb := sPMVCommit;
  FParent := sPMVCommitParent;
  FHelpContext := 0;
  FSvnIDEClient := ASvnIDEClient;
  CommitView := nil;
  FRootType := rtRootDir;
end;

procedure TBaseCommitHgMenu.Execute(const MenuContextList: IInterfaceList);
var
  DirectoryList: TStringList;
  ProjectFound: Boolean;
begin
  if CommitView <> nil then
  begin
    (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(CommitView);
    MessageDlg(sCommitLoaded, mtWarning, [mbOK], 0);
  end
  else
  begin
  DirectoryList := TStringList.Create;
  try
    BuildFileList(MenuContextList, DirectoryList, FSvnIDEClient.HgClient, FRootType, ProjectFound);
    CommitView := TCommit.Create(FSvnIDEClient.HgClient, DirectoryList, FRootType);
    (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(CommitView);
  finally
    DirectoryList.Free;
  end;
end;
end;

{ TParentCommitHgMenu }

constructor TParentCommitHgMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMCommit;
  FVerb := sPMVCommitParent;
  FParent := sPMVHgParent;
  FPosition := pmmpParentCommitSvnMenu;
  FHelpContext := 0;
end;

function TParentCommitHgMenu.GetImageIndex: Integer;
begin
  Result := CommitImageIndex;
end;

{ TRootDirCommitHgMenu }

constructor TRootDirCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVCommitParent;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirCommit;
  FPosition := pmmpRootDirCommitSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirCommitHgMenu }

constructor TProjectDirCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FParent := sPMVCommitParent;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirCommit;
  FPosition := pmmpProjectDirCommitSvnMenu;
  FHelpContext := 0;
end;

{ TExpicitFilesCommitHgMenu }

constructor TExpicitFilesCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtExpicitFiles;
  FParent := sPMVCommitParent;
  FCaption := sPMMExpicitFiles;
  FVerb := sPMVExpicitFilesCommit;
  FPosition := pmmpExpicitFilesCommitSvnMenu;
  FHelpContext := 0;
end;

{ TFileCommitHgMenu }

constructor TFileCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVHgParent;
  FCaption := sPMMCommit;
  FVerb := sPMVCommit;
  FPosition := pmmpFileCommitSvnMenu;
  FHelpContext := 0;
end;

function TFileCommitHgMenu.GetImageIndex: Integer;
begin
  Result := CommitImageIndex;
end;

{ TDirCommitHgMenu }

constructor TDirCommitHgMenu.Create(ASvnIDEClient: THgIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVHgParent;
  FCaption := sPMMCommit;
  FVerb := sPMVDirCommit;
  FPosition := pmmpFileCommitSvnMenu;
  FHelpContext := 0;
end;

function TDirCommitHgMenu.GetImageIndex: Integer;
begin
  Result := CommitImageIndex;
end;

{ TCommit }

function TCommit.AddCallBack(const FileName: string): Boolean;
begin
  Result := FSvnClient.Add(FileName);
  FlushFileState(FileName);
end;

function TCommit.CloneEditorView: INTACustomEditorView;
begin
  Result := nil;
end;

procedure TCommit.Close(var Allowed: Boolean);
begin
  Allowed := True;
  CommitView := nil;
end;

procedure TCommit.CloseAllCalled(var ShouldClose: Boolean);
begin
  ShouldClose := True;
end;

procedure TCommit.CloseCallBack;
begin
  (BorlandIDEServices as IOTAEditorViewServices).CloseActiveEditorView;
end;

procedure TCommit.CommitCallBack(const CommitList: TStringList;
  const Comment: string; const RecentComments: TStringList);
begin
  DoCommit(FSvnClient, CommitList, Comment, RecentComments, IDEClient.Options.DeleteBackupFilesAfterCommit);
end;

constructor TCommit.Create(SvnClient: THgClient; const DirectoryList: TStringList;
  RootType: TRootType);
begin
  inherited Create;
  FSvnClient := SvnClient;
  FDirectoryList := TStringList.Create;
  FDirectoryList.Assign(DirectoryList);
  FRootType := RootType;
  SaveAll;
end;

procedure TCommit.DeselectView;
begin
  // Not used
end;

destructor TCommit.Destroy;
begin
  FDirectoryList.Free;
  inherited;
end;

procedure TCommit.DiffCallBack(const FileName: string);
var
  SvnItem: THgItem;
  Stream1: IStream;
  Stream2: IStream;
  TempStream: IStream;
  Rev: Integer;
  Flag2: TOTADiffFlag;
  StreamLength: Largeint;
  Dummy: Largeint;
begin
  SvnItem := THgItem.Create(FSvnClient, {nil, }FileName{, False, False, True});
  try
    Rev := SvnItem.BaseChangeSetID;
    Stream1 := TIStreamAdapter.Create(TStringStream.Create(SvnItem.GetBaseFile), soOwned);
  finally
    SvnItem.Free;
  end;
  TempStream := TIFileStream.Create(FileName, fmShareDenyNone);
  Stream2 := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
  TempStream.Seek(0, STREAM_SEEK_END, StreamLength);
  TempStream.Seek(0, STREAM_SEEK_SET, Dummy);
  TempStream.CopyTo(Stream2 as IStream, StreamLength, Dummy, Dummy);
  TempStream := nil;
  if SameText(ExtractFileExt(FileName), '.dproj') or SameText(ExtractFileExt(FileName), '.cbproj') then
    Flag2 := dfOTAFile
  else
    Flag2 := dfOTABuffer;
  (BorlandIDEServices as IOTACustomDifferenceManager).
    ShowDifference(Stream1, Stream2, FileName + '-' + IntToStr(Rev),
      FileName + sWorking, '', FileName, dfOTARevision, Flag2, dtOTADefault);
  Stream1 := nil;
  Stream2 := nil;
end;

function TCommit.EditAction(Action: TEditAction): Boolean;
var
  SvnEditAction: TSvnEditAction;
begin
  Result := False;
  if Assigned(FSvnCommitFrame) then
  begin
    SvnEditAction := EditActionToSvnEditAction(Action);
    if SvnEditAction <> seaUnknown then
      Result := FSvnCommitFrame.PerformEditAction(SvnEditAction);
  end;
end;

function TCommit.FileColorCallBack(AItem: TSvnListViewItem): TColor;
begin
  Result := IDEClient.Colors.GetStatusColor(AItem.TextStatus);
end;

procedure TCommit.PrepareFileList(FrameAdd: TRefreshProc; AModificationCallBack: THgStatusCallback; var AURL: string);
{

  // make sure that all files/paths are versioned
  procedure CheckVersionProjectFiles(AFilesAndDirectoriesInRepo,
    AddedDirectories, AUnversionedFilesAndDirectories: TStringList; AutoAdd: Boolean = False);
  var
    I, J, Idx, StartUnversionedIdx: Integer;
    S: string;
    TestPaths, VersionedPaths: TStringList;
  begin
    VersionedPaths := TStringList.Create;
    try
      VersionedPaths.Sorted := True;
      for I := AFilesAndDirectoriesInRepo.Count - 1 downto 0 do
        if not FSvnClient.IsPathVersioned(AFilesAndDirectoriesInRepo[I]) then
        begin
          TestPaths := TStringList.Create;
          try
            S := ExtractFilePath(AFilesAndDirectoriesInRepo[I]);
            while Pos(PathDelim, S) > 0 do
            begin
              Delete(S, Length(S), 1);
              TestPaths.Add(S);
              Idx := LastDelimiter(PathDelim, S);
              if Idx > 0 then
                Delete(S, Idx + 1, Length(S) - Idx);
            end;
            TestPaths.Sort;
            StartUnversionedIdx := -1;
            for J := TestPaths.Count - 1 downto 0 do
              if VersionedPaths.IndexOf(TestPaths[J]) <> -1 then
              begin
                StartUnversionedIdx := J + 1;
                Break;
              end
              else
              if FSvnClient.IsPathVersioned(TestPaths[J]) then
              begin
                VersionedPaths.Add(TestPaths[J]);
                StartUnversionedIdx := J + 1;
                Break;
              end;
            if StartUnversionedIdx <> -1 then
            begin
              for J := StartUnversionedIdx to TestPaths.Count - 1 do
                if AutoAdd then
              begin
                FSvnClient.Add(TestPaths[J]);
                AddedDirectories.Add(TestPaths[J]);
                end
                else
                if AUnversionedFilesAndDirectories.IndexOf(TestPaths[J]) = -1 then
                  AUnversionedFilesAndDirectories.AddObject(TestPaths[J], TObject(1));
              if TestPaths[Pred(TestPaths.Count)] <> AFilesAndDirectoriesInRepo[I] then
                if AutoAdd then
                  FSvnClient.Add(AFilesAndDirectoriesInRepo[I])
                else
                begin
                  AUnversionedFilesAndDirectories.Add(AFilesAndDirectoriesInRepo[I]);
                  AFilesAndDirectoriesInRepo.Delete(I);
              end;
            end
            else
              //remove files outside of the working copy (they cannot be committed)
              AFilesAndDirectoriesInRepo.Delete(I);
          finally
            TestPaths.Free;
          end;
        end;
    finally
      VersionedPaths.Free;
    end;
  end;

var
  I: Integer;
  TempBasePath: string;
  FilesAndDirectoriesInRepo, AddedDirectories, UnversionedFilesAndDirectories: TStringList;
begin
  FilesAndDirectoriesInRepo := TStringList.Create;
  AddedDirectories := TStringList.Create;
  UnversionedFilesAndDirectories := TStringList.Create;
  try
    UnversionedFilesAndDirectories.Sorted := True;
    FilesAndDirectoriesInRepo.Assign(FDirectoryList);
    if FRootType = rtExpicitFiles then
    begin
      CheckVersionProjectFiles(FilesAndDirectoriesInRepo, AddedDirectories,
        UnversionedFilesAndDirectories);
      for I := 0 to UnversionedFilesAndDirectories.Count - 1 do
        FrameAdd(TSvnListViewItem.Create(UnversionedFilesAndDirectories[I],
          svnWcStatusUnversioned, UnversionedFilesAndDirectories.Objects[I] <> nil, False));
    end;
    for I := 0 to FilesAndDirectoriesInRepo.Count - 1 do
      FSvnClient.GetModifications(FilesAndDirectoriesInRepo[I], AModificationCallBack, True,
        False, False, True);
    if (FRootType = rtExpicitFiles) and (AddedDirectories.Count > 0) then
    begin
      for I := 0 to AddedDirectories.Count - 1 do
        FrameAdd(TSvnListViewItem.Create(AddedDirectories[I], svnWcStatusAdded, True, False));
      FilesAndDirectoriesInRepo.AddStrings(AddedDirectories);
    end;
    AURL := FSvnClient.GetBaseURL(FilesAndDirectoriesInRepo, TempBasePath);
  finally
    FilesAndDirectoriesInRepo.Free;
    AddedDirectories.Free;
    UnversionedFilesAndDirectories.Free;
  end;
}
var
  I: Integer;
  TempBasePath: string;
  FilesAndDirectoriesInRepo: TStringList;
begin
  //TODO: add support for FRootType = rtExpicitFiles
  if FDirectoryList.Count >= 1 then
    AURL := ExtractFilePath(FDirectoryList[0]);
  FilesAndDirectoriesInRepo := TStringList.Create;
  try
    FilesAndDirectoriesInRepo.Assign(FDirectoryList);
    for I := 0 to FilesAndDirectoriesInRepo.Count - 1 do
      FSvnClient.GetModifications(FilesAndDirectoriesInRepo[I], AModificationCallBack);
  finally
    FilesAndDirectoriesInRepo.Free;
  end;
end;

procedure TCommit.FrameCreated(AFrame: TCustomFrame);
var
  Cursor: TCursor;
  RecentComments: TStringList;
  URL: string;
begin
  FSvnCommitFrame := THgCommitFrame(AFrame);
  if IDEClient.Options.AlternativeCommitLayout then
    FSvnCommitFrame.EnableAlternativeLayout;
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    FSvnCommitFrame.BeginUpdate;
    try
      PrepareFileList(FSvnCommitFrame.Add, ModificationCallBack, URL);
      FSvnCommitFrame.AllowEmptyComment := False;
      FSvnCommitFrame.SupportsExternals := False;
      FSvnCommitFrame.CheckForNoFilesVisible;
      FSvnCommitFrame.URL := URL;
      FSvnCommitFrame.CommitCallBack := CommitCallBack;
      FSvnCommitFrame.CloseCallBack := CloseCallBack;
      FSvnCommitFrame.DiffCallBack := DiffCallBack;
      FSvnCommitFrame.RevertCallBack := RevertCallBack;
      FSvnCommitFrame.AddCallBack := AddCallBack;
      FSvnCommitFrame.ResolveCallBack := ResolveCallBack;
      FSvnCommitFrame.GetFileStatusCallBack := GetFileStatusCallBack;
      FSvnCommitFrame.FileColorCallBack := FileColorCallBack;
      FSvnCommitFrame.RefreshCallBack := RefreshCallBack;
      if FFoundMissing then
        FSvnCommitFrame.HandleMissingFiles;
      RecentComments := TStringList.Create;
      try
        LoadRecentComments(RecentComments);
        FSvnCommitFrame.RecentComments := RecentComments;
      finally
        RecentComments.Free;
      end;
      FSvnCommitFrame.KeepOpenAfterCommit := IDEClient.Options.KeepCommitViewOpenAfterCommit;
    finally
      FSvnCommitFrame.EndUpdate;
    end;
  finally
    Screen.Cursor := Cursor;
  end;
end;

function TCommit.GetCanCloneView: Boolean;
begin
  Result := False;
end;

function TCommit.GetCaption: string;
begin
  Result := sCommit;
end;

function TCommit.GetEditorWindowCaption: string;
begin
  Result := sCommit;
end;

function TCommit.GetEditState: TEditState;
begin
  Result := [];
  if Assigned(FSvnCommitFrame) then
    Result := SvnEditStateToEditState(FSvnCommitFrame.SvnEditState);
end;

procedure TCommit.GetFileStatusCallBack(const FileName: string;
  var SvnListViewItem: TSvnListViewItem);
begin
  FStatusItem := @SvnListViewItem;
  FSvnClient.GetModifications(FileName, StatusCallBack);
end;

function TCommit.GetFrameClass: TCustomFrameClass;
begin
  Result := THgCommitFrame;
end;

function TCommit.GetImageIndex: Integer;
begin
  Result := 0;
end;

function TCommit.GetTabHintText: string;
begin
  Result := '';
  //TODO: provide a custom hint for the Commit tab
end;

function TCommit.GetViewIdentifier: string;
begin
  Result := sSvnCommitView;
end;

procedure TCommit.ModificationCallBack(Sender: TObject; Item: THgItem;
  var Cancel: Boolean);
begin
  if (FRootType <> rtExpicitFiles) or (not FSvnCommitFrame.Found(Item.FileName)) then
  begin
    if Item.Status = gsMissing then
      FFoundMissing := True;
    FSvnCommitFrame.Add(TSvnListViewItem.Create(Item.FileName, Item.Status, False, False));
  end;
end;

procedure TCommit.ModificationRefreshCallBack(Sender: TObject; Item: THgItem;
  var Cancel: Boolean);
begin
  if (FRootType <> rtExpicitFiles) or (not FSvnCommitFrame.Found(Item.FileName)) then
  begin
    if Item.Status = gsMissing then
      FFoundMissing := True;
    FSvnCommitFrame.RefreshAdd(TSvnListViewItem.Create(Item.FileName, Item.Status, False, False));
  end;
end;

procedure TCommit.RefreshCallBack;
var
  Dummy: string;
begin
  PrepareFileList(FSvnCommitFrame.RefreshAdd, ModificationRefreshCallBack, Dummy);
  if FFoundMissing then
    FSvnCommitFrame.HandleMissingFiles(True);
end;

procedure TCommit.ResolveCallBack(const FileName: string);
begin
  //no idea if Mercurial has conflicts
end;

function TCommit.RevertCallBack(const FileName: string; ARecursive: Boolean; var ANewTextStatus: THgStatus): Boolean;
var
  Module: IOTAModule;
  SvnItem: THgItem;
begin
  Result := FSvnClient.Revert(FileName);
  if Result then
    SvnMessageView.WriteMessage(FileName, Format(sRevertedFile, [FileName]));
  SvnItem := THgItem.Create(FSvnClient, FileName);
  try
    SvnItem.LoadStatus;
    ANewTextStatus := SvnItem.Status;
  finally
    SvnItem.Free;
  end;
  Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
  if Module <> nil then
    Module.Refresh(True);
  FlushFileState(FileName);
end;

procedure TCommit.SelectView;
begin
  // Not used
end;

procedure TCommit.StatusCallBack(Sender: TObject; Item: THgItem;
  var Cancel: Boolean);
begin
  FStatusItem^.NewValues(Item.FileName, Item.Status, False, False);
end;

function DoDeleteCommitListLocalHistory(CommitList: TStringList): Integer;
var
  I, J: Integer;
  Files: TStringDynArray;
  Path, SearchPattern: string;
begin
  Result := 0;
  for I := 0 to CommitList.Count - 1 do
  begin
    Path := ExtractFilePath(CommitList[I]) + '__history'; // do not localize
    if DirectoryExists(Path) then
    begin
      SearchPattern := ExtractFileName(CommitList[I]) + '.~*';
      Files := TDirectory.GetFiles(Path, SearchPattern);
      for J := Low(Files) to High(Files) do
        if Copy(Files[J], Length(Files[J]), 1) = '~' then
        begin
          DeleteFile(Files[J]);
          Inc(Result);
        end;
    end;
  end;
end;

procedure DoCommit(const SvnClient: THgClient; const CommitList: TStringList;
  const Comment: string; const RecentComments: TStringList; ADeleteLocalHistory: Boolean);
var
  Error: THgError;
begin
  SvnMessageView.CheckMessageGroup(True);
  SaveRecentComments(RecentComments);
  Error := SvnClient.Commit(CommitList, TrimRight(Comment));
  if Error = hgeSuccess then
  begin
    SvnMessageView.WriteTitle(Format(sCommitCompleted, [SvnClient.LastCommitInfoChangeSetID]));
    if ADeleteLocalHistory then
      DoDeleteCommitListLocalHistory(CommitList);
  end
  else
  if Error = hgeEmptyCommitMessage then
    MessageDlg('Empty commit message', mtError, [mbOK], 0)//str
  else
  if Error = hgeNoUsernameSupplied then
    MessageDlg('Username not configured', mtError, [mbOK], 0)
  else
    MessageDlg('unknown error', mtError, [mbOK], 0);
  FlushFileListFileStates(CommitList);
end;

procedure LoadRecentComments(const RecentComments: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  S: string;
  I: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sRecentComments;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    for I := 0 to MaxRecentComments - 1 do
    begin
      Key := Format(sComment, [I]);
      S := Reg.ReadString(Key);
      if S = '' then
        Break
      else
        RecentComments.Add(S);
    end;
  finally
    Reg.Free;
  end;
end;

procedure SaveRecentComments(const RecentComments: TStringList);
var
  Reg: TRegistry;
  BaseKey: string;
  Key: string;
  I: Integer;
  WriteCount: Integer;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + sRecentComments;
    Reg.OpenKey(BaseKey, True);
    WriteCount := MaxRecentComments;
    if WriteCount > RecentComments.Count then
      WriteCount := RecentComments.Count;
    for I := 0 to WriteCount - 1 do
    begin
      Key := Format(sComment, [I]);
      Reg.WriteString(Key, RecentComments[I]);
    end;
  finally
    Reg.Free;
  end;
end;

function GetView: INTACustomEditorView;
begin
  Result := CommitView;
end;

procedure Register;
begin
  (BorlandIDEServices as IOTAEditorViewServices).RegisterEditorView(sSvnCommitView, GetView);
end;

initialization
finalization
  (BorlandIDEServices as IOTAEditorViewServices).UnRegisterEditorView(sSvnCommitView);
end.
