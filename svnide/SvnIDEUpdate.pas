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
unit SvnIDEUpdate;

interface

uses Classes, ToolsApi, svn_client, SvnClient, SvnIDEClient, SvnIDEMenus;

type
  TBaseUpdateSvnMenu = class(TSvnMenu)
  protected
    FSvnIDEClient: TSvnIDEClient;
    FRootType: TRootType;
    { Misc }
    procedure Execute(const MenuContextList: IInterfaceList); override;
    function ResolveCallBack(const FileName: string): Boolean;
    procedure FileRefresh(const FileList: TStringList; ForceReload: Boolean);
    procedure FileRefreshCallBack(const FileList: TStringList);
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentUpdateSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirUpdateSvnMenu = class(TBaseUpdateSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TProjectDirUpdateSvnMenu = class(TBaseUpdateSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TExpicitFilesUpdateSvnMenu = class(TBaseUpdateSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TFileUpdateSvnMenu = class(TBaseUpdateSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirUpdateSvnMenu = class(TBaseUpdateSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  function Resolve(SvnClient: TSvnClient; const FileName: string): Boolean;

  procedure Merge(const MergeViewer: IOTACustomMergeViewer; const BaseFileName,
    TheirFileName, MyFileName, FileName: string);


implementation

uses Forms, Controls, Windows, SvnIDEConst, SysUtils, SvnIDEMessageView,
  SvnClientConflict, ActiveX, IStreams, SvnClientUpdate, svnconst,
  Generics.Defaults, Generics.Collections, SvnUIUtils, SvnIDEUtils, Graphics,
  SvnIDEIcons;

const
  sPMVUpdate = 'Update';
  sPMVUpdateParent = 'UpdateParent';
  sPMVUpdateRootDir = 'UpdateRootDir';
  sPMVUpdateProjectDir = 'UpdateProjectDir';
  sPMVUpdateExpicitFiles = 'UpdateExpicitFiles';
  sPMVUpdateDir = 'UpdateDir';

type
  TUpdateThread = class(TThread)
  protected
    FDirectoryList: TStringList;
    FProjectFound: Boolean;
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
    procedure Execute; override;
    procedure UpdateCallBack(Sender: TObject; const Path, MimeType: string; Action: TSvnWcNotifyAction;
      Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean);
  public
    constructor Create(const MenuContextList: IInterfaceList;
      SvnIDEClient: TSvnIDEClient; RootDir: TRootType;
      ResolveCallBack: TResolveCallBack; FileRefreshCallBack: TFileRefreshCallBack); reintroduce;
    destructor Destroy; override;
  end;

var
  Aborted: Boolean;

procedure Merge(const MergeViewer: IOTACustomMergeViewer; const BaseFileName,
  TheirFileName, MyFileName, FileName: string);
var
  Base, Theirs, Mine: TIFileStream;
begin
  Base := TIFileStream.Create(BaseFileName, fmShareDenyNone);
  FileClose(Base.FileStream.Handle);
  Theirs := TIFileStream.Create(TheirFileName, fmShareDenyNone);
  FileClose(Theirs.FileStream.Handle);
  Mine := TIFileStream.Create(MyFileName, fmShareDenyNone);
  FileClose(Mine.FileStream.Handle);
  MergeViewer.ShowMerge(Base, Theirs, Mine, BaseFilename, TheirFileName,
    MyFileName, FileName, BaseFilename, TheirFileName, MyFileName, True);
end;

function Resolve(SvnClient: TSvnClient; const FileName: string): Boolean;
var
  Status: TStatusEntry;
  CanPostpone: Boolean;
  MergeViewer: IOTACustomMergeViewer;
  MergeViewerName: string;
  ConflictDialog: TSvnConflictDialog;
  Path: string;
  Files: TStringList;

  procedure Resolved(const FileName: string; Choice: TSvnWcConflictChoice);
  begin
    try
      SvnClient.Resolved(FileName, Choice);
    except
      if not HandleSvnException(ExceptObject) then
        raise;
    end;
  end;

begin
  Result := False;
  SvnClient.GetFirstStatus(FileName, Status);
  if (Status.Valid) then
  begin
    CanPostpone := not ((BorlandIDEServices as IOTAServices).IsProject(FileName)
      or (BorlandIDEServices as IOTAServices).IsProjectGroup(FileName));
    MergeViewer := (BorlandIDEServices as IOTACustomMergeManager).DefaultMergeViewer;
    if MergeViewer <> nil then
      MergeViewerName := MergeViewer.DisplayName
    else
      MergeViewerName := '';
    ConflictDialog := TSvnConflictDialog.Create(Application, FileName,
      MergeViewerName, CanPostpone);
    if ConflictDialog.ShowModal = mrOk then
    begin
      Result := True;
      if ConflictDialog.Local.Checked then
      begin
        Resolved(FileName, SvnWcConflictChooseMineFull);
        if CopyFile(PChar(FileName + '.mine'), PChar(FileName), False) then
          DeleteFile(FileName + '.mine');
      end
      else if ConflictDialog.Server.Checked then
      begin
        Files := TStringList.Create;
        try
          Files.Add(FileName);
          try
            SvnClient.Revert(Files);
          except
            if not HandleSvnException(ExceptObject) then
              raise;
          end;
        finally
          Files.Free;
        end;
        Resolved(FileName, SvnWcConflictChooseMerged);
      end
      else if ConflictDialog.MergeView.Checked then
      begin
        Path := IncludeTrailingPathDelimiter(ExtractFilePath(FileName));
        Merge(MergeViewer, Path + Status.Conflict_old, Path + Status.Conflict_new,
          Path + Status.Conflict_wrk, FileName);
        Resolved(FileName, SvnWcConflictChooseMerged);
      end;
    end;
  end;
end;

{ TBaseUpdateSvnMenu }

constructor TBaseUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdate;
  FHelpContext := 0;
  FSvnIDEClient := ASvnIDEClient;
  FRootType := rtRootDir;
end;

procedure TBaseUpdateSvnMenu.Execute(const MenuContextList: IInterfaceList);
begin
  SaveAll;
  Aborted := False;
  TUpdateThread.Create(MenuContextList, FSvnIdeClient, FRootType,
    ResolveCallBack, FileRefreshCallBack);
end;

procedure TBaseUpdateSvnMenu.FileRefresh(const FileList: TStringList; ForceReload: Boolean);
var
  ModuleList: TDictionary<string, IOTAModule>;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I: Integer;
  IsProjectOrGroup: Boolean;
begin
  ModuleList := TDictionary<string, IOTAModule>.Create(TOrdinalStringComparer.Create);
  try
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    for I := 0 to ModuleServices.ModuleCount - 1 do
      ModuleList.Add(ModuleServices.Modules[I].FileName, ModuleServices.Modules[I]);
    for I := 0 to FileList.Count - 1 do
      if ModuleList.TryGetValue(FileList[I], Module) then
      begin
        IsProjectOrGroup := (BorlandIDEServices as IOTAServices).IsProject(FileList[I])
          or (BorlandIDEServices as IOTAServices).IsProjectGroup(FileList[I]);
        // Don't load conflicted projects
        if not IsProjectOrGroup or (Integer(FileList.Objects[I]) = 1) then
          Module.Refresh(ForceReload);
      end;
  finally
    ModuleList.Free;
  end;
end;

procedure TBaseUpdateSvnMenu.FileRefreshCallBack(const FileList: TStringList);
begin
  FileRefresh(FileList, False);
end;

function TBaseUpdateSvnMenu.ResolveCallBack(const FileName: string): Boolean;
var
  FileList: TStringList;
begin
  Result := Resolve(FSvnIDEClient.SvnClient, FileName);
  if Result then
  begin
    FileList := TStringList.Create;
    try
      FileList.AddObject(FileName, TObject(1));
      FileRefresh(FileList, True);
    finally
      FileList.Free;
    end;
  end;
end;

{ TParentUpdateSvnMenu }

constructor TParentUpdateSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdateParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentUpdateSvnMenu;
  FHelpContext := 0;
end;

function TParentUpdateSvnMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;

{ TRootDirUpdateSvnMenu }

constructor TRootDirUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVUpdateParent;
  FCaption := sPMMRootDir;
  FVerb := sPMVUpdateRootDir;
  FPosition := pmmpRootDirUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirUpdateSvnMenu }

constructor TProjectDirUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FParent := sPMVUpdateParent;
  FCaption := sPMMProjectDir;
  FVerb := sPMVUpdateProjectDir;
  FPosition := pmmpProjectDirUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TExpicitFilesUpdateSvnMenu }

constructor TExpicitFilesUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtExpicitFiles;
  FParent := sPMVUpdateParent;
  FCaption := sPMMExpicitFiles;
  FVerb := sPMVUpdateExpicitFiles;
  FPosition := pmmpExpicitFilesUpdateSvnMenu;
  FHelpContext := 0;
end;

{ TFileUpdateSvnMenu }

constructor TFileUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdate;
  FPosition := pmmpFileUpdateSvnMenu;
  FHelpContext := 0;
end;

function TFileUpdateSvnMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;

{ TDirUpdateSvnMenu }

constructor TDirUpdateSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMUpdate;
  FVerb := sPMVUpdateDir;
  FPosition := pmmpFileUpdateSvnMenu;
  FHelpContext := 0;
end;

function TDirUpdateSvnMenu.GetImageIndex: Integer;
begin
  Result := UpdateImageIndex;
end;

{ TUpdateThread }

procedure TUpdateThread.AbortCallBack;
begin
  Aborted := True;
end;

procedure TUpdateThread.Add(const Path, Action: string; Conflicted: Boolean; TextColor: TColor);
begin
  FSyncPath := StringReplace(Path, '/', '\', [rfReplaceAll]);
  FSyncAction := Action;
  FSyncConflicted := Conflicted;
  FSyncTextColor := TextColor;
  Synchronize(nil, SyncAdd);
end;

procedure TUpdateThread.CancelCallback(Sender: TObject; var Cancel: Boolean);
begin
  Cancel := Aborted;
end;

function TUpdateThread.ConflictCallback(Sender: TObject;
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

constructor TUpdateThread.Create(const MenuContextList: IInterfaceList;
  SvnIDEClient: TSvnIDEClient; RootDir: TRootType; ResolveCallBack: TResolveCallBack;
  FileRefreshCallBack: TFileRefreshCallBack);
var
  TempBasePath: string;
begin
  inherited Create(True);
  FSvnIDEClient := SvnIDEClient;
  FDirectoryList := TStringList.Create;
  BuildFileList(MenuContextList, FDirectoryList, FSvnIDEClient.SvnClient, RootDir, FProjectFound);
  FSvnIDEClient.SvnClient.GetBaseURL(FDirectoryList, TempBasePath);
  FUpdateDialog := GetUpdateDialog(FSvnIDEClient.SvnClient.GetBaseURL(FDirectoryList, TempBasePath),
    AbortCallBack, ResolveCallBack, FileRefreshCallBack);
  FUpdateDialog.Show;
  FreeOnTerminate := True;
  Resume;
end;

destructor TUpdateThread.Destroy;
begin
  FDirectoryList.Free;
  inherited;
end;

procedure TUpdateThread.Execute;
begin
  NameThreadForDebugging('DelphiSVN Update');
  try
    FExceptionMessage := '';
    FSvnIDEClient.SvnClient.Update(FDirectoryList, UpdateCallBack, FProjectFound, False, ConflictCallback, CancelCallback);
  except
    if not GetSvnExceptionMessage(ExceptObject, FExceptionMessage) then
      raise;
  end;
  Synchronize(nil, SyncCompleted);
end;

procedure TUpdateThread.SyncAdd;
begin
  FUpdateDialog.Add(FSyncPath, FSyncAction, FSyncConflicted, FSyncTextColor);
end;

procedure TUpdateThread.SyncCompleted;
begin
  if FExceptionMessage <> '' then
    ShowSvnExceptionMessage(FExceptionMessage);
  FUpdateDialog.Completed;
end;

procedure TUpdateThread.UpdateCallBack(Sender: TObject; const Path,
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

end.
