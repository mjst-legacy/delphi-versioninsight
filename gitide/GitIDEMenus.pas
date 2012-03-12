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
unit GitIDEMenus;

interface

uses
  {$IFDEF TOOLSPROAPI}
  ToolsProAPI,
  {$ENDIF TOOLSPROAPI}
  Classes, ToolsAPI, GitIDEClient, GitClient;

const
  sPMVGitParent = 'GitParent';

//  Menu Positions
const
  // Project Menu Items
  // Commit
  pmmpParentCommitSvnMenu = pmmpUserVersionControl + 10;
  pmmpRootDirCommitSvnMenu = pmmpUserVersionControl + 20;
  pmmpProjectDirCommitSvnMenu = pmmpUserVersionControl + 30;
  pmmpExpicitFilesCommitSvnMenu = pmmpUserVersionControl + 40;
  // Update
  pmmpParentUpdateSvnMenu = pmmpUserVersionControl + 50;
  pmmpRootDirUpdateSvnMenu = pmmpUserVersionControl + 60;
  pmmpProjectDirUpdateSvnMenu = pmmpUserVersionControl + 70;
  pmmpExpicitFilesUpdateSvnMenu = pmmpUserVersionControl + 80;
  // Log
  pmmpParentLogSvnMenu = pmmpUserVersionControl + 90;
  pmmpRootDirLogSvnMenu = pmmpUserVersionControl + 100;
  pmmpProjectDirLogSvnMenu = pmmpUserVersionControl + 110;
  // Clean
  pmmpParentCleanSvnMenu = pmmpUserVersionControl + 120;
  pmmpRootDirCleanSvnMenu = pmmpUserVersionControl + 130;
  pmmpProjectDirCleanSvnMenu = pmmpUserVersionControl + 140;
  // Repository Browser
  pmmpParentRepoSvnMenu = pmmpUserVersionControl + 150;
  pmmpRootDirRepoSvnMenu = pmmpUserVersionControl + 160;
  pmmpProjectDirRepoSvnMenu = pmmpUserVersionControl + 170;
  // File Menu Items
  pmmpFileCommitSvnMenu = pmmpUserVersionControl + 1010;
  pmmpFileUpdateSvnMenu = pmmpUserVersionControl + 1020;
  pmmpFileRepoSvnMenu = pmmpUserVersionControl + 1030;
  pmmpFileRevertSvnMenu = pmmpUserVersionControl + 1040;


type
  TGitMenu = class(TInterfacedObject, IOTALocalMenu, IOTAProjectManagerMenu
    {$IFDEF TOOLSPROAPI}, IOTAProProjectManagerMenu155{$ENDIF})
  protected
    FCaption: string;
    FChecked: Boolean;
    FEnabled: Boolean;
    FHelpContext: Integer;
    FIsMultiSelectable: Boolean;
    FName: string;
    FParent: string;
    FPosition: Integer;
    FGitIDEClient: TGitIDEClient;
    FVerb: string;

    {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    {IOTALocalMenu}
    function GetCaption: string;
    function GetChecked: Boolean;
    function GetEnabled: Boolean;
    function GetHelpContext: Integer;
    function GetName: string;
    function GetParent: string;
    function GetPosition: Integer;
    function GetVerb: string;
    procedure SetCaption(const Value: string);
    procedure SetChecked(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpContext(Value: Integer);
    procedure SetName(const Value: string);
    procedure SetParent(const Value: string);
    procedure SetPosition(Value: Integer);
    procedure SetVerb(const Value: string);

    { IOTAProjectManagerMenu }
    function GetIsMultiSelectable: Boolean;
    procedure SetIsMultiSelectable(Value: Boolean);
    procedure Execute(const MenuContextList: IInterfaceList); virtual;
    function PreExecute(const MenuContextList: IInterfaceList): Boolean;
    function PostExecute(const MenuContextList: IInterfaceList): Boolean;

    { IOTAProProjectManagerMenu155 }
    function GetImageIndex: Integer; virtual;
  public
    constructor Create(AGitIDEClient: TGitIDEClient);
  end;

  TRootType = (rtRootDir, rtProjectDir, rtExpicitFiles, rtDir);

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const GitClient: TGitClient;
  RootType: TRootType; var ProjectFound: Boolean);
procedure RegisterMenus(AGitIDEClient: TGitIDEClient);
procedure UnRegisterMenus;
function RootDirectory(const GitClient: TGitClient; const Path: string): string;


implementation

uses
  {$IFDEF TOOLSPROAPI}
  GitIDEFileStates,
  {$ENDIF TOOLSPROAPI}
  SysUtils, Dialogs, GitIDEConst, GitIDECommit, GitIDELog, GitIDECheckout, GitIDEIcons,
  GitIDERevert;

const
  sGitName = 'versioninsight.git';

type
  TGitNotifier = class(TInterfacedObject, IOTAVersionControlNotifier,
    IOTAVersionControlNotifier150 {$IFDEF TOOLSPROAPI}, IOTAProVersionControlNotifier155, IOTAProVersionControlSearchFileFind{$ENDIF})
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAVersionControlNotifier }
    function GetDisplayName: string;
    function IsFileManaged(const Project: IOTAProject; const IdentList: TStrings): Boolean;
    procedure ProjectManagerMenu(const Project: IOTAProject; const IdentList: TStrings;
      const ProjectManagerMenuList: IInterfaceList; IsMultiSelect: Boolean);
    function AddNewProject(const Project: IOTAProject): Boolean;
    {  IOTAVersionControlNotifier150 }
    function CheckoutProject(var ProjectName: string): Boolean;
    function CheckoutProjectWithConnection(var ProjectName: string;
      const Connection: string): Boolean;
    function GetName: string;
    { IOTAProVersionControlNotifier155 }
    procedure FileBrowserMenu(const IdentList: TStrings;
      const FileBrowserMenuList: IInterfaceList; IsMultiSelect: Boolean);
    function GetImageIndex: Integer;
    function GetCheckoutMenuCaption: string;
    function GetAddNewProjectCaption: string;
    function GetAddNewProjectEnabled: Boolean;
    { IOTAProVersionControlSearchFileFind }
    {$IFDEF TOOLSPROAPI}
    function GetModifiedFiles(const AModifiedFiles: TStrings; AProgress: IOTAProSearchFileFindProgress): Boolean;
    {$ENDIF TOOLSPROAPI}
    { Misc }
    procedure InitNonFileIdentifiers;
  protected
    FGitIDEClient: TGitIDEClient;
    FNonFileIdentifiers: TStringList;
  public
    constructor Create(const GitIDEClient: TGitIDEClient);
    destructor Destroy; override;
  end;

  TParentGitMenu = class(TGitMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

var
  PMMSvnParent, PMMParentCommit, PMMRootDirCommit, PMMProjectDirCommit,
  PMMExpicitFilesCommit, PMMFileCommit, PMMParentUpdate, PMMRootDirUpdate,
  PMMProjectDirUpdate, PMMExpicitFilesUpdate, PMMFileUpdate,
  PMMParentCleanSvnMenu, PMMRootDirCleanSvnMenu, PMMProjectDirCleanSvnMenu,
  PMMParentLogGitMenu, PMMRootDirLogGitMenu, PMMProjectDirLogGitMenu,
  PMMParentRepo, PMMRootDirRepo, PMMProjectDirRepo, PMMFileRepoSvnMenu,
  PMMFileRevert: IOTAProjectManagerMenu;

  FBMMSvnParent, FBMMCommit, FBMMLog: IOTAProjectManagerMenu;

function RootDirectory(const GitClient: TGitClient; const Path: string): string;
var
  RepoPath: string;
  TempPath: string;
begin
  Result := ExtractFilePath(Path);
  RepoPath := GitClient.FindRepositoryRoot(Result);
  if RepoPath = '' then
  else
  begin
    TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    while RepoPath = GitClient.FindRepositoryRoot(TempPath) do
    begin
      Result := TempPath;
      TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    end;
  end;
end;

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const GitClient: TGitClient;
  RootType: TRootType; var ProjectFound: Boolean);
var
  I: Integer;
  MenuContext: IOTAMenuContext;
  Project: IOTAProject;
  TempProject: IOTAProject;
  Path: string;
  Module: IOTAModule;
begin
  ProjectFound := False;
  for I := 0 to MenuContextList.Count - 1 do
  begin
    if RootType = rtDir then
    begin
      if Supports(MenuContextList[I], IOTAMenuContext, MenuContext) then
        DirectoryList.Add(IncludeTrailingPathDelimiter(MenuContext.Ident));
    end
    else
    begin
      Project := (MenuContextList[I] as IOTAProjectMenuContext).Project;
      if Supports(MenuContextList[I], IOTAMenuContext, MenuContext) then
        if FileExists(MenuContext.Ident) then
        begin
          // If it is a project
          if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident), IOTAProject, TempProject) then
          begin
            ProjectFound := True;
            case RootType of
              rtRootDir:
                begin
                  Path := RootDirectory(GitClient, MenuContext.Ident);
                  if Path = '' then
                    Path := ExtractFilePath(MenuContext.Ident);
                  DirectoryList.Add(Path);
                end;
              rtProjectDir: DirectoryList.Add(ExtractFilePath(MenuContext.Ident));
              rtExpicitFiles: TempProject.GetCompleteFileList(DirectoryList);
            end;
          end
          else
          begin
            if Project <> nil then
              Project.GetAssociatedFiles(MenuContext.Ident, DirectoryList)
            else
            begin
              Module := (BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident);
              if Module <> nil then
                Module.GetAssociatedFilesFromModule(DirectoryList)
              else
                DirectoryList.Add(MenuContext.Ident);
            end;
          end;
        end;
    end;
  end;
end;

{ TGitMenu }

procedure TGitMenu.AfterSave;
begin

end;

procedure TGitMenu.BeforeSave;
begin

end;

constructor TGitMenu.Create(AGitIDEClient: TGitIDEClient);
begin
  inherited Create;
  FGitIDEClient := AGitIDEClient;
  FParent := '';
  FChecked := False;
  FEnabled := True;
  FIsMultiSelectable := True;
  FName := '';
end;

procedure TGitMenu.Destroyed;
begin

end;

procedure TGitMenu.Execute(const MenuContextList: IInterfaceList);
begin

end;

function TGitMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function TGitMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function TGitMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TGitMenu.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function TGitMenu.GetImageIndex: Integer;
begin
  Result := -1;
end;

function TGitMenu.GetIsMultiSelectable: Boolean;
begin
  Result := FIsMultiSelectable;
end;

function TGitMenu.GetName: string;
begin
  Result := FName;
end;

function TGitMenu.GetParent: string;
begin
  Result := FParent;
end;

function TGitMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TGitMenu.GetVerb: string;
begin
  Result := FVerb;
end;

procedure TGitMenu.Modified;
begin

end;

function TGitMenu.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function TGitMenu.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

procedure TGitMenu.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TGitMenu.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure TGitMenu.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TGitMenu.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure TGitMenu.SetIsMultiSelectable(Value: Boolean);
begin
  FIsMultiSelectable := Value;
end;

procedure TGitMenu.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TGitMenu.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure TGitMenu.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure TGitMenu.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

{ TGitNotifier }

function TGitNotifier.AddNewProject(const Project: IOTAProject): Boolean;
begin
  Result := False;
end;

procedure TGitNotifier.AfterSave;
begin

end;

procedure TGitNotifier.BeforeSave;
begin

end;

function TGitNotifier.CheckoutProject(var ProjectName: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName);
end;

function TGitNotifier.CheckoutProjectWithConnection(var ProjectName: string;
  const Connection: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName, Connection);
end;

constructor TGitNotifier.Create(const GitIDEClient: TGitIDEClient);
begin
  inherited Create;
  FGitIDEClient := GitIDEClient;
  FNonFileIdentifiers := TStringList.Create;
  FNonFileIdentifiers.Sorted := True;
  InitNonFileIdentifiers;
end;

destructor TGitNotifier.Destroy;
begin
  FNonFileIdentifiers.Free;
  inherited Destroy;
end;

procedure TGitNotifier.Destroyed;
begin

end;

procedure TGitNotifier.FileBrowserMenu(const IdentList: TStrings;
  const FileBrowserMenuList: IInterfaceList; IsMultiSelect: Boolean);
begin
  if (IdentList.Count = 1) and DirectoryExists(IdentList[0]) and
    IDEClient.GitClient.IsPathInWorkingCopy(IdentList[0]) and
    IDEClient.GitClient.IsVersioned(IdentList[0]) then
  begin
    FileBrowserMenuList.Add(FBMMSvnParent);
    FileBrowserMenuList.Add(FBMMCommit);
    FileBrowserMenuList.Add(FBMMLog);
  end;
end;

function TGitNotifier.GetAddNewProjectCaption: string;
begin
  Result := '';//Import is not yet supported
end;

function TGitNotifier.GetAddNewProjectEnabled: Boolean;
begin
  Result := False;
end;

function TGitNotifier.GetCheckoutMenuCaption: string;
begin
  Result := sMenuOpenFromVersionControl;
end;

function TGitNotifier.GetDisplayName: string;
begin
  Result := sGit;
end;

function TGitNotifier.GetImageIndex: Integer;
begin
  Result := GitImageIndex;
end;

{$IFDEF TOOLSPROAPI}
function TGitNotifier.GetModifiedFiles(const AModifiedFiles: TStrings;
  AProgress: IOTAProSearchFileFindProgress): Boolean;

  function IsSearchable(const AFileName: string; AFileState: TOTAProFileState): Boolean;
  var
    I: Integer;
    ModuleServices: IOTAModuleServices;
    Module: IOTAModule;
    Editor: IOTAEditor;
    SourceEditor: IOTASourceEditor;
  begin
    Result := AFileState.FileStateIndex in [fsiModified, fsiAdded];
    if (not Result) and (AFileState.FileStateIndex = fsiNormal) then
    begin
      ModuleServices := BorlandIDEServices as IOTAModuleServices;
      if Assigned(ModuleServices) then
      begin
        Module := ModuleServices.FindModule(AFileName);
        if Assigned(Module) then
        begin
          for I := 0 to Pred(Module.GetModuleFileCount) do
          begin
            Editor := Module.GetModuleFileEditor(I);
            if Assigned(Editor) and (Editor.QueryInterface(IOTASourceEditor, SourceEditor) = S_OK) then
              if SourceEditor.Modified then
              begin
                Result := True;
                Break;
              end;
          end;
        end;
      end;
    end;
  end;

var
  I: Integer;
  DeferredFiles, TempFiles: TStringList;
  Res: TOTAProFileStateResult;
  FileState: TOTAProFileState;
  Searchable: Boolean;
  WaitCycles, FoundCount: Integer;
  FileStateProvider: IOTAProVersionControlFileStateProvider;
begin
  if (AModifiedFiles.Count > 0) and Supports(GetFileStateProvider, IOTAProVersionControlFileStateProvider, FileStateProvider) then
  begin
    DeferredFiles := TStringList.Create;
    TempFiles := TStringList.Create;
    try
      for I := 0 to AModifiedFiles.Count - 1 do
      begin
        Res := FileStateProvider.GetFileState(AModifiedFiles[I], FileState);
        if (Res = fsrOK) and IsSearchable(AModifiedFiles[I], FileState) then
          TempFiles.Add(AModifiedFiles[I])
        else
        if Res = fsrDeferred then
          DeferredFiles.Add(AModifiedFiles[I]);
      end;
      WaitCycles := 0;//WaitCycles < 100 = up to about five seconds [100 * Sleep(50)]
      while (DeferredFiles.Count > 0) and (WaitCycles < 100) do
      begin
        FoundCount := 0;
        for I := DeferredFiles.Count - 1 downto 0 do
        begin
          Res := FileStateProvider.GetFileState(DeferredFiles[I], FileState);
          if (Res = fsrOK) and IsSearchable(DeferredFiles[I], FileState) then
            TempFiles.Add(DeferredFiles[I]);
          if Res <> fsrDeferred then
          begin
            DeferredFiles.Delete(I);
            Inc(FoundCount);
          end;
        end;
        if FoundCount = 0 then
        begin
          Inc(WaitCycles);
          Sleep(50);
        end
        else
          WaitCycles := 0;
      end;
      AModifiedFiles.Assign(TempFiles);
    finally
      TempFiles.Free;
      DeferredFiles.Free;
    end;
    Result := AModifiedFiles.Count > 0;
  end
  else
    Result := False;
end;
{$ENDIF TOOLSPROAPI}

function TGitNotifier.GetName: string;
begin
  Result := sGitName;
end;

procedure TGitNotifier.InitNonFileIdentifiers;
begin
  FNonFileIdentifiers.Clear;
  FNonFileIdentifiers.Add(sBaseContainer);
  FNonFileIdentifiers.Add(sFileContainer);
  FNonFileIdentifiers.Add(sProjectContainer);
  FNonFileIdentifiers.Add(sProjectGroupContainer);
  FNonFileIdentifiers.Add(sCategoryContainer);
  FNonFileIdentifiers.Add(sDirectoryContainer);
  FNonFileIdentifiers.Add(sReferencesContainer);
  FNonFileIdentifiers.Add(sContainsContainer);
  FNonFileIdentifiers.Add(sRequiresContainer);
  FNonFileIdentifiers.Add(sVirtualFoldContainer);
  FNonFileIdentifiers.Add(sBuildConfigContainer);
  FNonFileIdentifiers.Add(sOptionSetContainer);
end;

function TGitNotifier.IsFileManaged(const Project: IOTAProject;
  const IdentList: TStrings): Boolean;

  function SaveIsPathVersioned(const APathName: string): Boolean;
  begin
    if FileExists(APathName) then
    begin
      try
        if IDEClient.GitClient.IsPathInWorkingCopy(ExtractFilePath(APathName)) then
          Result := IDEClient.GitClient.IsVersioned(APathName)
        else
          Result := False;
      except
        Result := False;
        Exit;
      end;
    end
    else
      Result := False;
  end;

var
  I, J: Integer;
  Services: IOTAServices;
  AdditionalFiles: TStringList;
begin
  Result := False;
  for I := 0 to IdentList.Count - 1 do
    if (FNonFileIdentifiers.IndexOf(IdentList[I]) = -1) and SaveIsPathVersioned(IdentList[I]) then
    begin
      Result := True;
      Break;
    end;
  //if it is a project and the *PROJ file is not versioned then check if there are other project
  // files and if they are versioned (means for example DPROJ is not versioned, but DPR or DPK is)
  if (not Result) and (IdentList.IndexOf(sProjectContainer) <> -1) and Assigned(Project) and
    BorlandIDEServices.GetService(IOTAServices, Services) then
  begin
    for I := 0 to IdentList.Count - 1 do
      if (FNonFileIdentifiers.IndexOf(IdentList[I]) = -1) and Services.IsProject(IdentList[I]) then
      begin
        AdditionalFiles := TStringList.Create;
        try
          Project.GetAssociatedFiles(IdentList[I], AdditionalFiles);
          for J := 0 to AdditionalFiles.Count - 1 do
            if (not SameFileName(AdditionalFiles[J], IdentList[I])) and
              Services.IsProject(AdditionalFiles[J]) and SaveIsPathVersioned(AdditionalFiles[J]) then
            begin
              Result := True;
              Break;
            end;
        finally
          AdditionalFiles.Free;
        end;
        if Result then
          Break;
      end;
  end;
end;

procedure TGitNotifier.Modified;
begin

end;

procedure TGitNotifier.ProjectManagerMenu(const Project: IOTAProject;
  const IdentList: TStrings; const ProjectManagerMenuList: IInterfaceList;
  IsMultiSelect: Boolean);

  function ContainersProject: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to IdentList.Count - 1 do
      if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(IdentList[I]), IOTAProject) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  ProjectManagerMenuList.Add(PMMSvnParent);
  if ContainersProject then
  begin
    ProjectManagerMenuList.Add(PMMParentCommit);
    ProjectManagerMenuList.Add(PMMRootDirCommit);
    ProjectManagerMenuList.Add(PMMProjectDirCommit);
    ProjectManagerMenuList.Add(PMMExpicitFilesCommit);
    ProjectManagerMenuList.Add(PMMParentUpdate);
    ProjectManagerMenuList.Add(PMMRootDirUpdate);
    ProjectManagerMenuList.Add(PMMProjectDirUpdate);
    ProjectManagerMenuList.Add(PMMExpicitFilesUpdate);
    ProjectManagerMenuList.Add(PMMParentLogGitMenu);
    ProjectManagerMenuList.Add(PMMRootDirLogGitMenu);
    ProjectManagerMenuList.Add(PMMProjectDirLogGitMenu);
    ProjectManagerMenuList.Add(PMMParentCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMRootDirCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMProjectDirCleanSvnMenu);
    ProjectManagerMenuList.Add(PMMParentRepo);
    ProjectManagerMenuList.Add(PMMRootDirRepo);
    ProjectManagerMenuList.Add(PMMProjectDirRepo);
  end
  else
  begin
    ProjectManagerMenuList.Add(PMMFileCommit);
    ProjectManagerMenuList.Add(PMMFileUpdate);
    ProjectManagerMenuList.Add(PMMFileRepoSvnMenu);
    ProjectManagerMenuList.Add(PMMFileRevert);
  end;
end;

var
  NotifierIndex: Integer;

procedure RegisterMenus(AGitIDEClient: TGitIDEClient);
begin
  NotifierIndex := (BorlandIDEServices as IOTAVersionControlServices).AddNotifier(TGitNotifier.Create(AGitIDEClient));
  PMMSvnParent := TParentGitMenu.Create;
  PMMParentCommit := TParentCommitGitMenu.Create;
  PMMRootDirCommit := TRootDirCommitGitMenu.Create(AGitIDEClient);
  PMMProjectDirCommit := TProjectDirCommitGitMenu.Create(AGitIDEClient);
  //PMMExpicitFilesCommit := TExpicitFilesCommitGitMenu.Create(AGitIDEClient);
  PMMFileCommit := TFileCommitGitMenu.Create(AGitIDEClient);
  {//TODO:1
  PMMParentUpdate := TParentUpdateSvnMenu.Create;
  PMMRootDirUpdate := TRootDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirUpdate := TProjectDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMExpicitFilesUpdate := TExpicitFilesUpdateSvnMenu.Create(ASvnIDEClient);
  PMMFileUpdate := TFileUpdateSvnMenu.Create(ASvnIDEClient);
  PMMParentCleanSvnMenu := TParentCleanSvnMenu.Create;
  PMMRootDirCleanSvnMenu := TRootDirCleanSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirCleanSvnMenu := TProjectDirCleanSvnMenu.Create(ASvnIDEClient);
  }
  PMMParentLogGitMenu := TParentLogGitMenu.Create;
  PMMRootDirLogGitMenu := TRootDirLogGitMenu.Create(AGitIDEClient);
  PMMProjectDirLogGitMenu := TProjectDirLogGitMenu.Create(AGitIDEClient);
  {
  PMMParentRepo := TParentRepoSvnMenu.Create;
  PMMRootDirRepo := TRootDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirRepo := TProjectDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMFileRepoSvnMenu := TFileRepoSvnMenu.Create(ASvnIDEClient);
  }
  PMMFileRevert := TFileRevertGitMenu.Create(AGitIDEClient);

  FBMMSvnParent := TParentGitMenu.Create;
  FBMMCommit := TDirCommitGitMenu.Create(AGitIDEClient);
  FBMMLog := TDirLogGitMenu.Create(AGitIDEClient);
end;

procedure UnRegisterMenus;
begin
  (BorlandIDEServices as IOTAVersionControlServices).RemoveNotifier(NotifierIndex);
  PMMSvnParent := nil;
  PMMParentCommit := nil;
  PMMRootDirCommit := nil;
  PMMProjectDirCommit := nil;
  PMMExpicitFilesCommit := nil;
  PMMFileCommit := nil;
  PMMParentUpdate := nil;
  PMMRootDirUpdate := nil;
  PMMProjectDirUpdate := nil;
  PMMExpicitFilesUpdate := nil;
  PMMFileUpdate := nil;
  PMMParentCleanSvnMenu := nil;
  PMMRootDirCleanSvnMenu := nil;
  PMMProjectDirCleanSvnMenu := nil;
  PMMParentLogGitMenu := nil;
  PMMRootDirLogGitMenu := nil;
  PMMProjectDirLogGitMenu := nil;
  PMMParentRepo := nil;
  PMMRootDirRepo := nil;
  PMMProjectDirRepo := nil;
  PMMFileRepoSvnMenu := nil;
  PMMFileRevert := nil;

  FBMMSvnParent := nil;
  FBMMCommit := nil;
  FBMMLog := nil;
end;

{ TParentGitMenu }

constructor TParentGitMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMGitParent;
  FVerb := sPMVGitParent;
  FPosition := pmmpUserVersionControl;
  FHelpContext := 0;
end;

function TParentGitMenu.GetImageIndex: Integer;
begin
  Result := GitImageIndex;
end;

end.
