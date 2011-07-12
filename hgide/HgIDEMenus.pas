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
unit HgIDEMenus;

interface

uses
  {$IFDEF TOOLSPROAPI}
  ToolsProAPI,
  {$ENDIF TOOLSPROAPI}
  Classes, ToolsApi, HgIDEClient, HgClient;

const
  sPMVHgParent = 'HgParent';

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


type
  THgMenu = class(TInterfacedObject, IOTALocalMenu, IOTAProjectManagerMenu
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
    FHgIDEClient: THgIDEClient;
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
    constructor Create(AHgIDEClient: THgIDEClient);
  end;

  TRootType = (rtRootDir, rtProjectDir, rtExpicitFiles, rtDir);

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const HgClient: THgClient;
  RootType: TRootType; var ProjectFound: Boolean);
procedure RegisterMenus(AHgIDEClient: THgIDEClient);
procedure UnRegisterMenus;
function RootDirectory(const HgClient: THgClient; const Path: string): string;


implementation

uses SysUtils, HgIDEConst, HgIDECommit{, SvnIDEUpdate, SvnIDEClean}, HgIDELog{,
  SvnIDEImport}, HgIDECheckout{, SvnIDERepoBrowser}, HgIDEIcons;

const
  sMercurialName = 'versioninsight.mercurial';

type
  TExecuteProc = procedure(HgIDEClient: THgIDEClient;
    const MenuContextList: IInterfaceList);

  THgNotifier = class(TInterfacedObject, IOTAVersionControlNotifier,
    IOTAVersionControlNotifier150 {$IFDEF TOOLSPROAPI}, IOTAProVersionControlNotifier155{$ENDIF})
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
    { Misc }
    procedure InitNonFileIdentifiers;
  protected
    FHgIDEClient: THgIDEClient;
    FNonFileIdentifiers: TStringList;
  public
    constructor Create(const HgIDEClient: THgIDEClient);
    destructor Destroy; override;
  end;

  TParentHgMenu = class(THgMenu)
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
  PMMParentLogHgMenu, PMMRootDirLogHgMenu, PMMProjectDirLogHgMenu,
  PMMParentRepo, PMMRootDirRepo, PMMProjectDirRepo, PMMFileRepoSvnMenu: IOTAProjectManagerMenu;

  FBMMSvnParent, FBMMCommit, FBMMLog: IOTAProjectManagerMenu;

function RootDirectory(const HgClient: THgClient; const Path: string): string;
var
  RepoPath: string;
  TempPath: string;
begin
  Result := ExtractFilePath(Path);
  RepoPath := HgClient.FindRepositoryRoot(Result);
  if RepoPath = '' then
  else
  begin
    TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    while RepoPath = HgClient.FindRepositoryRoot(TempPath) do
    begin
      Result := TempPath;
      TempPath := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
    end;
  end;
end;

procedure BuildFileList(const MenuContextList: IInterfaceList;
  const DirectoryList: TStringList; const HgClient: THgClient;
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
                  Path := RootDirectory(HgClient, MenuContext.Ident);
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

{ THgMenu }

procedure THgMenu.AfterSave;
begin

end;

procedure THgMenu.BeforeSave;
begin

end;

constructor THgMenu.Create(AHgIDEClient: THgIDEClient);
begin
  inherited Create;
  FHgIDEClient := AHgIDEClient;
  FParent := '';
  FChecked := False;
  FEnabled := True;
  FIsMultiSelectable := True;
  FName := '';
end;

procedure THgMenu.Destroyed;
begin

end;

procedure THgMenu.Execute(const MenuContextList: IInterfaceList);
begin

end;

function THgMenu.GetCaption: string;
begin
  Result := FCaption;
end;

function THgMenu.GetChecked: Boolean;
begin
  Result := FChecked;
end;

function THgMenu.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function THgMenu.GetHelpContext: Integer;
begin
  Result := FHelpContext;
end;

function THgMenu.GetImageIndex: Integer;
begin
  Result := -1;
end;

function THgMenu.GetIsMultiSelectable: Boolean;
begin
  Result := FIsMultiSelectable;
end;

function THgMenu.GetName: string;
begin
  Result := FName;
end;

function THgMenu.GetParent: string;
begin
  Result := FParent;
end;

function THgMenu.GetPosition: Integer;
begin
  Result := FPosition;
end;

function THgMenu.GetVerb: string;
begin
  Result := FVerb;
end;

procedure THgMenu.Modified;
begin

end;

function THgMenu.PostExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

function THgMenu.PreExecute(const MenuContextList: IInterfaceList): Boolean;
begin
  Result := True;
end;

procedure THgMenu.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure THgMenu.SetChecked(Value: Boolean);
begin
  FChecked := Value;
end;

procedure THgMenu.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
end;

procedure THgMenu.SetHelpContext(Value: Integer);
begin
  FHelpContext := Value;
end;

procedure THgMenu.SetIsMultiSelectable(Value: Boolean);
begin
  FIsMultiSelectable := Value;
end;

procedure THgMenu.SetName(const Value: string);
begin
  FName := Value;
end;

procedure THgMenu.SetParent(const Value: string);
begin
  FParent := Value;
end;

procedure THgMenu.SetPosition(Value: Integer);
begin
  FPosition := Value;
end;

procedure THgMenu.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

{ THgNotifier }

function THgNotifier.AddNewProject(const Project: IOTAProject): Boolean;
begin
  Result := False;
end;

procedure THgNotifier.AfterSave;
begin

end;

procedure THgNotifier.BeforeSave;
begin

end;

function THgNotifier.CheckoutProject(var ProjectName: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName);
end;

function THgNotifier.CheckoutProjectWithConnection(var ProjectName: string;
  const Connection: string): Boolean;
begin
  Result := DoCheckOutProject(ProjectName, Connection);
end;

constructor THgNotifier.Create(const HgIDEClient: THgIDEClient);
begin
  inherited Create;
  FHgIDEClient := HgIDEClient;
  FNonFileIdentifiers := TStringList.Create;
  FNonFileIdentifiers.Sorted := True;
  InitNonFileIdentifiers;
end;

destructor THgNotifier.Destroy;
begin
  FNonFileIdentifiers.Free;
  inherited Destroy;
end;

procedure THgNotifier.Destroyed;
begin

end;

procedure THgNotifier.FileBrowserMenu(const IdentList: TStrings;
  const FileBrowserMenuList: IInterfaceList; IsMultiSelect: Boolean);
begin
  if (IdentList.Count = 1) and DirectoryExists(IdentList[0]) and
    IDEClient.HgClient.IsVersioned(IdentList[0]) then
  begin
    FileBrowserMenuList.Add(FBMMSvnParent);
    FileBrowserMenuList.Add(FBMMCommit);
    FileBrowserMenuList.Add(FBMMLog);
  end;
end;

function THgNotifier.GetAddNewProjectCaption: string;
begin
  Result := '';//Import is not yet supported
end;

function THgNotifier.GetAddNewProjectEnabled: Boolean;
begin
  Result := False;
end;

function THgNotifier.GetCheckoutMenuCaption: string;
begin
  Result := sMenuOpenFromVersionControl;
end;

function THgNotifier.GetDisplayName: string;
begin
  Result := sMercurial;
end;

function THgNotifier.GetImageIndex: Integer;
begin
  Result := MercurialImageIndex;
end;

function THgNotifier.GetName: string;
begin
  Result := sMercurialName;
end;

procedure THgNotifier.InitNonFileIdentifiers;
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

function THgNotifier.IsFileManaged(const Project: IOTAProject;
  const IdentList: TStrings): Boolean;

  function SaveIsPathVersioned(const APathName: string): Boolean;
  begin
    if FileExists(APathName) then
    begin
      try
        Result := IDEClient.HgClient.IsVersioned(APathName);
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

procedure THgNotifier.Modified;
begin

end;

procedure THgNotifier.ProjectManagerMenu(const Project: IOTAProject;
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
  //ProjectManagerMenuList.Add(PMMSvnParent);//so far the file menu doesn't exist -> add root item only in the Project branch
  if ContainersProject then
  begin
    ProjectManagerMenuList.Add(PMMSvnParent);
    ProjectManagerMenuList.Add(PMMParentCommit);
    ProjectManagerMenuList.Add(PMMRootDirCommit);
    ProjectManagerMenuList.Add(PMMProjectDirCommit);
    ProjectManagerMenuList.Add(PMMExpicitFilesCommit);
    ProjectManagerMenuList.Add(PMMParentUpdate);
    ProjectManagerMenuList.Add(PMMRootDirUpdate);
    ProjectManagerMenuList.Add(PMMProjectDirUpdate);
    ProjectManagerMenuList.Add(PMMExpicitFilesUpdate);
    ProjectManagerMenuList.Add(PMMParentLogHgMenu);
    ProjectManagerMenuList.Add(PMMRootDirLogHgMenu);
    ProjectManagerMenuList.Add(PMMProjectDirLogHgMenu);
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
  end;
end;

var
  NotifierIndex: Integer;

procedure RegisterMenus(AHgIDEClient: THgIDEClient);
begin
  NotifierIndex := (BorlandIDEServices as IOTAVersionControlServices).AddNotifier(THgNotifier.Create(AHgIDEClient));
  PMMSvnParent := TParentHgMenu.Create;
  PMMParentCommit := TParentCommitHgMenu.Create;
  PMMRootDirCommit := TRootDirCommitHgMenu.Create(AHgIDEClient);
  PMMProjectDirCommit := TProjectDirCommitHgMenu.Create(AHgIDEClient);
  //PMMExpicitFilesCommit := TExpicitFilesCommitHgMenu.Create(AHgIDEClient);
  {//TODO:1
  PMMFileCommit := TFileCommitSvnMenu.Create(ASvnIDEClient);
  PMMParentUpdate := TParentUpdateSvnMenu.Create;
  PMMRootDirUpdate := TRootDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirUpdate := TProjectDirUpdateSvnMenu.Create(ASvnIDEClient);
  PMMExpicitFilesUpdate := TExpicitFilesUpdateSvnMenu.Create(ASvnIDEClient);
  PMMFileUpdate := TFileUpdateSvnMenu.Create(ASvnIDEClient);
  PMMParentCleanSvnMenu := TParentCleanSvnMenu.Create;
  PMMRootDirCleanSvnMenu := TRootDirCleanSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirCleanSvnMenu := TProjectDirCleanSvnMenu.Create(ASvnIDEClient);
  }
  PMMParentLogHgMenu := TParentLogHgMenu.Create;
  PMMRootDirLogHgMenu := TRootDirLogHgMenu.Create(AHgIDEClient);
  PMMProjectDirLogHgMenu := TProjectDirLogHgMenu.Create(AHgIDEClient);
  {
  PMMParentRepo := TParentRepoSvnMenu.Create;
  PMMRootDirRepo := TRootDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirRepo := TProjectDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMFileRepoSvnMenu := TFileRepoSvnMenu.Create(ASvnIDEClient);
  }

  FBMMSvnParent := TParentHgMenu.Create;
  FBMMCommit := TDirCommitHgMenu.Create(AHgIDEClient);
  FBMMLog := TDirLogHgMenu.Create(AHgIDEClient);
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
  PMMParentLogHgMenu := nil;
  PMMRootDirLogHgMenu := nil;
  PMMProjectDirLogHgMenu := nil;
  PMMParentRepo := nil;
  PMMRootDirRepo := nil;
  PMMProjectDirRepo := nil;
  PMMFileRepoSvnMenu := nil;

  FBMMSvnParent := nil;
  FBMMCommit := nil;
  FBMMLog := nil;
end;

{ TParentHgMenu }

constructor TParentHgMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMHgParent;
  FVerb := sPMVHgParent;
  FPosition := pmmpUserVersionControl;
  FHelpContext := 0;
end;

function TParentHgMenu.GetImageIndex: Integer;
begin
  Result := MercurialImageIndex;
end;

end.
