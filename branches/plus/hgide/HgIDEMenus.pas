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
  pmmpFileRevertSvnMenu = pmmpUserVersionControl + 1040;


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

uses
  {$IFDEF TOOLSPROAPI}
  HgIDEFileStates, VerInsIDEMacros,
  {$ENDIF TOOLSPROAPI}
  SysUtils, HgIDEConst, HgIDECommit{, SvnIDEUpdate, SvnIDEClean}, HgIDELog{,
  SvnIDEImport}, HgIDECheckout{, SvnIDERepoBrowser}, HgIDEIcons, HgIDERevert,
  Generics.Collections;

const
  sMercurialName = 'versioninsight.mercurial';

type
  TExecuteProc = procedure(HgIDEClient: THgIDEClient;
    const MenuContextList: IInterfaceList);

  THgNotifier = class(TInterfacedObject, IOTAVersionControlNotifier,
    IOTAVersionControlNotifier150 {$IFDEF TOOLSPROAPI}, IOTAProVersionControlNotifier155,
    IOTAProVersionControlSearchFileFind, IOTAProVersionControlVersionInfoNotifier{$ENDIF})
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
    { IOTAProVersionControlVersionInfoNotifier }
    function GetMacroCount: Integer;
    {$IFDEF TOOLSPROAPI}
    function GetMacros(AIndex: Integer): IOTAProMacro;
    {$ENDIF TOOLSPROAPI}
    procedure PrepareMacros(AProject: IOTAProject; AMacros: TStrings);
    function ExpandMacros(const S: string): string;
    { Misc }
    procedure InitMacroList;
    procedure InitNonFileIdentifiers;
    procedure MacroStatusCallback(Sender: TObject; Item: THgItem; var Cancel: Boolean);
  protected
    FHgIDEClient: THgIDEClient;
    FMacros: TInterfaceList;
    FMacroValues: TStringList;
    FNonFileIdentifiers: TStringList;
    FFoundModifications: Boolean;
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
  PMMParentRepo, PMMRootDirRepo, PMMProjectDirRepo, PMMFileRepoSvnMenu,
  PMMFileRevert: IOTAProjectManagerMenu;

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
  FMacros := TInterfaceList.Create;
  FMacroValues := TStringList.Create;
  FMacroValues.NameValueSeparator := #1;
  FNonFileIdentifiers := TStringList.Create;
  FNonFileIdentifiers.Sorted := True;
  InitMacroList;
  InitNonFileIdentifiers;
end;

destructor THgNotifier.Destroy;
begin
  FMacros.Free;
  FMacroValues.Free;
  FNonFileIdentifiers.Free;
  inherited Destroy;
end;

procedure THgNotifier.Destroyed;
begin

end;

function THgNotifier.ExpandMacros(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := 0 to FMacroValues.Count - 1 do
    Result := StringReplace(Result, '$(' + FMacroValues.Names[I] + ')', FMacroValues.ValueFromIndex[I], [rfReplaceAll]);
end;

procedure THgNotifier.FileBrowserMenu(const IdentList: TStrings;
  const FileBrowserMenuList: IInterfaceList; IsMultiSelect: Boolean);
begin
  if (IdentList.Count = 1) and DirectoryExists(IdentList[0]) and
    IDEClient.HgClient.IsPathInWorkingCopy(IdentList[0]) and
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

procedure THgNotifier.InitMacroList;
{$IFDEF TOOLSPROAPI}
var
  Macro: TOTAMacro;
begin
  //TODO: Resource strings
  FMacros.Add(TOTAMacro.Create('CHANGESET', 'Current working copy changeset'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Path', 'Path (Default = working copy root; . = project dir)');
  Macro.AddParameter('Format', 'Format');

  FMacros.Add(TOTAMacro.Create('CHANGESETID', 'Current working copy changeset ID'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Path', 'Path (Default = working copy root; . = project dir)');

  FMacros.Add(TOTAMacro.Create('CHANGESETAUTHOR', 'Current working copy changeset author'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Path', 'Path (Default = working copy root; . = project dir)');

  FMacros.Add(TOTAMacro.Create('CHANGESETAUTHOREMAIL', 'Current working copy changeset author email'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Path', 'Path (Default = working copy root; . = project dir)');

  FMacros.Add(TOTAMacro.Create('CHANGESETDATE', 'Current working copy changeset date'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Path', 'Path (Default = working copy root; . = project dir)');
  Macro.AddParameter('Format', 'Format');

  FMacros.Add(TOTAMacro.Create('UNCOMMITTEDCHANGES', 'Working copy contains uncommitted changes'));
  Macro := FMacros.Last as TOTAMacro;
  Macro.AddParameter('Paths', 'Paths');
  Macro.AddParameter('TrueStr', 'String if working copy contains uncommitted changes');
  Macro.AddParameter('FalseStr', 'String if working copy does not contain uncommitted changes');
{$ELSE ~TOOLSPROAPI}
begin
{$ENDIF ~TOOLSPROAPI}
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
        if IDEClient.HgClient.IsPathInWorkingCopy(ExtractFilePath(APathName)) then
          Result := IDEClient.HgClient.IsVersioned(APathName)
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

function THgNotifier.GetMacroCount: Integer;
begin
  Result := FMacros.Count;
end;

{$IFDEF TOOLSPROAPI}
function THgNotifier.GetMacros(AIndex: Integer): IOTAProMacro;
begin
  Result := FMacros[AIndex] as IOTAProMacro;
end;

function THgNotifier.GetModifiedFiles(const AModifiedFiles: TStrings; AProgress: IOTAProSearchFileFindProgress): Boolean;

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

procedure THgNotifier.MacroStatusCallback(Sender: TObject; Item: THgItem; var Cancel: Boolean);
begin
  if (not AnsiSameText(ExtractFileExt(Item.FileName), '.dproj')) and
    not (Item.Status in [gsUnknown, gsUnversioned]) then
  begin
    Cancel := True;
    FFoundModifications := True;
  end;
end;

procedure THgNotifier.Modified;
begin

end;

procedure THgNotifier.PrepareMacros(AProject: IOTAProject; AMacros: TStrings);
var
  HistoryItems: TObjectList<THgItem>;

  function GetPathLatestHistoryItem(const APath: string): THgHistoryItem;
  var
    I: Integer;
    Found: Boolean;
  begin
    Result := nil;
    Found := False;
    for I := 0 to HistoryItems.Count - 1 do
      if HistoryItems[I].FileName = APath then
      begin
        Found := True;
        if HistoryItems[I].HistoryCount > 0 then
          Result := HistoryItems[I].HistoryItems[0]
        else
          Result := nil;
      end;
    if not Found then
    begin
      HistoryItems.Add(THgItem.Create(IDEClient.HgClient, APath));
      HistoryItems.Last.LoadHistory(True);
      if HistoryItems.Last.HistoryCount > 0 then
        Result := HistoryItems.Last.HistoryItems[0]
      else
        Result := nil;
    end;
  end;

var
  I, J: Integer;
  Path: string;
  Author, DateStr, FormatStr, ChangeSetStr: string;
  MacroParts: TStringList;
  Paths, ModificationPaths: TStringList;
  HistoryItem: THgHistoryItem;
begin
  FMacroValues.Clear;
  MacroParts := TStringList.Create;
  HistoryItems := TObjectList<THgItem>.Create;
  try
    MacroParts.Delimiter := '|';
    MacroParts.StrictDelimiter := True;
    for I := 0 to AMacros.Count - 1 do
    begin
      MacroParts.DelimitedText := AMacros[I];
      if MacroParts.Count > 0 then
      begin
        if MacroParts[0] = 'CHANGESET' then
        begin
          Path := MacroParts.Values['Path'];
          if Path = '' then
            Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
          else
          if Pos('.', Path) = 1 then
            Path := ExtractFilePath(AProject.FileName) + Path;
          FormatStr := MacroParts.Values['Format'];
          HistoryItem := GetPathLatestHistoryItem(Path);
          if Assigned(HistoryItem) then
            ChangeSetStr := HistoryItem.ChangeSet
          else
            ChangeSetStr := '';
          if (ChangeSetStr <> '') and (FormatStr <> '') then
            ChangeSetStr := Format(FormatStr, [ChangeSetStr]);
          FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + '%s', [AMacros[I], ChangeSetStr]));
        end
        else
        if MacroParts[0] = 'CHANGESETID' then
        begin
          Path := MacroParts.Values['Path'];
          if Path = '' then
            Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
          else
          if Pos('.', Path) = 1 then
            Path := ExtractFilePath(AProject.FileName) + Path;
          HistoryItem := GetPathLatestHistoryItem(Path);
          if Assigned(HistoryItem) then
            ChangeSetStr := IntToStr(HistoryItem.ChangeSetID)
          else
            ChangeSetStr := '';
          FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + '%s', [AMacros[I], ChangeSetStr]));
        end
        else
        if MacroParts[0] = 'CHANGESETAUTHOR' then
        begin
          Path := MacroParts.Values['Path'];
          if Path = '' then
            Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
          else
          if Pos('.', Path) = 1 then
            Path := ExtractFilePath(AProject.FileName) + Path;
          HistoryItem := GetPathLatestHistoryItem(Path);
          if Assigned(HistoryItem) then
            Author := HistoryItem.Author
          else
            Author := '';
          FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + '%s', [AMacros[I], Author]));
        end
        else
        if MacroParts[0] = 'CHANGESETAUTHOREMAIL' then
        begin
          Path := MacroParts.Values['Path'];
          if Path = '' then
            Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
          else
          if Pos('.', Path) = 1 then
            Path := ExtractFilePath(AProject.FileName) + Path;
          HistoryItem := GetPathLatestHistoryItem(Path);
          if Assigned(HistoryItem) then
            Author := HistoryItem.AuthorEmail
          else
            Author := '';
          FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + '%s', [AMacros[I], Author]));
        end
        else
        if MacroParts[0] = 'CHANGESETDATE' then
        begin
          Path := MacroParts.Values['Path'];
          if Path = '' then
            Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
          else
          if Pos('.', Path) = 1 then
            Path := ExtractFilePath(AProject.FileName) + Path;
          FormatStr := MacroParts.Values['Format'];
          HistoryItem := GetPathLatestHistoryItem(Path);
          if Assigned(HistoryItem) then
          begin
            if FormatStr <> '' then
              DateStr := FormatDateTime(FormatStr, HistoryItem.Date)
            else
              DateStr := DateTimeToStr(HistoryItem.Date);
          end
          else
            DateStr := '';
          FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + '%s', [AMacros[I], DateStr]));
        end
        else
        if MacroParts[0] = 'UNCOMMITTEDCHANGES' then
        begin
          ModificationPaths := TStringList.Create;
          try
            Paths := TStringList.Create;
            try
              Paths.Delimiter := ';';
              Paths.StrictDelimiter := True;
              Paths.DelimitedText := MacroParts.Values['Paths'];
              if Paths.Count = 0 then
                ModificationPaths.Add(RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName)))
              else
              begin
                for J := 0 to Pred(Paths.Count) do
                begin
                  Path := Paths[J];
                  if Path = '' then
                    Path := RootDirectory(IDEClient.HgClient, ExtractFilePath(AProject.FileName))
                  else
                  if Pos('.', Path) = 1 then
                    Path := ExtractFilePath(AProject.FileName) + Path;
                  if ModificationPaths.IndexOf(Path) = -1 then
                    ModificationPaths.Add(Path);
                end;
              end;
            finally
              Paths.Free;
            end;
            FFoundModifications := False;
            for J := 0 to Pred(ModificationPaths.Count) do
            begin
              IDEClient.HgClient.GetModifications(ModificationPaths[J], MacroStatusCallback);
              if FFoundModifications then
                Break;
            end;
          finally
            ModificationPaths.Free;
          end;
          if FFoundModifications then
            FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + MacroParts.Values['TrueStr'], [AMacros[I]]))
          else
            FMacroValues.Add(Format('%s' + FMacroValues.NameValueSeparator + MacroParts.Values['FalseStr'], [AMacros[I]]))
        end;
      end;
    end;
  finally
    HistoryItems.Free;
    MacroParts.Free;
  end;
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
    ProjectManagerMenuList.Add(PMMFileRevert);
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
  PMMFileCommit := TFileCommitHgMenu.Create(AHgIDEClient);
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
  PMMParentLogHgMenu := TParentLogHgMenu.Create;
  PMMRootDirLogHgMenu := TRootDirLogHgMenu.Create(AHgIDEClient);
  PMMProjectDirLogHgMenu := TProjectDirLogHgMenu.Create(AHgIDEClient);
  {
  PMMParentRepo := TParentRepoSvnMenu.Create;
  PMMRootDirRepo := TRootDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMProjectDirRepo := TProjectDirRepoSvnMenu.Create(ASvnIDEClient);
  PMMFileRepoSvnMenu := TFileRepoSvnMenu.Create(ASvnIDEClient);
  }
  PMMFileRevert := TFileRevertHgMenu.Create(AHgIDEClient);

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
  PMMFileRevert := nil;

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
