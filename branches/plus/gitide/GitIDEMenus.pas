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
  Classes, ToolsAPI, GitIDEClient;

procedure RegisterMenus(AGitIDEClient: TGitIDEClient);
procedure UnRegisterMenus;


implementation

uses
  SysUtils, Dialogs, GitIDEConst, GitIDEIcons;

const
  sGitName = 'versioninsight.git';

type
  TGitNotifier = class(TInterfacedObject, IOTAVersionControlNotifier,
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
    FGitIDEClient: TGitIDEClient;
    FNonFileIdentifiers: TStringList;
  public
    constructor Create(const GitIDEClient: TGitIDEClient);
    destructor Destroy; override;
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
  MessageDlg('Sorry, Git clone is not yet supported.', mtInformation, [mbOK], 0);
  Result := False;
end;

function TGitNotifier.CheckoutProjectWithConnection(var ProjectName: string;
  const Connection: string): Boolean;
begin
  Result := False;
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
//there is no menu support for Git yet
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
  Result := '';//Checkout is not yet supported
end;

function TGitNotifier.GetDisplayName: string;
begin
  Result := sGit;
end;

function TGitNotifier.GetImageIndex: Integer;
begin
  Result := GitImageIndex;
end;

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
begin
end;

var
  NotifierIndex: Integer;

procedure RegisterMenus(AGitIDEClient: TGitIDEClient);
begin
  NotifierIndex := (BorlandIDEServices as IOTAVersionControlServices).AddNotifier(TGitNotifier.Create(AGitIDEClient));
end;

procedure UnRegisterMenus;
begin
  (BorlandIDEServices as IOTAVersionControlServices).RemoveNotifier(NotifierIndex);
end;

end.
