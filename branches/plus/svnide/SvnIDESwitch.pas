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
{ The Original Code is SvnIDESwitch.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDESwitch;

interface

uses Classes, Generics.Collections, SvnIDEMessageView, SvnIDEMenus, SvnClient, SvnIDEClient,
  SvnClientSwitch;

type
  TBaseSwitchSvnMenu = class(TSvnMenu)
  protected
    FSvnIDEClient: TSvnIDEClient;
    FRootType: TRootType;
    procedure DoSwitch(AParameter: TSwitchParameter);
    procedure Execute(const MenuContextList: IInterfaceList); override;
    function SelectLogRevisionCallBack(const AURL: string; var ASelectedRevision: Integer): Boolean;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentSwitchSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirSwitchSvnMenu = class(TBaseSwitchSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirSwitchSvnMenu = class(TBaseSwitchSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

implementation

uses SysUtils, SvnIDEConst, ToolsAPI, Controls, Forms, SvnIDEIcons,
  SvnIDETypes, SvnClientRepoBrowserDialog;

const
  sPMVSwitchParent = 'SvnSwitchParent';
  sPMVRootDirSwitch = 'RootDirSwitch';
  sPMVProjectDirSwitch = 'ProjectDirSwitch';
  sPMVDirSwitch = 'DirSwitch';

{ TBaseSwitchSvnMenu }

constructor TBaseSwitchSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FParent := sPMVSwitchParent;
  FSvnIDEClient := ASvnIDEClient;
end;

function BrowseURLCallBack(var AURL: string): Boolean;
begin
  Result := GetRepoURL(SvnIDEClient.IDEClient.SvnClient, AURL);
end;

procedure TBaseSwitchSvnMenu.DoSwitch(AParameter: TSwitchParameter);
begin
  TSwitchThread.Create(IDEClient, AParameter.Path, AParameter.URL, -1, AParameter.Revision,
    AParameter.Depth, AParameter.DepthIsSticky, AParameter.IgnoreExternals);
end;

procedure TBaseSwitchSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  DirectoryList, URLHistory: TStringList;
  ProjectFound, Switch: Boolean;
  Path, SvnPath, RootURL, URL: string;
  Parameter: TSwitchParameter;
begin
  URLHistory := TStringList.Create;
  DirectoryList := TStringList.Create;
  try
    BuildFileList(MenuContextList, DirectoryList, FSvnIDEClient.SvnClient, FRootType, ProjectFound);
    Path := DirectoryList[0];
    SvnPath := SvnExcludeTrailingPathDelimiter(FSvnIDEClient.SvnClient.NativePathToSvnPath(Path));
    RootURL := FSvnIDEClient.SvnClient.FindRepositoryRoot(Path);
    URL := FSvnIDEClient.SvnClient.FindRepository(Path);
    LoadURLHistory(URLHistory);
    Switch := GetSwitchInformation(URLHistory, RootURL, URL, SvnPath, Path, BrowseURLCallBack, SelectLogRevisionCallBack, Parameter);
    SaveURLHistory(URLHistory);
    if Switch then
      DoSwitch(Parameter);
  finally
    DirectoryList.Free;
    URLHistory.Free;
  end;
end;

function TBaseSwitchSvnMenu.SelectLogRevisionCallBack(const AURL: string; var ASelectedRevision: Integer): Boolean;
var
  LogDialogHelper: TLogDialogHelper;
  SelectedRevisions: TList<Integer>;
begin
  SelectedRevisions := TList<Integer>.Create;
  LogDialogHelper := TLogDialogHelper.Create(FSvnIDEClient.SvnClient, '', AURL);
  try
    Result := LogDialogHelper.Show(SelectedRevisions);
    if Result then
    begin
      Result := SelectedRevisions.Count > 0;
      ASelectedRevision := SelectedRevisions[0];
    end;
  finally
    LogDialogHelper.Free;
    SelectedRevisions.Free;
  end;
end;

{ TParentSwitchSvnMenu }

constructor TParentSwitchSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMSwitch;
  FVerb := sPMVSwitchParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentSwitchSvnMenu;
  FHelpContext := 0;
end;

function TParentSwitchSvnMenu.GetImageIndex: Integer;
begin
  Result := SwitchImageIndex;
end;

{ TRootDirSwitchSvnMenu }

constructor TRootDirSwitchSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirSwitch;
  FPosition := pmmpRootDirSwitchSvnMenu;
  FHelpContext := 0;
end;

{ TDirSwitchSvnMenu }

constructor TDirSwitchSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMSwitch;
  FVerb := sPMVDirSwitch;
  FPosition := pmmpProjectDirSwitchSvnMenu;
  FHelpContext := 0;
end;

function TDirSwitchSvnMenu.GetImageIndex: Integer;
begin
  Result := SwitchImageIndex;
end;

end.
