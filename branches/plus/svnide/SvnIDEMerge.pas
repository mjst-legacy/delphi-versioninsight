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
{ The Original Code is SvnIDEMerge.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2011 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDEMerge;

interface

uses Classes, Generics.Collections, SvnIDEMessageView, SvnIDEMenus, SvnClient, SvnIDEClient,
  SvnClientMerge;

type
  TBaseMergeSvnMenu = class(TSvnMenu)
  protected
    FSvnIDEClient: TSvnIDEClient;
    FRootType: TRootType;
    procedure DoMerge(AParameter: TMergeParameter);
    procedure Execute(const MenuContextList: IInterfaceList); override;
    function SelectLogRevisionsCallBack(const AURL: string; ASelectedRevisions: TList<Integer>): Boolean;
    procedure ShowLogCallBack(const APath: string);
    procedure TestMergeCallBack(AParameter: TMergeParameter);
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentMergeSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirMergeSvnMenu = class(TBaseMergeSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TProjectDirMergeSvnMenu = class(TBaseMergeSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirMergeSvnMenu = class(TBaseMergeSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

implementation

uses SysUtils, SvnIDEConst, ToolsAPI, Controls, Forms, SvnIDEIcons,
  SvnIDETypes, SvnClientRepoBrowserDialog;

const
  sPMVMergeParent = 'SvnMergeParent';
  sPMVRootDirMerge = 'RootDirMerge';
  sPMVProjectDirMerge = 'ProjectDirMerge';
  sPMVDirMerge = 'DirMerge';

{ TBaseMergeSvnMenu }

constructor TBaseMergeSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FParent := sPMVMergeParent;
  FSvnIDEClient := ASvnIDEClient;
end;

function BrowseURLCallBack(var AURL: string): Boolean;
begin
  Result := GetRepoURL(SvnIDEClient.IDEClient.SvnClient, AURL);
end;

procedure TBaseMergeSvnMenu.DoMerge(AParameter: TMergeParameter);
var
  P, PegRevision, HeadRevision: Integer;
  RevisionStrWithoutPeg: string;
  RangesToMerge: TSvnMergeRevisionList;
begin
  RangesToMerge := TSvnMergeRevisionList.Create;
  try
    P := Pos('@', AParameter.RevisionStr);
    if P > 0 then
    begin
      RevisionStrWithoutPeg := Copy(AParameter.RevisionStr, 1, P -1);
      PegRevision := StrToIntDef(Copy(AParameter.RevisionStr, P + 1, MaxInt), - 1);
    end
    else
    begin
      RevisionStrWithoutPeg := AParameter.RevisionStr;
      PegRevision := -1;
    end;
    RangesToMerge.AsString := RevisionStrWithoutPeg;
    HeadRevision := -1;
    if Pos('HEAD', AnsiUpperCase(RevisionStrWithoutPeg)) > 0 then
      HeadRevision := IDEClient.SvnClient.GetHeadRevision(AParameter.URL);
    RangesToMerge.PrepareForMerge(HeadRevision, AParameter.ReverseMerge);
    TMergeRangeThread.Create(IDEClient, AParameter.URL, RangesToMerge, PegRevision, AParameter.TargetWcpath,
      AParameter.Depth, AParameter.IgnoreAncestry, AParameter.Force, AParameter.RecordOnly, AParameter.DryRun,
      AParameter.IgnoreEOL, AParameter.IgnoreSpace, AParameter.IgnoreAllSpace);
  finally
    RangesToMerge.Free;
  end;
end;

procedure TBaseMergeSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  DirectoryList, URLHistory: TStringList;
  ProjectFound, Merge: Boolean;
  Path: string;
  Parameter: TMergeParameter;
begin
  URLHistory := TStringList.Create;
  DirectoryList := TStringList.Create;
  try
    BuildFileList(MenuContextList, DirectoryList, FSvnIDEClient.SvnClient, FRootType, ProjectFound);
    Path := SvnExcludeTrailingPathDelimiter(FSvnIDEClient.SvnClient.NativePathToSvnPath(DirectoryList[0]));
    LoadURLHistory(URLHistory);
    Merge := GetMergeInformation(URLHistory, Path, BrowseURLCallBack, TestMergeCallBack,
      SelectLogRevisionsCallBack, ShowLogCallBack, Parameter);
    SaveURLHistory(URLHistory);
    if Merge then
      DoMerge(Parameter);
  finally
    DirectoryList.Free;
    URLHistory.Free;
  end;
end;

function TBaseMergeSvnMenu.SelectLogRevisionsCallBack(const AURL: string;
  ASelectedRevisions: TList<Integer>): Boolean;
var
  LogDialogHelper: TLogDialogHelper;
begin
  LogDialogHelper := TLogDialogHelper.Create(FSvnIDEClient.SvnClient, '', AURL);
  try
    Result := LogDialogHelper.Show(ASelectedRevisions);
  finally
    LogDialogHelper.Free;
  end;
end;

procedure TBaseMergeSvnMenu.ShowLogCallBack(const APath: string);
var
  LogDialogHelper: TLogDialogHelper;
begin
  LogDialogHelper := TLogDialogHelper.Create(FSvnIDEClient.SvnClient, APath, '');
  try
    LogDialogHelper.Show;
  finally
    LogDialogHelper.Free;
  end;
end;

procedure TBaseMergeSvnMenu.TestMergeCallBack(AParameter: TMergeParameter);
begin
  DoMerge(AParameter);
end;

{ TParentMergeSvnMenu }

constructor TParentMergeSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMMerge;
  FVerb := sPMVMergeParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentMergeSvnMenu;
  FHelpContext := 0;
end;

function TParentMergeSvnMenu.GetImageIndex: Integer;
begin
  Result := MergeImageIndex;
end;

{ TRootDirMergeSvnMenu }

constructor TRootDirMergeSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirMerge;
  FPosition := pmmpRootDirMergeSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirMergeSvnMenu }

constructor TProjectDirMergeSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirMerge;
  FPosition := pmmpProjectDirMergeSvnMenu;
  FHelpContext := 0;
end;

{ TDirMergeSvnMenu }

constructor TDirMergeSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMMerge;
  FVerb := sPMVDirMerge;
  FPosition := pmmpProjectDirMergeSvnMenu;
  FHelpContext := 0;
end;

function TDirMergeSvnMenu.GetImageIndex: Integer;
begin
  Result := MergeImageIndex;
end;

end.
