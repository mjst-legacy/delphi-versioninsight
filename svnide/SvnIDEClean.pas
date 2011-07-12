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
unit SvnIDEClean;

interface

uses Classes, SvnIDEMessageView, SvnIDEMenus, SvnClient, SvnIDEClient;

type
  TBaseCleanSvnMenu = class(TSvnMenu)
  protected
    FSvnIDEClient: TSvnIDEClient;
    FRootType: TRootType;
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentCleanSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirCleanSvnMenu = class(TBaseCleanSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TProjectDirCleanSvnMenu = class(TBaseCleanSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirCleanSvnMenu = class(TBaseCleanSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

procedure DoClean(const SvnClient: TSvnClient; const DirectoryList: TStringList);

implementation

uses SysUtils, SvnIDEConst, ToolsApi, Controls, Forms, SvnIDEIcons;

const
  sPMVCleanParent = 'SvnCleanParent';
  sPMVRootDirClean = 'RootDirClean';
  sPMVProjectDirClean = 'ProjectDirClean';
  sPMVDirClean = 'DirClean';

{ TBaseCleanSvnMenu }

constructor TBaseCleanSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FParent := sPMVCleanParent;
  FSvnIDEClient := ASvnIDEClient;
end;

procedure TBaseCleanSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  DirectoryList: TStringList;
  ProjectFound: Boolean;
begin
  DirectoryList := TStringList.Create;
  try
    BuildFileList(MenuContextList, DirectoryList, FSvnIDEClient.SvnClient, FRootType, ProjectFound);
    DoClean(FSvnIDEClient.SvnClient, DirectoryList);
  finally
    DirectoryList.Free;
  end;
end;

procedure DoClean(const SvnClient: TSvnClient; const DirectoryList: TStringList);
var
  UniqueList: TStringList;
  I: Integer;
  Cursor: TCursor;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    SvnMessageView.CheckMessageGroup(True);
    UniqueList := TStringList.Create;
    UniqueList.Sorted := True;
    UniqueList.CaseSensitive := False;
    try
      for I := 0 to DirectoryList.Count - 1 do
        if UniqueList.IndexOf(ExtractFilePath(DirectoryList[I])) = -1 then
          UniqueList.Add(ExtractFilePath(DirectoryList[I]));
      for I := 0 to UniqueList.Count - 1 do
      begin
        SvnMessageView.WriteTitle(sCleaning + UniqueList[I]);
        try
        SvnClient.Cleanup(UniqueList[I]);
        except
          if not HandleSvnException(ExceptObject) then
            raise;
        end;
      end;
    finally
      UniqueList.Free;
    end;
  finally
    Screen.Cursor := Cursor;
  end;
end;

{ TParentCleanSvnMenu }

constructor TParentCleanSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMClean;
  FVerb := sPMVCleanParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentCleanSvnMenu;
  FHelpContext := 0;
end;

function TParentCleanSvnMenu.GetImageIndex: Integer;
begin
  Result := CleanImageIndex;
end;

{ TRootDirCleanSvnMenu }

constructor TRootDirCleanSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirClean;
  FPosition := pmmpRootDirCleanSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirCleanSvnMenu }

constructor TProjectDirCleanSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirClean;
  FPosition := pmmpProjectDirCleanSvnMenu;
  FHelpContext := 0;
end;

{ TDirCleanSvnMenu }

constructor TDirCleanSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMClean;
  FVerb := sPMVDirClean;
  FPosition := pmmpProjectDirCleanSvnMenu;
  FHelpContext := 0;
end;

function TDirCleanSvnMenu.GetImageIndex: Integer;
begin
  Result := CleanImageIndex;
end;

end.
