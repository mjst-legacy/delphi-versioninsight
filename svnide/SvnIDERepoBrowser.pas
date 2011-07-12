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
{ The Initial Developer of the Original Code is Ondrej Kelle.                  }
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
unit SvnIDERepoBrowser;

interface

uses Classes, SvnIDEMenus, SvnIDEClient;

type
  TBaseRepoSvnMenu = class(TSvnMenu)
  protected
    FRootType: TRootType;
    FSvnIDEClient: TSvnIDEClient;
    procedure Execute(const MenuContextList: IInterfaceList); override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TParentRepoSvnMenu = class(TSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create;
  end;

  TRootDirRepoSvnMenu = class(TBaseRepoSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TProjectDirRepoSvnMenu = class(TBaseRepoSvnMenu)
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TFileRepoSvnMenu = class(TBaseRepoSvnMenu)
  protected
    procedure Execute(const MenuContextList: IInterfaceList); override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

  TDirRepoSvnMenu = class(TBaseRepoSvnMenu)
  protected
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

implementation

uses ToolsApi, SysUtils, SvnIDEConst, SvnClient, SvnClientRepoBrowserFrame,
  DesignIntf, Forms, SvnIDEUtils, SvnUITypes, SvnIDEIcons;

const
  sPMVRepoParent = 'SvnRepoParent';
  sPMVRootDirRepo = 'SvnRootDirRepo';
  sPMVProjectDirRepo = 'SvnProjectDirRepo';
  sPMVFileRepo = 'SvnFileRepo';
  sPMVDirRepo = 'SvnDirRepo';

var
  RepoView: INTACustomEditorView;

type
  TRepoView = class(TInterfacedObject, INTACustomEditorView,
    INTACustomEditorView150)
  protected
    FSvnClient: TSvnClient;
    FSvnRepoFrame: TFrmRepoBrowser;
    FRootRepoPath: string;
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
  public
    constructor Create(SvnClient: TSvnClient; const ARootPath: string);
  end;

{ TBaseRepoSvnMenu }

constructor TBaseRepoSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FParent := sPMVRepoParent;
  FSvnIDEClient := ASvnIDEClient;
end;

procedure TBaseRepoSvnMenu.Execute(const MenuContextList: IInterfaceList);
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
    RepoView := TRepoView.Create(FSvnIDEClient.SvnClient, RootPath);
    (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(RepoView);
  end;
end;

{ TParentRepoSvnMenu }

constructor TParentRepoSvnMenu.Create;
begin
  inherited Create(nil);
  FCaption := sPMMRepo;
  FVerb := sPMVRepoParent;
  FParent := sPMVSvnParent;
  FPosition := pmmpParentRepoSvnMenu;
  FHelpContext := 0;
end;

function TParentRepoSvnMenu.GetImageIndex: Integer;
begin
  Result := RepoBrowserImageIndex;
end;

{ TRootDirRepoSvnMenu }

constructor TRootDirRepoSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtRootDir;
  FCaption := sPMMRootDir;
  FVerb := sPMVRootDirRepo;
  FPosition := pmmpRootDirRepoSvnMenu;
  FHelpContext := 0;
end;

{ TProjectDirRepoSvnMenu }

constructor TProjectDirRepoSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtProjectDir;
  FCaption := sPMMProjectDir;
  FVerb := sPMVProjectDirRepo;
  FPosition := pmmpProjectDirRepoSvnMenu;
  FHelpContext := 0;
end;

{ TFileRepoSvnMenu }

constructor TFileRepoSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FCaption := sPMMRepo;
  FVerb := sPMVFileRepo;
  FPosition := pmmpFileRepoSvnMenu;
  FParent := sPMVSvnParent;
  FHelpContext := 0;
end;

procedure TFileRepoSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  MenuContext: IOTAMenuContext;
begin
  if Supports(MenuContextList[0], IOTAMenuContext, MenuContext) then
  begin
    if FileExists(MenuContext.Ident) then
    begin
      RepoView := TRepoView.Create(FSvnIDEClient.SvnClient, ExtractFilePath(MenuContext.Ident));
      (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(RepoView);
    end;
  end;
end;

function TFileRepoSvnMenu.GetImageIndex: Integer;
begin
  Result := RepoBrowserImageIndex;
end;

{ TDirRepoSvnMenu }

constructor TDirRepoSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited Create(ASvnIDEClient);
  FRootType := rtDir;
  FParent := sPMVSvnParent;
  FCaption := sPMMRepo;
  FVerb := sPMVDirRepo;
  FPosition := pmmpProjectDirRepoSvnMenu;
  FHelpContext := 0;
end;

function TDirRepoSvnMenu.GetImageIndex: Integer;
begin
  Result := RepoBrowserImageIndex;
end;

{ TRepoView }

function TRepoView.CloneEditorView: INTACustomEditorView;
begin
  Result := nil;
end;

procedure TRepoView.Close(var Allowed: Boolean);
begin
  Allowed := True;
  RepoView := nil;
end;

procedure TRepoView.CloseAllCalled(var ShouldClose: Boolean);
begin
  ShouldClose := True;
end;

constructor TRepoView.Create(SvnClient: TSvnClient; const ARootPath: string);
begin
  inherited Create;
  FSvnClient := SvnClient;
  FRootRepoPath := SvnClient.FindRepository(ARootPath);
end;

procedure TRepoView.DeselectView;
begin
  // Not used
end;

function TRepoView.EditAction(Action: TEditAction): Boolean;
var
  SvnEditAction: TSvnEditAction;
begin
  Result := False;
  if Assigned(FSvnRepoFrame) then
  begin
    SvnEditAction := EditActionToSvnEditAction(Action);
    if SvnEditAction <> seaUnknown then
      Result := FSvnRepoFrame.PerformEditAction(SvnEditAction);
  end;
end;

procedure TRepoView.FrameCreated(AFrame: TCustomFrame);
begin
  FSvnRepoFrame := TFrmRepoBrowser(AFrame);
  FSvnRepoFrame.SvnClient := FSvnClient;
  FSvnRepoFrame.URL := FRootRepoPath;
end;

function TRepoView.GetCanCloneView: Boolean;
begin
  Result := False;
end;

function TRepoView.GetCaption: string;
begin
  Result := sRepoBrowser;
end;

function TRepoView.GetEditorWindowCaption: string;
begin
  Result := sRepoBrowser;
end;

function TRepoView.GetEditState: TEditState;
begin
  Result := [];
  if Assigned(FSvnRepoFrame) then
    Result := SvnEditStateToEditState(FSvnRepoFrame.SvnEditState);
end;

function TRepoView.GetFrameClass: TCustomFrameClass;
begin
  Result := TFrmRepoBrowser;
end;

function TRepoView.GetImageIndex: Integer;
begin
  Result := 0;
end;

function TRepoView.GetTabHintText: string;
begin
  Result := sRepoBrowser;
end;

function TRepoView.GetViewIdentifier: string;
begin
  Result := sRepoBrowser;
end;

procedure TRepoView.SelectView;
begin
  // Not used
end;

end.
