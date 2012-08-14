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
{ The Original Code is SvnIDERevert.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDERevert;

interface

uses Classes, SvnIDEClient, SvnIDEMenus;

type
  TFileRevertSvnMenu = class(TSvnMenu)
  protected
    FSvnIDEClient: TSvnIDEClient;
    procedure Execute(const MenuContextList: IInterfaceList); override;
    function GetImageIndex: Integer; override;
  public
    constructor Create(ASvnIDEClient: TSvnIDEClient);
  end;

implementation

uses SysUtils, Controls, Dialogs, ToolsAPI, SvnIDEConst, SvnIDEMessageView,
  SvnIDEFileStates, SvnUIConst, SvnIDEIcons;

const
  sPMVRevert = 'Revert';

{ TFileRevertSvnMenu }

constructor TFileRevertSvnMenu.Create(ASvnIDEClient: TSvnIDEClient);
begin
  inherited;
  FSvnIDEClient := ASvnIDEClient;
  FParent := sPMVSvnParent;
  FCaption := sPMMRevert;
  FVerb := sPMVRevert;
  FPosition := pmmpFileRevertSvnMenu;
  FHelpContext := 0;
end;

procedure TFileRevertSvnMenu.Execute(const MenuContextList: IInterfaceList);
var
  I, J, AdditionalFileCount: Integer;
  MenuContext: IOTAMenuContext;
  ProjectContect: IOTAProjectMenuContext;
  FileList, AssociatedFiles: TStringList;
  S: string;
  Module: IOTAModule;
begin
  FileList := TStringList.Create;
  try
    for I := 0 to MenuContextList.Count - 1 do
      if Supports(MenuContextList[I], IOTAMenuContext, MenuContext) then
      begin
        FileList.Add(MenuContext.Ident);
        AssociatedFiles := TStringList.Create;
        try
          if Supports(MenuContext, IOTAProjectMenuContext, ProjectContect) and Assigned(ProjectContect.Project) then
            ProjectContect.Project.GetAssociatedFiles(MenuContext.Ident, AssociatedFiles)
          else
          begin
            Module := (BorlandIDEServices as IOTAModuleServices).FindModule(MenuContext.Ident);
            if Assigned(Module) then
              Module.GetAssociatedFilesFromModule(AssociatedFiles);
          end;
          for J := 0 to AssociatedFiles.Count - 1 do
            if FileList.IndexOf(AssociatedFiles[J]) = -1 then
              FileList.Add(AssociatedFiles[J]);
        finally
          AssociatedFiles.Free;
        end;
      end;
    if FileList.Count > 0 then
    begin
      S := sRevertCheck;
      if FileList.Count <= 6 then
      begin
        for I := 0 to FileList.Count - 1 do
          S := S + sLineBreak + Format('%s', [FileList[I]])
      end
      else
      begin
        AdditionalFileCount := 0;
        for I := 0 to FileList.Count - 1 do
          if I <= 4 then
            S := S + sLineBreak + Format('%s', [FileList[I]])
          else
            Inc(AdditionalFileCount);
        if AdditionalFileCount > 0 then
          S := S + sLineBreak + Format(sRevertDirMoreFiles, [AdditionalFileCount]);
      end;
      if MessageDlg(S, mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        try
          FSvnIDEClient.SvnClient.Revert(FileList, SvnMessageView.MessageViewCallBack, False);
        except
          if not HandleSvnException(ExceptObject) then
            raise;
        end;
        for I := 0 to FileList.Count - 1 do
        begin
          Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileList[I]);
          if Module <> nil then
            Module.Refresh(True);
        end;
        FlushFileListFileStates(FileList);
      end;
    end;
  finally
    FileList.Free;
  end;
end;

function TFileRevertSvnMenu.GetImageIndex: Integer;
begin
  Result := RevertImageIndex;
end;

end.
