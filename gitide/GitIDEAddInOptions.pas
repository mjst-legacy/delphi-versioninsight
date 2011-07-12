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
{ The Original Code is GitIDEAddInOptions.pas.                                 }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEAddInOptions;

interface

uses
  Forms, ToolsAPI, GitAddInOptionsFrame;

type
  TGitAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TfrmGitTestsOptions;
  public
    procedure DialogClosed(Accepted: Boolean);
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetArea: string;
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    function GetHelpContext: Integer;
    function ValidateContents: Boolean;
    function IncludeInIDEInsight: Boolean;
  end;

procedure RegisterAddInOptions;

implementation

uses
  Registry, GitIDEClient;

{ TGitAddInOptions }

procedure TGitAddInOptions.DialogClosed(Accepted: Boolean);
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  if Accepted and (IDEClient.GitClient.GitExecutable <> FFrame.edGitExecutable.Text) then
  begin
    IDEClient.GitClient.GitExecutable := FFrame.edGitExecutable.Text;
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
    RegIniFile := TRegIniFile.Create(Key);
    try
      RegIniFile.WriteString('Git', 'Executable', IDEClient.GitClient.GitExecutable);
    finally
      RegIniFile.Free;
    end;
  end;
end;

procedure TGitAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TfrmGitTestsOptions(AFrame);
  FFrame.edGitExecutable.Text := IDEClient.GitClient.GitExecutable;
end;

function TGitAddInOptions.GetArea: string;
begin
  Result := 'Version Control';
end;

function TGitAddInOptions.GetCaption: string;
begin
  Result := 'Git';
end;

function TGitAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmGitTestsOptions;
end;

function TGitAddInOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TGitAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TGitAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  GitAddInOptions: TGitAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  GitAddInOptions := TGitAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(GitAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(GitAddInOptions);
  GitAddInOptions := nil;

end.
