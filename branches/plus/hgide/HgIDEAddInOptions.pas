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
{ The Original Code is HgIDEAddInOptions.pas.                                  }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEAddInOptions;

interface

uses
  Forms, ToolsAPI, HgAddInOptionsFrame;

type
  THgAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TfrmHgTestsOptions;
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
  Registry, HgIDEClient;

{ THgAddInOptions }

procedure THgAddInOptions.DialogClosed(Accepted: Boolean);
var
  RegIniFile: TRegIniFile;
  Key: string;
begin
  if Accepted and (IDEClient.HgClient.HgExecutable <> FFrame.edHgExecutable.Text) then
  begin
    IDEClient.HgClient.HgExecutable := FFrame.edHgExecutable.Text;
    Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight';
    RegIniFile := TRegIniFile.Create(Key);
    try
      RegIniFile.WriteString('Mercurial', 'Executable', IDEClient.HgClient.HgExecutable);
    finally
      RegIniFile.Free;
    end;
  end;
end;

procedure THgAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TfrmHgTestsOptions(AFrame);
  FFrame.edHgExecutable.Text := IDEClient.HgClient.HgExecutable;
end;

function THgAddInOptions.GetArea: string;
begin
  Result := 'Version Control';
end;

function THgAddInOptions.GetCaption: string;
begin
  Result := 'Mercurial';
end;

function THgAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TfrmHgTestsOptions;
end;

function THgAddInOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function THgAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function THgAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  HgAddInOptions: THgAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  HgAddInOptions := THgAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(HgAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(HgAddInOptions);
  HgAddInOptions := nil;

end.
