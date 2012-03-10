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
{ The Original Code is SvnIDEAddInOptions.pas.                                 }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDEAddInOptions;

interface

uses
  Forms, ToolsAPI, SvnClientOptionsFrame;

type
  TSvnAddInOptions = class(TInterfacedObject, INTAAddInOptions)
  private
    FFrame: TSvnOptionsFrame;
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
  SvnIDEClient, SvnIDEColors, SvnIDEConst;

{ TSvnAddInOptions }

procedure TSvnAddInOptions.DialogClosed(Accepted: Boolean);
var
  Colors: TSvnColorArray;
begin
  if Accepted then
  begin
    IDEClient.Colors.Enabled := FFrame.cbStatusColorsEnabled.Checked;
    Colors := IDEClient.Colors.Colors;
    Colors[ssckConflicted] := FFrame.cboxConflicted.Selected;
    Colors[ssckAdded] := FFrame.cboxAdded.Selected;
    Colors[ssckDeleted] := FFrame.cboxDeleted.Selected;
    Colors[ssckMerged] := FFrame.cboxMerged.Selected;
    Colors[ssckModified] := FFrame.cboxModified.Selected;
    IDEClient.Colors.Colors := Colors;
    IDEClient.Colors.Save;
    IDEClient.Options.AlternativeCommitLayout := FFrame.cbAlternativeCommitLayout.Checked;
    IDEClient.Options.ClearFileStatesAfterCloseAll := FFrame.cbClearFileStatesAfterCloseAll.Checked;
    IDEClient.Options.DeleteBackupFilesAfterCommit := FFrame.cbDeleteBackupFilesAfterCommit.Checked;
    IDEClient.Options.BlameOptions.IgnoreEOL := FFrame.IgnoreEOL.Checked;
    IDEClient.Options.BlameOptions.IgnoreSpace := FFrame.IgnoreSpace.Checked;
    IDEClient.Options.BlameOptions.IgnoreSpaceAll := FFrame.IgnoreAllSpace.Checked;
    IDEClient.Options.Save;
    IDEClient.SettingsModified;
  end;
end;

procedure TSvnAddInOptions.FrameCreated(AFrame: TCustomFrame);
begin
  FFrame := TSvnOptionsFrame(AFrame);
  FFrame.cbStatusColorsEnabled.Checked := IDEClient.Colors.Enabled;
  FFrame.cboxConflicted.Selected := IDEClient.Colors.Colors[ssckConflicted];
  FFrame.cboxAdded.Selected := IDEClient.Colors.Colors[ssckAdded];
  FFrame.cboxDeleted.Selected := IDEClient.Colors.Colors[ssckDeleted];
  FFrame.cboxMerged.Selected := IDEClient.Colors.Colors[ssckMerged];
  FFrame.cboxModified.Selected := IDEClient.Colors.Colors[ssckModified];
  FFrame.cbAlternativeCommitLayout.Checked := IDEClient.Options.AlternativeCommitLayout;
  {$IFNDEF TOOLSPROAPI}
  FFrame.cbClearFileStatesAfterCloseAll.Visible := False;
  {$ENDIF ~TOOLSPROAPI}
  FFrame.cbClearFileStatesAfterCloseAll.Checked := IDEClient.Options.ClearFileStatesAfterCloseAll;
  FFrame.cbDeleteBackupFilesAfterCommit.Checked := IDEClient.Options.DeleteBackupFilesAfterCommit;
  FFrame.IgnoreEOL.Checked := IDEClient.Options.BlameOptions.IgnoreEOL;
  if IDEClient.Options.BlameOptions.IgnoreSpaceAll then
    FFrame.IgnoreAllSpace.Checked := True
  else
  if IDEClient.Options.BlameOptions.IgnoreSpace then
    FFrame.IgnoreSpace.Checked := True
  else
    FFrame.CompareSpace.Checked := True;
end;

function TSvnAddInOptions.GetArea: string;
begin
  Result := sVersionControlAddInOptionArea;
end;

function TSvnAddInOptions.GetCaption: string;
begin
  Result := sSubversion;
end;

function TSvnAddInOptions.GetFrameClass: TCustomFrameClass;
begin
  Result := TSvnOptionsFrame;
end;

function TSvnAddInOptions.GetHelpContext: Integer;
begin
  Result := 0;
end;

function TSvnAddInOptions.IncludeInIDEInsight: Boolean;
begin
  Result := True;
end;

function TSvnAddInOptions.ValidateContents: Boolean;
begin
  Result := True;
end;

var
  SvnAddInOptions: TSvnAddInOptions = nil;

procedure RegisterAddInOptions;
begin
  SvnAddInOptions := TSvnAddInOptions.Create;
  (BorlandIDEServices as INTAEnvironmentOptionsServices).RegisterAddInOptions(SvnAddInOptions);
end;

initialization

finalization
  (BorlandIDEServices as INTAEnvironmentOptionsServices).UnregisterAddInOptions(SvnAddInOptions);
  SvnAddInOptions := nil;

end.
