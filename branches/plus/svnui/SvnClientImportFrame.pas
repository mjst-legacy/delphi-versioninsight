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
unit SvnClientImportFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CheckLst, SvnUITypes, SvnUIUtils, Clipbrd;

type

  TImportCallBack = function(const RepoPath, Comment: string;
    const RecentComments: TStringList; const URLHistory: TStringList): Boolean of object;
  TImportCloseCallBack = procedure of object;
  TBrowseURLCallBack = function(var AURL: string): Boolean of object;

  TSvnImportFrame = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    URL: TComboBoxEx;
    Browse: TButton;
    Top: TPanel;
    Center: TPanel;
    Bottom2: TPanel;
    Bottom3: TPanel;
    GroupBox2: TGroupBox;
    Comment: TMemo;
    RecentMessages: TButton;
    OutOfScopeFilesGroupBox: TGroupBox;
    OutOfScopeFiles: TListBox;
    Import: TButton;
    Bottom1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    FilesGroup: TGroupBox;
    CommitFiles: TCheckListBox;
    procedure ImportClick(Sender: TObject);
    procedure URLKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RecentMessagesClick(Sender: TObject);
    procedure BrowseClick(Sender: TObject);
    procedure URLSelect(Sender: TObject);
  protected
    FRecentComments: TStringList;
    FURLHistory: TStringList;
    FImportCallBack: TImportCallBack;
    FCloseCallBack: TImportCloseCallBack;
    FBrowseURLCallBack: TBrowseURLCallBack;
    FVersionedRoot: Boolean;
    class var FUseCount: Integer;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    function GetSvnEditState: TSvnEditState;
    procedure SetRecentComments(const Value: TStringList);
    procedure SetURLHistory(const Value: TStringList);
    procedure SetVersionedRoot(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetFiles(var Files: TStringList);
    function PerformEditAction(AEditAction: TSvnEditAction): Boolean;
    procedure SetFiles(Files: TStringList);
    procedure UpdateImportButton;
    property ImportCallBack: TImportCallBack read FImportCallBack write FImportCallBack;
    property CloseCallBack: TImportCloseCallBack read FCloseCallBack write FCloseCallBack;
    property BrowseURLCallBack: TBrowseURLCallBack read FBrowseURLCallBack write FBrowseURLCallBack;
    property RecentComments: TStringList read FRecentComments write SetRecentComments;
    property SvnEditState: TSvnEditState read GetSvnEditState;
    property URLHistory: TStringList read FURLHistory write SetURLHistory;
    property VersionedRoot: Boolean read FVersionedRoot write SetVersionedRoot;

  end;

implementation

uses SvnClientRecentComments;

{$R *.dfm}

procedure TSvnImportFrame.BrowseClick(Sender: TObject);
var
  SURL: string;
begin
  SURL := URL.Text;
  if FBrowseURLCallBack(SURL) then
  begin
    URL.Text := SURL;
    UpdateImportButton;
  end;
end;

procedure TSvnImportFrame.CMRelease(var Message: TMessage);
begin
  CloseCallBack;
end;

constructor TSvnImportFrame.Create(AOwner: TComponent);
begin
  inherited;
  Name := Format('%s_%d', [Name, FUseCount]);
  Inc(FUseCount);
  FRecentComments := TStringList.Create;
  FURLHistory := TStringList.Create;
end;

destructor TSvnImportFrame.Destroy;
begin
  FRecentComments.Free;
  FURLHistory.Free;
  inherited;
end;

procedure TSvnImportFrame.GetFiles(var Files: TStringList);
var
  I: Integer;
begin
  for I := 0 to CommitFiles.Items.Count - 1 do
    if CommitFiles.Checked[I] then
      Files.Add(CommitFiles.Items[I]);
end;

function TSvnImportFrame.GetSvnEditState: TSvnEditState;
var
  CustomListBox: TCustomListBox;
begin
  if URL.Focused then
    Result := ControlToSvnEditState(URL)
  else
  if CommitFiles.Focused or OutOfScopeFiles.Focused then
  begin
    if CommitFiles.Focused then
      CustomListBox := CommitFiles
    else
      CustomListBox := OutOfScopeFiles;
    if (CustomListBox.MultiSelect and (CustomListBox.SelCount > 0)) or
      (not CustomListBox.MultiSelect and (CustomListBox.ItemIndex <> -1)) then
    begin
      Result := [sesCanCopy];
      if CustomListBox.MultiSelect then
        Include(Result, sesCanSelectAll);
    end;
  end
  else
  if Comment.Focused then
    Result := ControlToSvnEditState(Comment)
  else
    Result := [];
end;

procedure TSvnImportFrame.ImportClick(Sender: TObject);
var
  Idx: Integer;
begin
  if Comment.Text <> '' then
  begin
    Idx := FRecentComments.IndexOf(Comment.Text);
    if Idx <> -1 then
      FRecentComments.Move(Idx, 0)
    else
      FRecentComments.Insert(0, Comment.Text);
  end;
  Idx := FURLHistory.IndexOf(URL.Text);
  if Idx <> -1 then
    FURLHistory.Move(Idx, 0)
  else
    FURLHistory.Insert(0, URL.Text);
  if Assigned(FImportCallBack) then
  begin
    if FImportCallBack(URL.Text, Comment.Text, FRecentComments, FURLHistory) then
      PostMessage(Handle, CM_Release, 0, 0);
  end;
end;

function TSvnImportFrame.PerformEditAction(AEditAction: TSvnEditAction): Boolean;
var
  I: Integer;
  SL: TStringList;
  CustomListBox: TCustomListBox;
begin
  if URL.Focused then
    Result := PerformDefaultSvnEditAction(URL, AEditAction)
  else
  if CommitFiles.Focused or OutOfScopeFiles.Focused then
  begin
    if CommitFiles.Focused then
      CustomListBox := CommitFiles
    else
      CustomListBox := OutOfScopeFiles;
    if CustomListBox.MultiSelect and (AEditAction = seaCopy) then
    begin
      SL := TStringList.Create;
      try
        for I := 0 to CustomListBox.Items.Count - 1 do
          if CustomListBox.Selected[I] then
            SL.Add(CustomListBox.Items.Strings[I]);
        Clipboard.AsText := SL.Text;
      finally
        SL.Free;
      end;
      Result := True;
    end
    else
    if not CustomListBox.MultiSelect and (AEditAction = seaCopy) and (CustomListBox.ItemIndex <> -1) then
    begin
      Clipboard.AsText := CustomListBox.Items[CustomListBox.ItemIndex];
      Result := True;
    end
    else
    if CustomListBox.MultiSelect and (AEditAction = seaSelectAll) then
    begin
      CustomListBox.SelectAll;
      Result := True;
    end
    else
      Result := False;
  end
  else
  if Comment.Focused then
  begin
    Result := PerformDefaultSvnEditAction(Comment, AEditAction);
    UpdateImportButton;
  end
  else
    Result := False;
end;

procedure TSvnImportFrame.RecentMessagesClick(Sender: TObject);
var
  S: string;
begin
  S := SelectRecentComments(Self, FRecentComments);
  if Comment.Text = '' then
    Comment.Text := S
  else
    Comment.Text := Comment.Text + ' ' + S;
end;

procedure TSvnImportFrame.SetFiles(Files: TStringList);
begin
  CommitFiles.Items.Assign(Files);
  CommitFiles.CheckAll(cbChecked);
end;

procedure TSvnImportFrame.SetRecentComments(const Value: TStringList);
begin
  FRecentComments.Assign(Value);
  RecentMessages.Enabled := FRecentComments.Count <> 0;
end;

procedure TSvnImportFrame.SetURLHistory(const Value: TStringList);
begin
  FURLHistory.Assign(Value);
  URL.Items.Assign(Value);
end;

procedure TSvnImportFrame.SetVersionedRoot(const Value: Boolean);
begin
  FVersionedRoot := Value;
  Url.Enabled := not FVersionedRoot;
  Browse.Enabled := not FVersionedRoot;
end;

procedure TSvnImportFrame.UpdateImportButton;
begin
  Import.Enabled := URL.Text <> '';
end;

procedure TSvnImportFrame.URLKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateImportButton;
end;

procedure TSvnImportFrame.URLSelect(Sender: TObject);
begin
  UpdateImportButton;
end;

initialization
  TSvnImportFrame.FUseCount := 0;
end.
