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
unit SvnIDENotifier;

interface

uses SvnUIUtils;

procedure RegisterFileNotification;

implementation

uses SysUtils, ToolsAPI, Classes, Generics.Collections, SvnIDEClient, Dialogs,
  svn_client, SvnClient, IOUtils;

type
  TProjectIndex = class
  protected
    FProject: IOTAProject;
    FIndex: Integer;
  public
    constructor Create(const AProject: IOTAProject; AIndex: Integer);
    property Project: IOTAProject read FProject;
    property Index: Integer read FIndex;
  end;

  TProjectList = TDictionary<string, TProjectIndex>;

  TFileNotifier = class(TInterfacedObject, IOTAIDENotifier)
  protected
    FProjectList: TProjectList;

  {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

  {IOTAIDENotifier}
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;

  {Misc}
    procedure ValueNotify(Sender: TObject; const Item: TProjectIndex;
      Action: TCollectionNotification);
   public
    constructor Create;
    destructor Destroy; override;
    property ProjectList: TProjectList read FProjectList;
  end;

  TProjectNotifier = class(TInterfacedObject, IOTAProjectNotifier, IOTAModuleNotifier)
  protected
    FFileName: string;
    FFileNotifier: TFileNotifier;
    FProject: IOTAProject;
    FStatus: TSvnWcStatusKind;
  {IOTANotifier}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  {IOTAModuleNotifier}
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string); overload;
  {IOTAProjectNotifier}
    procedure ModuleAdded(const AFileName: string);
    procedure ModuleRemoved(const AFileName: string);
    procedure ModuleRenamed(const AOldFileName, ANewFileName: string); overload;
  {Misc}
    procedure HandleAdd(const FileName: string);
    procedure HandleDelete(const FileName: string);
    procedure HandleRename(const InitialName, CurrentName: string);
    procedure StatusCallback(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
  public
    constructor Create(const FileNotifier: TFileNotifier; const FileName: string;
      const Project: IOTAProject);
  end;

var
  FileNotifier: TFileNotifier;
  NotifierIndex: Integer;

procedure RegisterFileNotification;
var
  Services: IOTAServices;
begin
  if Assigned(BorlandIDEServices) and
    BorlandIDEServices.GetService(IOTAServices, Services) then
  begin
    FileNotifier := TFileNotifier.Create;
    NotifierIndex := Services.AddNotifier(FileNotifier);
  end;
end;

procedure UnRegisterFileNotification;
var
  Services: IOTAServices;
begin
  if Assigned(BorlandIDEServices) and
    BorlandIDEServices.GetService(IOTAServices, Services) then
  begin
    FileNotifier := nil;
    Services.RemoveNotifier(NotifierIndex);
  end;
end;

{ TFileNotifier }

procedure TFileNotifier.AfterCompile(Succeeded: Boolean);
begin
// Not used.
end;

procedure TFileNotifier.AfterSave;
begin
// Not used.
end;

procedure TFileNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
// Not used.
end;

procedure TFileNotifier.BeforeSave;
begin
// Not used.
end;

constructor TFileNotifier.Create;
begin
  inherited Create;
  FProjectList := TDictionary<string, TProjectIndex>.Create(0, TOrdinalStringComparer.Create);
  FProjectList.OnValueNotify := ValueNotify;
end;

destructor TFileNotifier.Destroy;
var
  Enumerator: TDictionary<string, TProjectIndex>.TPairEnumerator;
begin
  Enumerator := ProjectList.GetEnumerator;
  while Enumerator.MoveNext do
//    with Enumerator.Current.Value do
    Enumerator.Current.Value.Project.RemoveNotifier(Enumerator.Current.Value.Index);
  inherited;
end;

procedure TFileNotifier.Destroyed;
begin
// Not used.
end;

procedure TFileNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  Module: IOTAModule;
  Index: Integer;
  Project: IOTAProject;
begin
  // Add Notifiers to all Projects
  if NotifyCode = ofnFileOpened then
  begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
    try
    if Supports(Module, IOTAProject, Project) then
      if IDEClient.SvnClient.IsPathVersioned(FileName) then
      begin
        Index := Module.AddNotifier(TProjectNotifier.Create(Self, FileName, Project));
        FProjectList.Add(FileName, TProjectIndex.Create(Project, Index));
      end;
    except
       // Ignore all error when trying to find out if the file is versioned.
       // If the file check fails for any reason do not install notifier.
      Exit;
    end;
  end;
end;

procedure TFileNotifier.Modified;
begin
// Not used.
end;

procedure TFileNotifier.ValueNotify(Sender: TObject; const Item: TProjectIndex;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

{ TProjectNotifier }

procedure TProjectNotifier.AfterSave;
var
  I, J: Integer;
  Module: IOTAModuleInfo;
  InitialName, CurrentName: string;
  FileList: IInterfaceList;
  AdditionalFiles: TStringList;
  BaseInitialName, BaseCurrentName: string;
  Ext: string;
begin
  if FProject <> nil then
  begin
    AdditionalFiles := TStringList.Create;
    try
    // Start an update so the transaction list is cleared of all user entries
    // at when finished.
      FProject.BeginFileTransactionUpdate;
      try
        for I := 0 to FProject.GetModuleCount - 1 do
        begin
          Module := FProject.GetModule(I);
          if FProject.GetFileTransaction(Module.FileName, InitialName, CurrentName) then
          begin
            BaseInitialName := ChangeFileExt(InitialName, '');
            BaseCurrentName := ChangeFileExt(CurrentName, '');
            AdditionalFiles.Clear;
            FProject.GetAssociatedFiles(Module.FileName, AdditionalFiles);
            for J := 0 to AdditionalFiles.Count - 1 do
            begin
              Ext := ExtractFileExt(AdditionalFiles[J]);
              if (InitialName = '') and (CurrentName <> '') then
                HandleAdd(ChangeFileExt(CurrentName, Ext))
              else if (InitialName <> '') and (CurrentName <> '') then
                HandleRename(ChangeFileExt(InitialName, Ext), ChangeFileExt(CurrentName, Ext));
            end;
          end;
        end;
        FileList := TInterfaceList.Create;
        FProject.GetAddedDeletedFiles(FileList);
        for I := 0 to FileList.Count - 1 do
          if (FileList[I] as IOTATransactionItem).TransactionType = ttDelete then
            HandleDelete((FileList[I] as IOTATransactionItem).FileName);
      finally
        FProject.EndFileTransactionUpdate(True);
      end;
    finally
      AdditionalFiles.Free
    end;
  end;
end;

procedure TProjectNotifier.BeforeSave;
begin
// Not used.
end;

function TProjectNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

constructor TProjectNotifier.Create(const FileNotifier: TFileNotifier;
  const FileName: string; const Project: IOTAProject);
begin
  inherited Create;
  FFileNotifier := FileNotifier;
  FFileName := FileName;
  FProject := Project;
end;

procedure TProjectNotifier.Destroyed;
begin
  FFileNotifier.ProjectList.Remove(FFileName);
  FProject := nil;
end;

procedure TProjectNotifier.HandleAdd(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    try
      IDEClient.SvnClient.GetModifications(FileName, StatusCallback, false);
      if FStatus = svnWcStatusUnversioned then
        IDEClient.SvnClient.Add(FileName);
    except
      // if we can not add silently fail.
    end;
  end;
end;

procedure TProjectNotifier.HandleDelete(const FileName: string);
begin

end;

procedure TProjectNotifier.HandleRename(const InitialName, CurrentName: string);
var
  TempFile: string;
  Files: TStringList;
begin
  if FileExists(CurrentName) then
  begin
    if FileExists(InitialName) then
    begin
      TempFile := TPath.GetTempFileName;
      if TFile.Exists(TempFile) then
        TFile.Delete(TempFile);
      TFile.Move(InitialName, TempFile);
    end
    else
      TempFile := '';
    try
      try
        IDEClient.SvnClient.GetModifications(CurrentName, StatusCallback, false);
        if FStatus = svnWcStatusUnversioned then
        begin
          TFile.Move(CurrentName, InitialName);
          Files := TStringList.Create;
          try
            Files.Add(InitialName);
            IDEClient.SvnClient.Move(Files, CurrentName);
          finally
            Files.Free;
          end;
        end;
      except
        if (not TFile.Exists(CurrentName)) and TFile.Exists(InitialName) then
          TFile.Move(InitialName, CurrentName);
        // if we can not move silently fail.
      end;
    finally
      if (TempFile <> '') and (TFile.Exists(TempFile)) then
        TFile.Move(TempFile, InitialName);
    end;
  end;
end;

procedure TProjectNotifier.Modified;
begin
// Not used.
end;

procedure TProjectNotifier.ModuleAdded(const AFileName: string);
begin

end;

procedure TProjectNotifier.ModuleRemoved(const AFileName: string);
begin

end;

procedure TProjectNotifier.ModuleRenamed(const NewName: string);
begin
// Not used.
end;

procedure TProjectNotifier.ModuleRenamed(const AOldFileName,
  ANewFileName: string);
begin

//  svn_client_move5
end;

procedure TProjectNotifier.StatusCallback(Sender: TObject; Item: TSvnItem;
  var Cancel: Boolean);
begin
  FStatus := Item.TextStatus;
end;

{ TProjectIndex }

constructor TProjectIndex.Create(const AProject: IOTAProject; AIndex: Integer);
begin
  inherited Create;
  FProject := AProject;
  FIndex := AIndex;
end;

initialization
finalization
  UnRegisterFileNotification;

end.
