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
{ The Original Code is SvnIDEFileStates.pas.                                   }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2011 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEFileStates;

interface

procedure RegisterFileStateProvider;
procedure UnregisterFileStateProvider;

implementation

{$IFDEF TOOLSPROAPI}
uses
  SyncObjs, SysUtils, Classes, Graphics, Generics.Collections, ToolsAPI, DesignIntf, ToolsProAPI, HgClient,
  HgIDEClient, TypInfo;

type
  TOnStateEvent = procedure(const AFileName: string; AVersioned: Boolean; ATextStatus: THgStatus; AProperties: TStringList) of object;

  TSvnStateThread = class(TThread)
  private
    FSvnClient: THgClient;
    FItems: TStringList;
    FLock: TCriticalSection;
    FOnState: TOnStateEvent;
    procedure DoState(const AFileName: string; AVersioned: Boolean; ATextStatus: THgStatus; AProperties: TStringList);
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure AddItem(const AFileName: string);
    property OnState: TOnStateEvent read FOnState write FOnState;
  end;

  TSvnRetrievalState = (rsNew, rsRunning, rsFinished);

  TSvnState = class(TObject)
  private
    FFileName: string;
    FProperties: TStringList;
    FRetrievalState: TSvnRetrievalState;
    FState: TOTAProFileState;
  public
    constructor Create(const AFileName: string; AState: TOTAProFileState);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property Properties: TStringList read FProperties;
    property RetrievalState: TSvnRetrievalState read FRetrievalState write FRetrievalState;
    property State: TOTAProFileState read FState write FState;
  end;

  TSvnStateDir = class(TObject)
  private
    FDirectory: string;
    FItems: TObjectList<TSvnState>;
  public
    constructor Create(ADirectory: string);
    destructor Destroy; override;
    function GetState(const AFileName: string): TSvnState;
    property Directory: string read FDirectory;
    property Items: TObjectList<TSvnState> read FItems;
  end;

  TSvnStateDirList = class(TObject)
  private
    FItems: TObjectList<TSvnStateDir>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DeleteDirectory(const ADirectory: string);
    function IndexOf(const ADirectory: string): Integer;
    function GetDirectory(const ADirectory: string): TSvnStateDir;
  end;

  TIOTAProVersionControlFileStateProvider = class(TInterfacedObject,
    IOTAProVersionControlFileStateProvider)
  private
    FThread: TSvnStateThread;
    FItems: TSvnStateDirList;
    FLock: TCriticalSection;
    procedure HandleState(const AFileName: string; AVersioned: Boolean; ATextStatus: THgStatus; AProperties: TStringList);
  protected
    procedure FlushDir(const ADirectory: string);
    function GetFileState(const FileName: string; var AFileState: TOTAProFileState): TOTAProFileStateResult;
    function GetFileStateInfo(const FileName: string; var AProperty: IProperty): TOTAProFileStateResult;
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TSvnStateThread.Create(CreateSuspended: Boolean);
begin
  FSvnClient := IDEClient.HgClient;
  FLock := TCriticalSection.Create;
  FItems := TStringList.Create;
  FItems.Sorted := True;
  inherited Create(CreateSuspended);
end;

destructor TSvnStateThread.Destroy;
begin
  inherited Destroy;
  FItems.Free;
  FLock.Free;
end;

procedure TSvnStateThread.AddItem(const AFileName: string);
begin
  FLock.Enter;
  try
    if FItems.IndexOf(AFileName) = -1 then
      FItems.Add(AFileName);
  finally
    FLock.Leave;
  end;
end;

procedure TSvnStateThread.DoState(const AFileName: string; AVersioned: Boolean; ATextStatus: THgStatus;
  AProperties: TStringList);
begin
  if Assigned(FOnState) then
    FOnState(AFileName, AVersioned, ATextStatus, AProperties);
end;

procedure TSvnStateThread.Execute;
var
  FileName: string;
  TextStatus: THgStatus;
  SvnItem: THgItem;
  Locked: Boolean;
  Properties: TStringList;
  StatusStr: string;
begin
  NameThreadForDebugging('VerIns Hg State Retriever');
  while not Terminated do
  begin
    FileName := '';
    Locked := FLock.TryEnter;
    try
      if Locked and (FItems.Count > 0) then
      begin
        FileName := FItems[0];
        FItems.Delete(0);
      end;
    finally
      if Locked then
        FLock.Leave;
    end;
    if FileName <> '' then
    begin
      try
        if FSvnClient.IsVersioned(ExtractFilePath(FileName)) then
        begin
          Properties := TStringList.Create;
          try
            SvnItem := THgItem.Create(FSvnClient, FileName);
            try
              SvnItem.LoadStatus;
              TextStatus := SvnItem.Status;
              //TODO: Resourcestrings + StatusKindStr
              case TextStatus of
                gsAdded: StatusStr := 'Added';
                gsModified: StatusStr := 'Modified';
                gsNormal: StatusStr := 'Normal';
                gsUnknown: StatusStr := 'Unknown';
                gsUnversioned: StatusStr := 'Unversioned';
                gsMissing: StatusStr := 'Missing';
                gsDeleted: StatusStr := 'Deleted';
                else
                  StatusStr := '?';
              end;
              Properties.Add('Status=' + StatusStr);
              if not (TextStatus in [gsUnknown, gsUnversioned]) then
              begin
                SvnItem.LoadHistory(True);
                if SvnItem.HistoryCount >= 1 then
                begin
                  Properties.Add('Commit User=' + SvnItem.HistoryItems[0].Author);
                  Properties.Add('Commit Date=' + DateTimeToStr(SvnItem.HistoryItems[0].Date));
                  Properties.Add('Commit Changeset=' + SvnItem.HistoryItems[0].ChangeSet);
                  Properties.Add('Commit ChangesetID=' + IntToStr(SvnItem.HistoryItems[0].ChangeSetID));
                end;
              end;
            finally
              SvnItem.Free;
            end;
            DoState(FileName, True, TextStatus, Properties);
          finally
            Properties.Free;
          end;
        end
        else
          DoState(FileName, False, gsUnknown, nil);
      except
      end;
    end;
    Sleep(1);
  end;
end;

constructor TSvnState.Create(const AFileName: string; AState: TOTAProFileState);
begin
  inherited Create;
  FFileName := AFileName;
  FProperties := TStringList.Create;
  FRetrievalState := rsNew;
  FState := AState;
end;

destructor TSvnState.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

constructor TSvnStateDir.Create(ADirectory: string);
begin
  inherited Create;
  FDirectory := ADirectory;
  FItems := TObjectList<TSvnState>.Create;
end;

destructor TSvnStateDir.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TSvnStateDir.GetState(const AFileName: string): TSvnState;
var
  I, Idx: Integer;
  State: TOTAProFileState;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if AnsiSameText(FItems[I].FileName, AFileName) then
    begin
      Result := FItems[I];
      Break;
    end;
  if not Assigned(Result) then
  begin
    State.OverlayImageIndex := -1;
    State.StatusBarImageIndex := -1;
    State.DisplayText := '?';
    State.TextColor := clNone;
    Idx := FItems.Add(TSvnState.Create(AFileName, State));
    Result := FItems[Idx];
  end;
end;

constructor TSvnStateDirList.Create;
begin
  inherited Create;
  FItems := TObjectList<TSvnStateDir>.Create;
end;

procedure TSvnStateDirList.DeleteDirectory(const ADirectory: string);
var
  Idx: Integer;
begin
  Idx := IndexOf(ADirectory);
  if Idx <> -1 then
    FItems.Delete(Idx);
end;

destructor TSvnStateDirList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TSvnStateDirList.GetDirectory(const ADirectory: string): TSvnStateDir;
var
  Idx: Integer;
begin
  Idx := IndexOf(ADirectory);
  if Idx <> - 1 then
    Result := FItems[Idx]
  else
  begin
    Idx := FItems.Add(TSvnStateDir.Create(ADirectory));
    Result := FItems[Idx];
  end;
end;

function TSvnStateDirList.IndexOf(const ADirectory: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FItems.Count - 1 do
    if AnsiSameText(FItems[I].Directory, ADirectory) then
    begin
      Result := I;
      Break;
    end;
end;

{ TIOTAProVersionControlFileStateProvider }

constructor TIOTAProVersionControlFileStateProvider.Create;
begin
  inherited Create;
  FThread := TSvnStateThread.Create(True);
  FThread.OnState := HandleState;
  FItems := TSvnStateDirList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TIOTAProVersionControlFileStateProvider.Destroy;
begin
  FLock.Free;
  FItems.Free;
  if not FThread.Suspended then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
  FThread.Free;
  inherited Destroy;
end;

procedure TIOTAProVersionControlFileStateProvider.FlushDir(const ADirectory: string);
begin
  FLock.Enter;
  try
    FItems.DeleteDirectory(ADirectory);
  finally
    FLock.Leave;
  end;
end;

function TIOTAProVersionControlFileStateProvider.GetFileState(const FileName: string;
  var AFileState: TOTAProFileState): TOTAProFileStateResult;
var
  StateDir: TSvnStateDir;
  StartThread: Boolean;
  State: TSvnState;
begin
  Result := fsrError;
  FLock.Enter;
  try
    StateDir := FItems.GetDirectory(ExtractFilePath(FileName));
    if Assigned(StateDir) then
    begin
      State := StateDir.GetState(FileName);
      AFileState := State.State;
      if State.RetrievalState in [rsNew, rsRunning] then
        Result := fsrDeferred
      else
      if AFileState.DisplayText <> '?' then
        Result := fsrOK;
    end;
    StartThread := False;
    if (State.RetrievalState = rsNew) and (Result = fsrDeferred) then
    begin
      FThread.AddItem(FileName);
      State.RetrievalState := rsRunning;
      StartThread := True;
    end;
  finally
    FLock.Leave;
  end;
  if StartThread and FThread.Suspended then
    FThread.Resume;
end;

type
  TSimpleStringProperty = class(TInterfacedObject, IProperty, IPropertyDescription, IPropertyKind)
  private
    FDescription: string;
    FName: string;
    FValue: string;
    FHasSub: Boolean;
    FReadOnly: Boolean;
    FSubproperties: TStringList;
  protected
    { IPropertyDescription }
    function GetDescription: string;
    { IProperty }
    procedure Activate;
    function AllEqual: Boolean;
    function AutoFill: Boolean;
    procedure Edit; overload;
    function HasInstance(Instance: TPersistent): Boolean;
    function GetAttributes: TPropertyAttributes;
    function GetEditLimit: Integer;
    function GetEditValue(out Value: string): Boolean;
    function GetName: string;
    procedure GetProperties(Proc: TGetPropProc);
    function GetPropInfo: PPropInfo;
    function GetPropType: PTypeInfo;
    function GetValue: string;
    procedure GetValues(Proc: TGetStrProc);
    procedure Revert;
    procedure SetValue(const Value: string);
    function ValueAvailable: Boolean;
    { IPropertyKind }
    function GetKind: TTypeKind;
    property Kind: TTypeKind read GetKind;
  public
    constructor Create(const AName, ADescription, AValue: string; AReadOnly: Boolean = True; ASubItems: TStringList = nil);
    destructor Destroy; override;
    property HasSub: Boolean read FHasSub;
  end;

constructor TSimpleStringProperty.Create(const AName, ADescription, AValue: string; AReadOnly: Boolean = True; ASubItems: TStringList = nil);
begin
  inherited Create;
  FDescription := ADescription;
  FName := AName;
  FValue := AValue;
  FReadOnly := AReadOnly;
  FSubproperties := TStringList.Create;
  if Assigned(ASubItems) then
    FSubproperties.Assign(ASubItems);
end;

destructor TSimpleStringProperty.Destroy;
begin
  FSubproperties.Free;
  inherited Destroy;
end;

procedure TSimpleStringProperty.Activate;
begin
//
end;

function TSimpleStringProperty.AllEqual: Boolean;
begin
  Result := True;
end;

function TSimpleStringProperty.AutoFill: Boolean;
begin
  Result := False;
end;

procedure TSimpleStringProperty.Edit;
begin
//
end;

function TSimpleStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
  if FReadOnly then
  begin
    Include(Result, paDisplayReadOnly);
  end;
  if FSubproperties.Count > 0 then
    Include(Result, paSubProperties);
end;

function TSimpleStringProperty.GetDescription: string;
begin
  Result := FDescription;
end;

function TSimpleStringProperty.GetEditLimit: Integer;
begin
  Result := 255;
end;

function TSimpleStringProperty.GetEditValue(out Value: string): Boolean;
begin
  Result := False;
  Value := GetValue;
end;

function TSimpleStringProperty.GetKind: TTypeKind;
begin
  Result := tkString;
end;

function TSimpleStringProperty.GetName: string;
begin
  Result := FName;
end;

procedure TSimpleStringProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  S, S2: string;
begin
  for I := 0 to FSubproperties.Count - 1 do
  begin
    S := FSubproperties.Names[I];
    S2 := FSubproperties.ValueFromIndex[I];
    Proc(TSimpleStringProperty.Create(S, '', S2));
  end;
end;

function TSimpleStringProperty.GetPropInfo: PPropInfo;
begin
  Result := nil;
end;

function TSimpleStringProperty.GetPropType: PTypeInfo;
begin
  Result := nil;
end;

function TSimpleStringProperty.GetValue: string;
begin
  Result := FValue;
end;

procedure TSimpleStringProperty.GetValues(Proc: TGetStrProc);
begin
//
end;

function TSimpleStringProperty.HasInstance(Instance: TPersistent): Boolean;
begin
  Result := False;
end;

procedure TSimpleStringProperty.Revert;
begin
//
end;

procedure TSimpleStringProperty.SetValue(const Value: string);
begin
//
end;

function TSimpleStringProperty.ValueAvailable: Boolean;
begin
  Result := True;
end;

function TIOTAProVersionControlFileStateProvider.GetFileStateInfo(const FileName: string;
  var AProperty: IProperty): TOTAProFileStateResult;
var
  StateDir: TSvnStateDir;
  StartThread: Boolean;
  State: TSvnState;
begin
  Result := fsrError;
  FLock.Enter;
  try
    StateDir := FItems.GetDirectory(ExtractFilePath(FileName));
    if Assigned(StateDir) then
    begin
      State := StateDir.GetState(FileName);
      if State.RetrievalState in [rsNew, rsRunning] then
        Result := fsrDeferred
      else
      if State.State.DisplayText <> '?' then
        Result := fsrOK;
    end
    else
      State := nil;
    StartThread := False;
    if (State.RetrievalState = rsNew) and (Result = fsrDeferred) then
    begin
      FThread.AddItem(FileName);
      State.RetrievalState := rsRunning;
      StartThread := True;
    end;
    if Result = fsrOK then //TODO: Resourcestring
      AProperty := TSimpleStringProperty.Create('Mercurial Status', 'Mercurial Status', '', True, State.Properties);
  finally
    FLock.Leave;
  end;
  if StartThread and FThread.Suspended then
    FThread.Resume;
end;

procedure TIOTAProVersionControlFileStateProvider.HandleState(
  const AFileName: string; AVersioned: Boolean; ATextStatus: THgStatus;
  AProperties: TStringList);
var
  StateDir: TSvnStateDir;
  State: TSvnState;
  StateVar: TOTAProFileState;
  Idx: Integer;
begin
  FLock.Enter;
  try
    StateDir := FItems.GetDirectory(ExtractFilePath(AFileName));
    if Assigned(StateDir) then
    begin
      State := StateDir.GetState(AFileName);
      State.RetrievalState := rsFinished;
      if AVersioned then
      begin
        case ATextStatus of
          gsNormal: Idx := fsiNormal;
          gsModified: Idx := fsiModified;
          gsDeleted: Idx := fsiDeleted;
          gsAdded: Idx :=  fsiAdded;
          gsUnversioned: Idx := fsiNonVersioned;
          gsUnknown: Idx := -1;
          gsMissing: Idx := -1;
          else
            Idx := -1;
        end;
      end
      else
        Idx := -1;
      if not (BorlandIDEServices as IOTAProVersionControlServices).GetDefaultFileStateValues(Idx, StateVar) then
      begin
        StateVar.OverlayImageIndex := -1;
        StateVar.StatusBarImageIndex := -1;
        StateVar.DisplayText := '?';
        StateVar.TextColor := clNone;
      end;
      State.State := StateVar;
      if Assigned(AProperties) then
        State.Properties.Assign(AProperties)
      else
        State.Properties.Clear;
    end;
  finally
    FLock.Leave;
  end;
  (BorlandIDEServices as IOTAProVersionControlServices).InvalidateControls;
end;

var
  NotifierIndex: Integer = -1;

procedure RegisterFileStateProvider;
begin
  if Supports(BorlandIDEServices, IOTAProVersionControlServices) then
    NotifierIndex := (BorlandIDEServices as IOTAProVersionControlServices).RegisterFileStateProvider(TIOTAProVersionControlFileStateProvider.Create);
end;

procedure UnregisterFileStateProvider;
begin
  if NotifierIndex <> -1 then
  begin
    if Supports(BorlandIDEServices, IOTAProVersionControlServices) then
      (BorlandIDEServices as IOTAProVersionControlServices).UnregisterFileStateProvider(NotifierIndex);
    NotifierIndex := -1;
  end;
end;

{$ELSE ~TOOLSPROAPI}
procedure RegisterFileStateProvider;
begin
//
end;

procedure UnregisterFileStateProvider;
begin
//
end;
{$ENDIF TOOLSPROAPI}

initialization

finalization
  UnregisterFileStateProvider;

end.
