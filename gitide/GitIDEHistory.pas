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
{ The Original Code is GitIDEHistory.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDEHistory;

interface

uses
  Classes, FileHistoryAPI, GitClient, GitIDEClient;

type
  TDispInterfacedObject = class(TInterfacedObject, IDispatch)
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  TGitFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider,
    IOTAAsynchronousHistoryProvider)
  private
    FClient: TGitClient;
    FItems: TStringList;
    FGitIDEClient: TGitIDEClient;

    procedure ClearItems;
    function CheckGitInitalize: Boolean;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;

    { IOTAAsynchronousHistoryProvider }
    procedure StartAsynchronousUpdate(const AFileName: WideString;
      const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
  public
    constructor Create(GitIDEClient: TGitIDEClient);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  IGitFileHistory = interface(IOTAFileHistory)
    ['{A29A1732-DA67-4B36-BF86-41978F8967D3}']
    function GetItem: TGitItem; safecall;
    property Item: TGitItem read GetItem;
  end;

implementation

uses
  ComObj, ActiveX, SysUtils, Forms, Windows, ExtCtrls, GitIDEConst;

const
  GitFileHistoryProvider = 'VersionInsight.GitFileHistoryProvider';  //Do not internationalize

type
  TGitFileHistory = class(TDispInterfacedObject, IOTAFileHistory, IGitFileHistory,
    IOTAFileHistoryHint, IOTAAsynchronousAnnotationProvider)
  private
    FItem: TGitItem;

    { IOTAFileHistory }
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;

    { IGitFileHistory }
    function GetItem: TGitItem; safecall;

    { IOTAFileHistoryHint }
    function GetHintStr(Index: Integer): string;

    { IOTAAsynchronousAnnotationProvider }
    function CanAnnotateFile(const FileName: string): Boolean;
    procedure StartAsynchronousUpdate(const FileName: string;
      FileHistoryIndex: Integer; const AnnotationCompletion: IOTAAnnotationCompletion);
  public
    constructor Create(AItem: TGitItem);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

{ TDispInterfacedObject }

function TDispInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDispInterfacedObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDispInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := S_OK;
  Count := 0;
end;

function TDispInterfacedObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TGitFileHistoryProvider }

function TGitFileHistoryProvider.CheckGitInitalize: Boolean;
begin
  try
    Result := FGitIDEClient.GitInitialize;
    if Result then
      FClient := FGitIDEClient.GitClient;
  except
    Result := False;
  end;
end;

procedure TGitFileHistoryProvider.ClearItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

constructor TGitFileHistoryProvider.Create(GitIDEClient: TGitIDEClient);
begin
  inherited Create;
  FClient := nil;
  FGitIDEClient := GitIDEClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

destructor TGitFileHistoryProvider.Destroy;
begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited;
end;

function TGitFileHistoryProvider.GetFileHistory(
  const AFileName: WideString): IOTAFileHistory;
var
  Index: Integer;
  Item: TGitItem;
begin
  Result := nil;
  if not CheckGitInitalize then
    Exit;

  if not FClient.IsVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
  begin
    Item := TGitItem(FItems.Objects[Index]);
  end
  else
  begin
    Item := TGitItem.Create(FClient, AFileName);
    try
      Item.LoadHistory;
      FItems.AddObject({Item.PathName}AFileName, Item);
    except
      Item.Free;
      raise;
    end;
  end;
  Result := TGitFileHistory.Create(Item);
end;

function TGitFileHistoryProvider.Get_Ident: WideString;
begin
  Result := GitFileHistoryProvider;
end;

function TGitFileHistoryProvider.Get_Name: WideString;
begin
  Result := 'Git history provider'; // Do not internationalize
end;

function TGitFileHistoryProvider.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

type
  TGitHistoryThread = class(TThread)
  private
    FAsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater;
    FGitItem: TGitItem;
    FFileHistory: IOTAFileHistory;
    procedure Completed(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AGitItem: TGitItem; AFileHistory: IOTAFileHistory; AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
  end;

{ TGitHistoryThread }

procedure TGitHistoryThread.Completed(Sender: TObject);
begin
  FAsynchronousHistoryUpdater.UpdateHistoryItems(FFileHistory, 0, FGitItem.HistoryCount - 1);
  FAsynchronousHistoryUpdater.Completed;
end;

constructor TGitHistoryThread.Create(AGitItem: TGitItem; AFileHistory: IOTAFileHistory; AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
begin
  FGitItem := AGitItem;
  FFileHistory := AFileHistory;
  FAsynchronousHistoryUpdater := AsynchronousHistoryUpdater;
  FreeOnTerminate := True;
  inherited Create;
  OnTerminate := Completed;
end;

procedure TGitHistoryThread.Execute;
begin
  NameThreadForDebugging('VerIns Git History Updater');
  FGitItem.LoadHistory;
end;

procedure TGitFileHistoryProvider.StartAsynchronousUpdate(
  const AFileName: WideString;
  const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
var
  Index: Integer;
  Item: TGitItem;
begin
  if (not CheckGitInitalize) or (not FClient.IsVersioned(AFileName)) then
  begin
    AsynchronousHistoryUpdater.Completed;
    Exit;
  end;

  if FItems.Find(AFileName, Index) then
  begin
    Item := TGitItem(FItems.Objects[Index]);
    TGitHistoryThread.Create(Item, TGitFileHistory.Create(Item), AsynchronousHistoryUpdater);
  end
  else
  begin
    Item := TGitItem.Create(FClient, AFileName);
    try
      FItems.AddObject({Item.PathName}AFileName, Item);
      TGitHistoryThread.Create(Item, TGitFileHistory.Create(Item), AsynchronousHistoryUpdater);
    except
      Item.Free;
      raise;
    end;
  end;
end;

{ TGitFileHistory }

function TGitFileHistory.CanAnnotateFile(const FileName: string): Boolean;
begin
  Result := True;
end;

constructor TGitFileHistory.Create(AItem: TGitItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor TGitFileHistory.Destroy;
begin
//  FItem.Tag := 1;
  inherited;
end;

function TGitFileHistory.GetAuthor(Index: Integer): WideString;
begin
  Result := TGitHistoryItem(FItem.HistoryItems[Index]).Author;
end;

function TGitFileHistory.GetComment(Index: Integer): WideString;
begin
  Result := TGitHistoryItem(FItem.HistoryItems[Index]).Subject + #13#10 +
    TGitHistoryItem(FItem.HistoryItems[Index]).Body;
end;

function TGitFileHistory.GetContent(Index: Integer): IStream;
var
  Item: TGitHistoryItem;
begin
  Item := FItem.HistoryItems[Index];
  Result := TStreamAdapter.Create(TStringStream.Create(Item.GetFile), soOwned);
end;

function TGitFileHistory.GetDate(Index: Integer): TDateTime;
begin
  Result := TGitHistoryItem(FItem.HistoryItems[Index]).Date;
end;

function TGitFileHistory.GetHintStr(Index: Integer): string;
var
  Item: TGitHistoryItem;
begin
  Item := TGitHistoryItem(FItem.HistoryItems[Index]);
  Result := SAuthor + Item.Author + sLineBreak +
    STime + DateTimeToStr(Item.Date) + sLineBreak +
    SComment + Item.Subject;
end;

function TGitFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;
{
var
  Item: TGitHistoryItem;
}
begin
  {
  Item := FItem.HistoryItems[Index];

  if Item.Revision = Item.Owner.CommittedRevision then
    Result := hsActiveRevision
  else
  }
    Result := hsRemoteRevision;
end;

function TGitFileHistory.GetIdent(Index: Integer): WideString;
begin
  Result := Copy(FItem.HistoryItems[Index].Hash, 1, 7);
end;

function TGitFileHistory.GetItem: TGitItem;
begin
  Result := FItem;
end;

function TGitFileHistory.GetLabelCount(Index: Integer): Integer;
begin
  Result := 1;
end;

function TGitFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;
begin
  case LabelIndex of
    0: Result := FItem.HistoryItems[Index].Hash;
  else
    Result := '';
  end;
end;

function TGitFileHistory.Get_Count: Integer;
begin
  Result := FItem.HistoryCount;
end;

function TGitFileHistory.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

type
  TGitAnnotationLineProvider = class(TInterfacedObject, IOTAAnnotationLineProvider)
  private
    FGitHistoryItem: TGitHistoryItem;
    FMaxGutterWidth: Integer;
    FMaxTime: Integer;
    FMinTime: Integer;

    { IOTAAnnotationLineProvider }
    function GetCount: Integer;
    function GetGutterInfo(Index: Integer): string;
    function GetIntensity(Index: Integer): Integer;
    function GetMaxGutterWidth: Integer;
    function GetHintStr(Index: Integer): string;

    procedure UpdateValues;
  public
    constructor Create(AGitHistoryItem: TGitHistoryItem);
  end;

{ TGitAnnotationLineProvider }

constructor TGitAnnotationLineProvider.Create(AGitHistoryItem: TGitHistoryItem);
begin
  inherited Create;
  FGitHistoryItem := AGitHistoryItem;
  UpdateValues;
end;

function TGitAnnotationLineProvider.GetCount: Integer;
begin
  Result := FGitHistoryItem.BlameCount;
end;

function TGitAnnotationLineProvider.GetGutterInfo(Index: Integer): string;
begin
  Result := '';
  Dec(Index);
  if (Index >= 0) and (Index < FGitHistoryItem.BlameCount) and
    Assigned(FGitHistoryItem.BlameItems[Index].HistoryItem) then
      Result := Copy(FGitHistoryItem.BlameItems[Index].HistoryItem.Hash, 1, 7);
end;

function TGitAnnotationLineProvider.GetHintStr(Index: Integer): string;
var
  Item: TGitHistoryItem;
begin
  Result := '';
  Dec(Index);
  if (Index >= 0) and (Index < FGitHistoryItem.BlameCount) and
    Assigned(FGitHistoryItem.BlameItems[Index].HistoryItem) then
  begin
    Item := FGitHistoryItem.BlameItems[Index].HistoryItem;
    Result := SAuthor + Item.Author + sLineBreak +
      STime + DateTimeToStr(Item.Date) + sLineBreak +
      SComment + Item.Subject;
  end;
end;

function TGitAnnotationLineProvider.GetIntensity(Index: Integer): Integer;
var
  Time: Integer;
  Diff: Integer;
begin
  Result := -1;
  Dec(Index);
  if (Index >= 0) and (Index < FGitHistoryItem.BlameCount) and
    Assigned(FGitHistoryItem.BlameItems[Index].HistoryItem) then
  begin
    Time := DateTimeToFileDate(FGitHistoryItem.BlameItems[Index].HistoryItem.Date);
    Diff := FMaxTime - FMinTime;
    Result := Trunc((Time - FMinTime) div ((Diff div 10) + 1)) * 100;
  end;
end;

function TGitAnnotationLineProvider.GetMaxGutterWidth: Integer;
begin
  Result := FMaxGutterWidth;
end;

procedure TGitAnnotationLineProvider.UpdateValues;
var
  I, L, ItemTime: Integer;
begin
  FMaxGutterWidth := 0;
  for I := 0 to FGitHistoryItem.BlameCount - 1 do
    if Assigned(FGitHistoryItem.BlameItems[I].HistoryItem) then
    begin
      L := Length(Copy(FGitHistoryItem.BlameItems[I].HistoryItem.Hash, 1, 7));
      if L > FMaxGutterWidth then
        FMaxGutterWidth := L;
    end;
  Inc(FMaxGutterWidth);
  FMinTime := 0;
  FMaxTime := 0;
  for I := 0 to FGitHistoryItem.Parent.HistoryCount - 1 do
  begin
    ItemTime := DateTimeToFileDate(FGitHistoryItem.Parent.HistoryItems[I].Date);
    if I = 0 then
    begin
      FMinTime := ItemTime;
      FMaxTime := ItemTime;
    end
    else
    if ItemTime < FMinTime then
      FMinTime := ItemTime
    else
    if ItemTime > FMaxTime then
      FMaxTime := ItemTime;
  end;
end;

type
  TGitBlameThread = class(TThread)
  private
    FAnnotationCompletion: IOTAAnnotationCompletion;
    FGitHistoryItem: TGitHistoryItem;
    procedure Completed(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AGitHistoryItem: TGitHistoryItem; AnnotationCompletion: IOTAAnnotationCompletion);
  end;

{ TGitBlameThread }

procedure TGitBlameThread.Completed(Sender: TObject);
begin
  FAnnotationCompletion.AnnotationComplete(TGitAnnotationLineProvider.Create(FGitHistoryItem));
end;

constructor TGitBlameThread.Create(AGitHistoryItem: TGitHistoryItem; AnnotationCompletion: IOTAAnnotationCompletion);
begin
  FGitHistoryItem := AGitHistoryItem;
  FAnnotationCompletion := AnnotationCompletion;
  FreeOnTerminate := True;
  inherited Create;
  OnTerminate := Completed;
end;

procedure TGitBlameThread.Execute;
begin
  NameThreadForDebugging('VerIns Git Blame Updater');
  FGitHistoryItem.LoadBlame;
end;

procedure TGitFileHistory.StartAsynchronousUpdate(const FileName: string;
  FileHistoryIndex: Integer;
  const AnnotationCompletion: IOTAAnnotationCompletion);
begin
  if (FileHistoryIndex >= 0) and (FileHistoryIndex < FItem.HistoryCount) then
    TGitBlameThread.Create(FItem.HistoryItems[FileHistoryIndex], AnnotationCompletion)
  else
    AnnotationCompletion.AnnotationComplete(nil);
end;

initialization

end.
