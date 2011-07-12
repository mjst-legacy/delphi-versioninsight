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
{ The Original Code is HgIDEHistory.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEHistory;

interface

uses
  Classes, FileHistoryAPI, HgClient, HgIDEClient;

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

  THgFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider,
    IOTAAsynchronousHistoryProvider)
  private
    FClient: THgClient;
    FItems: TStringList;
    FHgIDEClient: THgIDEClient;

    procedure ClearItems;
    function CheckHgInitalize: Boolean;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;

    { IOTAAsynchronousHistoryProvider }
    procedure StartAsynchronousUpdate(const AFileName: WideString;
      const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
  public
    constructor Create(HgIDEClient: THgIDEClient);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  IHgFileHistory = interface(IOTAFileHistory)
    ['{F9294E84-FC13-4B24-9596-7250135CBDA8}']
    function GetItem: THgItem; safecall;
    property Item: THgItem read GetItem;
  end;

implementation

uses
  ComObj, ActiveX, SysUtils, Forms, Windows, ExtCtrls, HgIDEConst;

const
  HgFileHistoryProvider = 'VersionInsight.HgFileHistoryProvider';  //Do not internationalize

type
  THgFileHistory = class(TDispInterfacedObject, IOTAFileHistory, IHgFileHistory,
    IOTAFileHistoryHint, IOTAAsynchronousAnnotationProvider)
  private
    FItem: THgItem;

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

    { IHgFileHistory }
    function GetItem: THgItem; safecall;

    { IOTAFileHistoryHint }
    function GetHintStr(Index: Integer): string;

    { IOTAAsynchronousAnnotationProvider }
    function CanAnnotateFile(const FileName: string): Boolean;
    procedure StartAsynchronousUpdate(const FileName: string;
      FileHistoryIndex: Integer; const AnnotationCompletion: IOTAAnnotationCompletion);
  public
    constructor Create(AItem: THgItem);
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

{ THgFileHistoryProvider }

function THgFileHistoryProvider.CheckHgInitalize: Boolean;
begin
  try
    Result := FHgIDEClient.HgInitialize;
    if Result then
      FClient := FHgIDEClient.HgClient;
  except
    Result := False;
  end;
end;

procedure THgFileHistoryProvider.ClearItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

constructor THgFileHistoryProvider.Create(HgIDEClient: THgIDEClient);
begin
  inherited Create;
  FClient := nil;
  FHgIDEClient := HgIDEClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

destructor THgFileHistoryProvider.Destroy;
begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited;
end;

function THgFileHistoryProvider.GetFileHistory(
  const AFileName: WideString): IOTAFileHistory;
var
  Index: Integer;
  Item: THgItem;
begin
  Result := nil;
  if not CheckHgInitalize then
    Exit;

  if not FClient.IsVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
  begin
    Item := THgItem(FItems.Objects[Index]);
  end
  else
  begin
    Item := THgItem.Create(FClient, AFileName);
    try
      Item.LoadHistory;
      FItems.AddObject({Item.PathName}AFileName, Item);
    except
      Item.Free;
      raise;
    end;
  end;
  Result := THgFileHistory.Create(Item);
end;

function THgFileHistoryProvider.Get_Ident: WideString;
begin
  Result := HgFileHistoryProvider;
end;

function THgFileHistoryProvider.Get_Name: WideString;
begin
  Result := 'Hg history provider'; // Do not internationalize
end;

function THgFileHistoryProvider.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

type
  THgHistoryThread = class(TThread)
  private
    FAsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater;
    FHgItem: THgItem;
    FFileHistory: IOTAFileHistory;
    procedure Completed(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AHgItem: THgItem; AFileHistory: IOTAFileHistory; AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
  end;

{ THgHistoryThread }

procedure THgHistoryThread.Completed(Sender: TObject);
begin
  FAsynchronousHistoryUpdater.UpdateHistoryItems(FFileHistory, 0, FHgItem.HistoryCount - 1);
  FAsynchronousHistoryUpdater.Completed;
end;

constructor THgHistoryThread.Create(AHgItem: THgItem; AFileHistory: IOTAFileHistory; AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
begin
  FHgItem := AHgItem;
  FFileHistory := AFileHistory;
  FAsynchronousHistoryUpdater := AsynchronousHistoryUpdater;
  FreeOnTerminate := True;
  inherited Create;
  OnTerminate := Completed;
end;

procedure THgHistoryThread.Execute;
begin
  NameThreadForDebugging('VerIns Hg History Updater');
  FHgItem.LoadHistory;
end;

procedure THgFileHistoryProvider.StartAsynchronousUpdate(
  const AFileName: WideString;
  const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
var
  Index: Integer;
  Item: THgItem;
begin
  if (not CheckHgInitalize) or (not FClient.IsVersioned(AFileName)) then
  begin
    AsynchronousHistoryUpdater.Completed;
    Exit;
  end;

  if FItems.Find(AFileName, Index) then
  begin
    Item := THgItem(FItems.Objects[Index]);
    THgHistoryThread.Create(Item, THgFileHistory.Create(Item), AsynchronousHistoryUpdater);
  end
  else
  begin
    Item := THgItem.Create(FClient, AFileName);
    try
      FItems.AddObject({Item.PathName}AFileName, Item);
      THgHistoryThread.Create(Item, THgFileHistory.Create(Item), AsynchronousHistoryUpdater);
    except
      Item.Free;
      raise;
    end;
  end;
end;

{ THgFileHistory }

function THgFileHistory.CanAnnotateFile(const FileName: string): Boolean;
begin
  Result := True;
end;

constructor THgFileHistory.Create(AItem: THgItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor THgFileHistory.Destroy;
begin
//  FItem.Tag := 1;
  inherited;
end;

function THgFileHistory.GetAuthor(Index: Integer): WideString;
begin
  Result := THgHistoryItem(FItem.HistoryItems[Index]).Author;
end;

function THgFileHistory.GetComment(Index: Integer): WideString;
begin
  Result := THgHistoryItem(FItem.HistoryItems[Index]).Description;
end;

function THgFileHistory.GetContent(Index: Integer): IStream;
var
  Item: THgHistoryItem;
begin
  Item := FItem.HistoryItems[Index];
  Result := TStreamAdapter.Create(TStringStream.Create(Item.GetFile), soOwned);
end;

function TzToUTCDateTime(Value: TDateTime): TDateTime;
var
  TZ: TTimeZoneInformation;
begin
  Result := Value;
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result + (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result + (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result + TZ.Bias / MinsPerDay;
  end;
end;

function THgFileHistory.GetDate(Index: Integer): TDateTime;
begin
  Result := TzToUTCDateTime(THgHistoryItem(FItem.HistoryItems[Index]).Date);
end;

function THgFileHistory.GetHintStr(Index: Integer): string;
var
  Item: THgHistoryItem;
begin
  Item := THgHistoryItem(FItem.HistoryItems[Index]);
  Result := SAuthor + Item.Author + sLineBreak +
    STime + DateTimeToStr(Item.Date) + sLineBreak +
    SComment + Item.Summary;
end;

function THgFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;
{
var
  Item: THgHistoryItem;
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

function THgFileHistory.GetIdent(Index: Integer): WideString;
begin
  Result := IntToStr(FItem.HistoryItems[Index].ChangeSetID);
end;

function THgFileHistory.GetItem: THgItem;
begin
  Result := FItem;
end;

function THgFileHistory.GetLabelCount(Index: Integer): Integer;
begin
  Result := 1;
end;

function THgFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;
begin
  case LabelIndex of
    0: Result := FItem.HistoryItems[Index].ChangeSet;
  else
    Result := '';
  end;
end;

function THgFileHistory.Get_Count: Integer;
begin
  Result := FItem.HistoryCount;
end;

function THgFileHistory.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

type
  THgAnnotationLineProvider = class(TInterfacedObject, IOTAAnnotationLineProvider)
  private
    FHgHistoryItem: THgHistoryItem;
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
    constructor Create(AHgHistoryItem: THgHistoryItem);
  end;

{ THgAnnotationLineProvider }

constructor THgAnnotationLineProvider.Create(AHgHistoryItem: THgHistoryItem);
begin
  inherited Create;
  FHgHistoryItem := AHgHistoryItem;
  UpdateValues;
end;

function THgAnnotationLineProvider.GetCount: Integer;
begin
  Result := FHgHistoryItem.BlameCount;
end;

function THgAnnotationLineProvider.GetGutterInfo(Index: Integer): string;
begin
  Result := '';
  Dec(Index);
  if (Index >= 0) and (Index < FHgHistoryItem.BlameCount) and
    Assigned(FHgHistoryItem.BlameItems[Index].HistoryItem) then
      Result := IntToStr(FHgHistoryItem.BlameItems[Index].HistoryItem.ChangeSetID);
end;

function THgAnnotationLineProvider.GetHintStr(Index: Integer): string;
var
  Item: THgHistoryItem;
begin
  Result := '';
  Dec(Index);
  if (Index >= 0) and (Index < FHgHistoryItem.BlameCount) and
    Assigned(FHgHistoryItem.BlameItems[Index].HistoryItem) then
  begin
    Item := FHgHistoryItem.BlameItems[Index].HistoryItem;
    Result := SAuthor + Item.Author + sLineBreak +
      STime + DateTimeToStr(Item.Date) + sLineBreak +
      SComment + Item.Summary;
  end;
end;

function THgAnnotationLineProvider.GetIntensity(Index: Integer): Integer;
var
  Time: Integer;
  Diff: Integer;
begin
  Result := -1;
  Dec(Index);
  if (Index >= 0) and (Index < FHgHistoryItem.BlameCount) and
    Assigned(FHgHistoryItem.BlameItems[Index].HistoryItem) then
  begin
    Time := DateTimeToFileDate(FHgHistoryItem.BlameItems[Index].HistoryItem.Date);
    Diff := FMaxTime - FMinTime;
    Result := Trunc((Time - FMinTime) div ((Diff div 10) + 1)) * 100;
  end;
end;

function THgAnnotationLineProvider.GetMaxGutterWidth: Integer;
begin
  Result := FMaxGutterWidth;
end;

procedure THgAnnotationLineProvider.UpdateValues;
var
  I, L, ItemTime: Integer;
begin
  FMaxGutterWidth := 0;
  for I := 0 to FHgHistoryItem.BlameCount - 1 do
    if Assigned(FHgHistoryItem.BlameItems[I].HistoryItem) then
    begin
      L := Length(IntToStr(FHgHistoryItem.BlameItems[I].HistoryItem.ChangeSetID));
      if L > FMaxGutterWidth then
        FMaxGutterWidth := L;
    end;
  Inc(FMaxGutterWidth);
  FMinTime := 0;
  FMaxTime := 0;
  for I := 0 to FHgHistoryItem.Parent.HistoryCount - 1 do
  begin
    ItemTime := DateTimeToFileDate(FHgHistoryItem.Parent.HistoryItems[I].Date);
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
  THgBlameThread = class(TThread)
  private
    FAnnotationCompletion: IOTAAnnotationCompletion;
    FHgHistoryItem: THgHistoryItem;
    procedure Completed(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AHgHistoryItem: THgHistoryItem; AnnotationCompletion: IOTAAnnotationCompletion);
  end;

{ THgBlameThread }

procedure THgBlameThread.Completed(Sender: TObject);
begin
  FAnnotationCompletion.AnnotationComplete(THgAnnotationLineProvider.Create(FHgHistoryItem));
end;

constructor THgBlameThread.Create(AHgHistoryItem: THgHistoryItem; AnnotationCompletion: IOTAAnnotationCompletion);
begin
  FHgHistoryItem := AHgHistoryItem;
  FAnnotationCompletion := AnnotationCompletion;
  FreeOnTerminate := True;
  inherited Create;
  OnTerminate := Completed;
end;

procedure THgBlameThread.Execute;
begin
  NameThreadForDebugging('VerIns Hg Blame Updater');
  FHgHistoryItem.LoadBlame;
end;

procedure THgFileHistory.StartAsynchronousUpdate(const FileName: string;
  FileHistoryIndex: Integer;
  const AnnotationCompletion: IOTAAnnotationCompletion);
begin
  if (FileHistoryIndex >= 0) and (FileHistoryIndex < FItem.HistoryCount) then
    THgBlameThread.Create(FItem.HistoryItems[FileHistoryIndex], AnnotationCompletion)
  else
    AnnotationCompletion.AnnotationComplete(nil);
end;

initialization

end.
