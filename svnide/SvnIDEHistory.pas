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
{                                                                              }
{ This unit contains Subversion history provider, implementing OpenTools       }
{ FileHistory API interfaces.                                                  }
{                                                                              }
{******************************************************************************}

unit SvnIDEHistory;

interface

uses Classes, FileHistoryApi, SvnClient, SvnIDEClient;

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

  TSvnFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider,
    IOTAAsynchronousHistoryProvider, IOTAAsynchronousHistoryProvider150)
  private
    FClient: TSvnClient;
    FItems: TStringList;
    FSvnIDEClient: TSvnIDEClient;

    procedure ClearItems;
    function CheckSvnInitalize: Boolean;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;

    { IOTAAsynchronousHistoryProvider }
    procedure StartAsynchronousUpdate(const AFileName: WideString;
      const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
    { IOTAAsynchronousHistoryProvider150 }
    procedure TerminateAsynchronousUpdate(const AFileName: WideString; WaitForTerminate: Boolean);
  public
    constructor Create(SvnIDEClient: TSvnIDEClient);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  ISvnFileHistory = interface(IOTAFileHistory)
    ['{BA13BDFA-7E77-409E-9FEB-E282BD0F809D}']
    function GetItem: TSvnItem; safecall;
    property Item: TSvnItem read GetItem;
  end;

implementation

uses ComObj, ActiveX, SysUtils, Forms, Windows, SvnIDEConst, ExtCtrls;

const
  SvnFileHistoryProvider = 'TOndrej.SubversionFileHistoryProvider';  //Do not internationalize

type

  TSvnFileHistory = class(TDispInterfacedObject, IOTAFileHistory, ISvnFileHistory,
    IOTAAsynchronousAnnotationProvider, IOTAFileHistoryHint)
  private
    FItem: TSvnItem;

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

    { ISvnFileHistory }
    function GetItem: TSvnItem; safecall;

    { IOTAAsynchronousAnnotationProvider }
    function CanAnnotateFile(const FileName: string): Boolean;
    procedure StartAsynchronousUpdate(const FileName: string;
      FileHistoryIndex: Integer; const AnnotationCompletion: IOTAAnnotationCompletion);

    { IOTAFileHistoryHint }
    function GetHintStr(Index: Integer): string;
  public
    constructor Create(AItem: TSvnItem);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  TAnnotationLineProvider = class(TInterfacedObject, IOTAAnnotationLineProvider)
    { IAnnotationLineProvider }
    function GetCount: Integer;
    function GetGutterInfo(Index: Integer): string;
    function GetIntensity(Index: Integer): Integer;
    function GetMaxGutterWidth: Integer;
    function GetHintStr(Index: Integer): string;
  protected
    FAnnotationLineProvider: IAnnotationLineProvider;
  public
    constructor Create(const AnnotationLineProvider: IAnnotationLineProvider);
  end;

  TAnnotationCompletion = class(TInterfacedObject, IAnnotationCompletion)
    { IAnnotationCompletion }
    procedure AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
  protected
    FAnnotationCompletion: IOTAAnnotationCompletion;
  public
    constructor Create(const AAnnotationCompletion: IOTAAnnotationCompletion);
  end;

  TAsyncUpdate = class(TInterfacedObject, IAsyncUpdate)
  protected
    FAsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater;
    FFileHistory: IOTAFileHistory;
    FLastUpdate: Word;
    FFirst: Integer;
    { IAsyncUpdate }
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;
  public
    constructor Create(AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater;
      FileHistory: IOTAFileHistory);
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

{ TSvnFileHistoryProvider }

function TSvnFileHistoryProvider.CheckSvnInitalize: Boolean;
begin
  try
    Result := FSvnIDEClient.SvnInitialize;
    if Result then
      FClient := FSvnIDEClient.SvnClient;
  except
    Result := False;
    if ExceptObject is Exception then
      Application.MessageBox(PChar(Exception(ExceptObject).Message), PChar(sLoadError), MB_ICONERROR);
  end;
end;

procedure TSvnFileHistoryProvider.ClearItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

constructor TSvnFileHistoryProvider.Create(SvnIDEClient: TSvnIDEClient);
begin
  inherited Create;
  FClient := nil;
  FSvnIDEClient := SvnIDEClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

destructor TSvnFileHistoryProvider.Destroy;
begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited;
end;

function TSvnFileHistoryProvider.GetFileHistory(
  const AFileName: WideString): IOTAFileHistory;
var
  Index: Integer;
  Item: TSvnItem;
begin
  Result := nil;
  if not CheckSvnInitalize then
    Exit;

  if not FClient.IsPathVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
  begin
    Item := TSvnItem(FItems.Objects[Index]);
    if Item.Tag <> 0 then
    begin
      Item.Tag := 0;
      Item.Reload;
    end;
  end
  else
  begin
    Item := TSvnItem.Create(FClient, nil, AFileName);
    try
      FItems.AddObject(Item.PathName, Item);
    except
      Item.Free;
      raise;
    end;
  end;
  Result := TSvnFileHistory.Create(Item);
end;

function TSvnFileHistoryProvider.Get_Ident: WideString;
begin
  Result := SvnFileHistoryProvider;
end;

function TSvnFileHistoryProvider.Get_Name: WideString;
begin
  Result := 'Subversion history provider'; // Do not internationalize
end;

function TSvnFileHistoryProvider.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

procedure TSvnFileHistoryProvider.StartAsynchronousUpdate(
  const AFileName: WideString;
  const AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater);
var
  Index: Integer;
  Item: TSvnItem;
begin
  if (not CheckSvnInitalize) or (not FClient.IsPathVersioned(AFileName)) then
  begin
    AsynchronousHistoryUpdater.Completed;
    Exit;
  end;

  if FItems.Find(AFileName, Index) then
  begin
    Item := TSvnItem(FItems.Objects[Index]);
    if Item.Tag <> 0 then
    begin
      Item.Tag := 0;
      Item.Reload;
    end;
    if Item.AsyncUpdate <> nil then
      Item.ScheduleAsyncReload(TAsyncUpdate.Create(AsynchronousHistoryUpdater, TSvnFileHistory.Create(Item)))
    else if Item.AsyncUpdate = nil then
    begin
      Item.AsyncUpdate := TAsyncUpdate.Create(AsynchronousHistoryUpdater, TSvnFileHistory.Create(Item));
      Item.AsyncReloadHistory;
    end;
  end
  else
  begin
    Item := TSvnItem.Create(FClient, nil, AFileName);
    try
      FItems.AddObject(Item.PathName, Item);
      Item.Reload;
      Item.AsyncUpdate := TAsyncUpdate.Create(AsynchronousHistoryUpdater, TSvnFileHistory.Create(Item));
      Item.AsyncReloadHistory;
    except
      Item.Free;
      raise;
    end;
  end;
end;

procedure TSvnFileHistoryProvider.TerminateAsynchronousUpdate(
  const AFileName: WideString; WaitForTerminate: Boolean);
var
  Index: Integer;
  Item: TSvnItem;
begin
  if FItems.Find(AFileName, Index) then
  begin
    Item := TSvnItem(FItems.Objects[Index]);
    Item.TerminateAsynchronousUpdate(WaitForTerminate);
  end;
end;

{ TSvnFileHistory }

function TSvnFileHistory.CanAnnotateFile(const FileName: string): Boolean;
begin
  Result := True;
end;

constructor TSvnFileHistory.Create(AItem: TSvnItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor TSvnFileHistory.Destroy;
begin
  FItem.Tag := 1;
  inherited;
end;

function TSvnFileHistory.GetAuthor(Index: Integer): WideString;
begin
  Result := TSvnHistoryItem(FItem.HistoryItems[Index]).Author;
end;

function TSvnFileHistory.GetComment(Index: Integer): WideString;
begin
  Result := TSvnHistoryItem(FItem.HistoryItems[Index]).LogMessage;
end;

function TSvnFileHistory.GetContent(Index: Integer): IStream;
var
  Item: TSvnHistoryItem;
begin
  Item := FItem.HistoryItems[Index];
  if Item.Revision = Item.Owner.BaseRevision then
    Result := TStreamAdapter.Create(TStringStream.Create(Item.Owner.GetBaseFile), soOwned)
  else if Item.Revision = Item.Owner.CommittedRevision then
    Result := TStreamAdapter.Create(TStringStream.Create(Item.Owner.GetCommittedFile), soOwned)
  else
    Result := TStreamAdapter.Create(TStringStream.Create(Item.GetFile), soOwned);
end;

function TSvnFileHistory.GetDate(Index: Integer): TDateTime;
begin
  Result := TzToUTCDateTime(TSvnHistoryItem(FItem.HistoryItems[Index]).Time);
end;

function TSvnFileHistory.GetHintStr(Index: Integer): string;
begin
  Result := FItem.HintStrings[Index];
end;

function TSvnFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;
var
  Item: TSvnHistoryItem;
begin
  Item := FItem.HistoryItems[Index];
  if Item.Revision = Item.Owner.CommittedRevision then
    Result := hsActiveRevision
  else
    Result := hsRemoteRevision;
end;

function TSvnFileHistory.GetIdent(Index: Integer): WideString;
begin
  Result := IntToStr(FItem.HistoryItems[Index].Revision);
end;

function TSvnFileHistory.GetItem: TSvnItem;
begin
  Result := FItem;
end;

function TSvnFileHistory.GetLabelCount(Index: Integer): Integer;
begin
  Result := 1;
end;

function TSvnFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;
begin
  case LabelIndex of
    0: Result := GetIdent(Index);
  else
    Result := '';
  end;
end;

function TSvnFileHistory.Get_Count: Integer;
begin
  Result := FItem.HistoryCount;
end;

function TSvnFileHistory.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

procedure TSvnFileHistory.StartAsynchronousUpdate(const FileName: string;
  FileHistoryIndex: Integer;
  const AnnotationCompletion: IOTAAnnotationCompletion);
var
  HistoryItem: TSvnHistoryItem;
begin
  HistoryItem := FItem.HistoryItems[FileHistoryIndex];
  if not HistoryItem.HasBlameLoaded then
  begin
    if HistoryItem.BlameError = '' then
    begin
      if not HistoryItem.IsLoadingBlame then
        HistoryItem.StartLoadingBlame(TAnnotationCompletion.Create(AnnotationCompletion));
        Exit;
    end
  end;
  AnnotationCompletion.AnnotationComplete(TAnnotationLineProvider.Create(TSvnClientAnnotationLineProvider.Create(HistoryItem)));
end;

{ TAsyncUpdate }

procedure TAsyncUpdate.Completed;
begin
  FAsynchronousHistoryUpdater.Completed;
end;

constructor TAsyncUpdate.Create(
  AsynchronousHistoryUpdater: IOTAAsynchronousHistoryUpdater;
  FileHistory: IOTAFileHistory);
begin
  inherited Create;
  FAsynchronousHistoryUpdater := AsynchronousHistoryUpdater;
  FFileHistory := FileHistory;
  FLastUpdate := 0;
  FFirst := 0;
end;

procedure TAsyncUpdate.UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
  ForceUpdate: Boolean);
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  if (SystemTime.wSecond <> FLastUpdate) or ForceUpdate then
  begin
    FLastUpdate := SystemTime.wSecond;
    FAsynchronousHistoryUpdater.UpdateHistoryItems(FFileHistory, FFirst, LastNewIndex);
    FFirst := LastNewIndex + 1;
  end;
end;

{ TAnnotationCompletion }

procedure TAnnotationCompletion.AnnotationComplete(
  const AnnotationLineProvider: IAnnotationLineProvider);
begin
  FAnnotationCompletion.AnnotationComplete(TAnnotationLineProvider.Create(AnnotationLineProvider));
end;

constructor TAnnotationCompletion.Create(const AAnnotationCompletion:
  IOTAAnnotationCompletion);
begin
  inherited Create;
  FAnnotationCompletion := AAnnotationCompletion;
end;

{ TAnnotationLineProvider }

constructor TAnnotationLineProvider.Create(
  const AnnotationLineProvider: IAnnotationLineProvider);
begin
  inherited Create;
  FAnnotationLineProvider := AnnotationLineProvider;
end;

function TAnnotationLineProvider.GetCount: Integer;
begin
  Result := FAnnotationLineProvider.Count;
end;

function TAnnotationLineProvider.GetGutterInfo(Index: Integer): string;
begin
  Result := FAnnotationLineProvider.GutterInfo[Index];
end;

function TAnnotationLineProvider.GetHintStr(Index: Integer): string;
begin
  Result := FAnnotationLineProvider.HintStr[Index]
end;

function TAnnotationLineProvider.GetIntensity(Index: Integer): Integer;
begin
  Result := FAnnotationLineProvider.Intensity[Index];
end;

function TAnnotationLineProvider.GetMaxGutterWidth: Integer;
begin
  Result := FAnnotationLineProvider.MaxGutterWidth;
end;

end.
