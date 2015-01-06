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
{ The Original Code is VerInsLiveBlameTypes.pas.                               }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsLiveBlameTypes;

interface

uses
  Classes, Graphics, Generics.Collections;

type
  TJVCSLineHistoryRevision = class(TPersistent)
  private
    FDate: TDateTime;
    FDateStr: string;
    FListIndex: Integer;
    FRevisionStr: string;
    FOrgUserStr: string;
    FUserStr: string;
    FComment: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Date: TDateTime read FDate write FDate;
    property DateStr: string read FDateStr write FDateStr;
    property ListIndex: Integer read FListIndex write FListIndex;
    property OrgUserStr: string read FOrgUserStr write FOrgUserStr;
    property RevisionStr: string read FRevisionStr write FRevisionStr;
    property UserStr: string read FUserStr write FUserStr;
    property Comment: string read FComment write FComment;
  end;

  TRevisionColor = class(TPersistent)
  private
    FDateColor: TColor;
    FRevisionColor: TColor;
    FLineHistoryRevision: TJVCSLineHistoryRevision;
    FUserColor: TColor;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
    property DateColor: TColor read FDateColor write FDateColor;
    property RevisionColor: TColor read FRevisionColor write FRevisionColor;
    property LineHistoryRevision: TJVCSLineHistoryRevision read FLineHistoryRevision;
    property UserColor: TColor read FUserColor write FUserColor;
  end;

  TJVCSLineHistorySummary = class;

  TJVCSLineHistoryRevisionSummary = class(TPersistent)
  private
    FLineHistoryRevision: TJVCSLineHistoryRevision;
    FLineCount: Integer;
    FParent: TJVCSLineHistorySummary;
    FPercent: Double;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalcPercent;
    procedure IncLineCount;
  public
    constructor Create(AParent: TJVCSLineHistorySummary; ALineHistoryRevision: TJVCSLineHistoryRevision);
    property LineHistoryRevision: TJVCSLineHistoryRevision read FLineHistoryRevision;
    property LineCount: Integer read FLineCount;
    property Percent: Double read FPercent;
  end;

  TJVCSLineHistoryUserSummary = class(TPersistent)
  private
    FUserName: string;
    FLineCount: Integer;
    FParent: TJVCSLineHistorySummary;
    FPercent: Double;
    FVisibleUserName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalcPercent;
    procedure IncLineCount;
  public
    constructor Create(AParent: TJVCSLineHistorySummary; const AUserName: string);
    property UserName: string read FUserName;
    property LineCount: Integer read FLineCount;
    property Percent: Double read FPercent;
    property VisibleUserName: string read FVisibleUserName write FVisibleUserName;
  end;

  TJVCSLineHistorySummary = class(TPersistent)
  private
    FLineCount: Integer;
    FRevisionItems: TObjectList<TJVCSLineHistoryRevisionSummary>;
    FRevisionItemsDict: TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevisionSummary>;
    FUserItems: TObjectList<TJVCSLineHistoryUserSummary>;
    FUserItemsDict: TDictionary<string, TJVCSLineHistoryUserSummary>;
    FLastRevision: TJVCSLineHistoryRevision;
    FLastRevisionSummary: TJVCSLineHistoryRevisionSummary;
    FLastUser: string;
    FLastUserSummary: TJVCSLineHistoryUserSummary;
    procedure Clear;
    function GetRevisionCount: Integer;
    function GetRevisionSummary(AIndex: Integer): TJVCSLineHistoryRevisionSummary;
    function GetUserCount: Integer;
    function GetUserSummary(AIndex: Integer): TJVCSLineHistoryUserSummary;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(LineRevision: TJVCSLineHistoryRevision);
    procedure AssignMapped(ASummary: TJVCSLineHistorySummary; ARevisionMapping: TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevision>);
    procedure CalcPercent;
    function FindRevisionSummary(ALineHistoryRevision: TJVCSLineHistoryRevision): TJVCSLineHistoryRevisionSummary;
    function FindUserSummary(const AUserName, AVisibleUserName: string): TJVCSLineHistoryUserSummary;

    property LineCount: Integer read FLineCount;
    property RevisionCount: Integer read GetRevisionCount;
    property RevisionSummary[AIndex: Integer]: TJVCSLineHistoryRevisionSummary read GetRevisionSummary;
    property UserCount: Integer read GetUserCount;
    property UserSummary[AIndex: Integer]: TJVCSLineHistoryUserSummary read GetUserSummary;
  end;

  TLiveBlameInformation = class(TPersistent)
  private
    FLatestMethodRevision: TJVCSLineHistoryRevision;
    FLatestRevision: TJVCSLineHistoryRevision;
    FLineNo: Integer;
    FLineRevision: TJVCSLineHistoryRevision;
    FMethodName: string;
    FMethodSummary: TJVCSLineHistorySummary;
    FRevisionColorList: TObjectList<TRevisionColor>;
    FRevisionMapping: TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevision>;
    FRevisions: TObjectList<TJVCSLineHistoryRevision>;
    FSummary: TJVCSLineHistorySummary;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRevisionColorsMapped(ARevisionColorList: TList<TRevisionColor>);
    procedure AddRevisions(ARevisions: TList<TJVCSLineHistoryRevision>);
    procedure AssignMethodSummaryMapped(ASummary: TJVCSLineHistorySummary);
    procedure AssignSummaryMapped(ASummary: TJVCSLineHistorySummary);
    procedure Clear;
    function GetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
    procedure SetLatestMethodRevisionMapped(AValue: TJVCSLineHistoryRevision);
    procedure SetLatestRevisionMapped(AValue: TJVCSLineHistoryRevision);
    procedure SetLineRevisionMapped(AValue: TJVCSLineHistoryRevision);
    property LatestMethodRevision: TJVCSLineHistoryRevision read FLatestMethodRevision write FLatestMethodRevision;
    property LatestRevision: TJVCSLineHistoryRevision read FLatestRevision write FLatestRevision;
    property LineMethodName: string read FMethodName write FMethodName;
    property LineNo: Integer read FLineNo write FLineNo;
    property LineRevision: TJVCSLineHistoryRevision read FLineRevision write FLineRevision;
    property MethodSummary: TJVCSLineHistorySummary read FMethodSummary;
    property RevisionColorList: TObjectList<TRevisionColor> read FRevisionColorList;
    property Revisions: TObjectList<TJVCSLineHistoryRevision> read FRevisions;
    property Summary: TJVCSLineHistorySummary read FSummary;
  end;

implementation

{ TJVCSLineHistoryRevision }

procedure TJVCSLineHistoryRevision.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryRevision then
  begin
    TJVCSLineHistoryRevision(Dest).FDate := FDate;
    TJVCSLineHistoryRevision(Dest).FDateStr := FDateStr;
    TJVCSLineHistoryRevision(Dest).FListIndex := FListIndex;
    TJVCSLineHistoryRevision(Dest).FRevisionStr := FRevisionStr;
    TJVCSLineHistoryRevision(Dest).FOrgUserStr := FOrgUserStr;
    TJVCSLineHistoryRevision(Dest).FUserStr := FUserStr;
    TJVCSLineHistoryRevision(Dest).FComment := FComment;
  end
  else
    inherited AssignTo(Dest);
end;

{ TRevisionColor }

constructor TRevisionColor.Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
begin
  inherited Create;
  FDateColor := clNone;
  FRevisionColor := clNone;
  FLineHistoryRevision := ALineHistoryRevision;
  FUserColor := clNone;
end;

procedure TRevisionColor.AssignTo(Dest: TPersistent);
begin
  if Dest is TRevisionColor then
  begin
    TRevisionColor(Dest).FDateColor := FDateColor;
    TRevisionColor(Dest).FRevisionColor := FRevisionColor;
    TRevisionColor(Dest).FLineHistoryRevision := FLineHistoryRevision;
    TRevisionColor(Dest).FUserColor := FUserColor;
  end
  else
    inherited AssignTo(Dest);
end;

{ TJVCSLineHistoryRevisionSummary }

constructor TJVCSLineHistoryRevisionSummary.Create(AParent: TJVCSLineHistorySummary; ALineHistoryRevision: TJVCSLineHistoryRevision);
begin
  inherited Create;
  FLineHistoryRevision := ALineHistoryRevision;
  FLineCount := 0;
  FParent := AParent;
end;

procedure TJVCSLineHistoryRevisionSummary.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryRevisionSummary then
  begin
    TJVCSLineHistoryRevisionSummary(Dest).FLineHistoryRevision := FLineHistoryRevision;
    TJVCSLineHistoryRevisionSummary(Dest).FLineCount := FLineCount;
    TJVCSLineHistoryRevisionSummary(Dest).FPercent := FPercent;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJVCSLineHistoryRevisionSummary.CalcPercent;
begin
  if Assigned(FParent) then
  begin
    if FParent.LineCount = 0 then
      FPercent := 0
    else
      FPercent := (FLineCount / FParent.LineCount) * 100;
  end;
end;

procedure TJVCSLineHistoryRevisionSummary.IncLineCount;
begin
  Inc(FLineCount);
  CalcPercent;
end;

{ TJVCSLineHistoryUserSummary }

procedure TJVCSLineHistoryUserSummary.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryUserSummary then
  begin
    TJVCSLineHistoryUserSummary(Dest).FUserName := FUserName;
    TJVCSLineHistoryUserSummary(Dest).FLineCount := FLineCount;
    TJVCSLineHistoryUserSummary(Dest).FPercent := FPercent;
    TJVCSLineHistoryUserSummary(Dest).FVisibleUserName := FVisibleUserName;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TJVCSLineHistoryUserSummary.Create(AParent: TJVCSLineHistorySummary; const AUserName: string);
begin
  inherited Create;
  FUserName := AUserName;
  FLineCount := 0;
  FPercent := 0;
  FParent := AParent;
  FVisibleUserName := AUserName;
end;

procedure TJVCSLineHistoryUserSummary.CalcPercent;
begin
  if Assigned(FParent) then
  begin
    if FParent.LineCount = 0 then
      FPercent := 0
    else
      FPercent := (FLineCount / FParent.LineCount) * 100;
  end;
end;

procedure TJVCSLineHistoryUserSummary.IncLineCount;
begin
  Inc(FLineCount);
  CalcPercent;
end;

{ TJVCSLineHistorySummary }

constructor TJVCSLineHistorySummary.Create;
begin
  inherited Create;
  FLineCount := 0;
  FRevisionItems := TObjectList<TJVCSLineHistoryRevisionSummary>.Create;
  FUserItems := TObjectList<TJVCSLineHistoryUserSummary>.Create;
  FRevisionItemsDict := TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevisionSummary>.Create;
  FUserItemsDict := TDictionary<string, TJVCSLineHistoryUserSummary>.Create;
end;

destructor TJVCSLineHistorySummary.Destroy;
begin
  FRevisionItems.Free;
  FUserItems.Free;
  FRevisionItemsDict.Free;
  FUserItemsDict.Free;
  inherited Destroy;
end;

procedure TJVCSLineHistorySummary.Add(LineRevision: TJVCSLineHistoryRevision);
var
  RevisionSummary: TJVCSLineHistoryRevisionSummary;
  UserSummary: TJVCSLineHistoryUserSummary;
begin
  if Assigned(LineRevision) then
  begin
    Inc(FLineCount);
    RevisionSummary := FindRevisionSummary(LineRevision);
    RevisionSummary.IncLineCount;
    UserSummary := FindUserSummary(LineRevision.OrgUserStr, LineRevision.UserStr);
    UserSummary.IncLineCount;
  end;
end;

procedure TJVCSLineHistorySummary.AssignMapped(ASummary: TJVCSLineHistorySummary;
  ARevisionMapping: TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevision>);
var
  I: Integer;
  MappedRevision: TJVCSLineHistoryRevision;
begin
  Assign(ASummary);
  for I := 0 to FRevisionItems.Count - 1 do
    if ARevisionMapping.TryGetValue(FRevisionItems[I].LineHistoryRevision, MappedRevision) then
      FRevisionItems[I].FLineHistoryRevision := MappedRevision
    else
      asm
        nop
      end;
end;

procedure TJVCSLineHistorySummary.AssignTo(Dest: TPersistent);
var
  I, Idx: Integer;
  RevisionSummary: TJVCSLineHistoryRevisionSummary;
  UserSummary: TJVCSLineHistoryUserSummary;
  DestSummary: TJVCSLineHistorySummary;
begin
  if Dest is TJVCSLineHistorySummary then
  begin
    DestSummary := TJVCSLineHistorySummary(Dest);
    DestSummary.Clear;
    DestSummary.FLineCount := FLineCount;
    for I := 0 to FRevisionItems.Count - 1 do
    begin
      Idx := DestSummary.FRevisionItems.Add(TJVCSLineHistoryRevisionSummary.Create(DestSummary, nil));
      RevisionSummary := DestSummary.FRevisionItems[Idx];
      RevisionSummary.Assign(FRevisionItems[I]);
    end;
    for I := 0 to FUserItems.Count - 1 do
    begin
      Idx := DestSummary.FUserItems.Add(TJVCSLineHistoryUserSummary.Create(DestSummary, ''));
      UserSummary := DestSummary.FUserItems[Idx];
      UserSummary.Assign(FUserItems[I]);
    end;
    DestSummary.CalcPercent;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJVCSLineHistorySummary.CalcPercent;
var
  I: Integer;
begin
  for I := 0 to RevisionCount - 1 do
    RevisionSummary[I].CalcPercent;
  for I := 0 to UserCount - 1 do
    UserSummary[I].CalcPercent;
end;

procedure TJVCSLineHistorySummary.Clear;
begin
  FLineCount := 0;
  FRevisionItems.Clear;
  FUserItems.Clear;
  FRevisionItemsDict.Clear;
  FUserItemsDict.Clear;
  FLastRevision := nil;
  FLastRevisionSummary := nil;
end;

function TJVCSLineHistorySummary.FindRevisionSummary(ALineHistoryRevision: TJVCSLineHistoryRevision): TJVCSLineHistoryRevisionSummary;
var
  I: Integer;
begin
  if FLastRevision = ALineHistoryRevision then
    Result := FLastRevisionSummary
  else
  begin
    if not FRevisionItemsDict.TryGetValue(ALineHistoryRevision, Result) then
    begin
      Result := nil;
      for I := 0 to RevisionCount - 1 do
        if RevisionSummary[I].LineHistoryRevision = ALineHistoryRevision then
        begin
          Result := RevisionSummary[I];
          Break;
        end;
      if not Assigned(Result) then
      begin
        FRevisionItems.Add(TJVCSLineHistoryRevisionSummary.Create(Self, ALineHistoryRevision));
        Result := TJVCSLineHistoryRevisionSummary(FRevisionItems.Last);
        FRevisionItemsDict.Add(ALineHistoryRevision, Result);
      end;
    end;
    FLastRevision := ALineHistoryRevision;
    FLastRevisionSummary := Result;
  end;
end;

function TJVCSLineHistorySummary.FindUserSummary(const AUserName, AVisibleUserName: string): TJVCSLineHistoryUserSummary;
var
  I: Integer;
begin
  if FLastUser = AUserName then
    Result := FLastUserSummary
  else
  begin
    if not FUserItemsDict.TryGetValue(AUserName, Result) then
    begin
      Result := nil;
      for I := 0 to UserCount - 1 do
        if UserSummary[I].UserName = AUserName then
        begin
          Result := UserSummary[I];
          Break;
        end;
      if not Assigned(Result) then
      begin
        FUserItems.Add(TJVCSLineHistoryUserSummary.Create(Self, AUserName));
        Result := TJVCSLineHistoryUserSummary(FUserItems.Last);
        Result.VisibleUserName := AVisibleUserName;
        FUserItemsDict.Add(AUserName, Result);
      end;
    end;
    FLastUser := AUserName;
    FLastUserSummary := Result;
  end;
end;

function TJVCSLineHistorySummary.GetRevisionCount: Integer;
begin
  Result := FRevisionItems.Count;
end;

function TJVCSLineHistorySummary.GetRevisionSummary(AIndex: Integer): TJVCSLineHistoryRevisionSummary;
begin
  Result := TJVCSLineHistoryRevisionSummary(FRevisionItems[AIndex]);
end;

function TJVCSLineHistorySummary.GetUserCount: Integer;
begin
  Result := FUserItems.Count;
end;

function TJVCSLineHistorySummary.GetUserSummary(AIndex: Integer): TJVCSLineHistoryUserSummary;
begin
  Result := TJVCSLineHistoryUserSummary(FUserItems[AIndex]);
end;

{ TLiveBlameInformation }

constructor TLiveBlameInformation.Create;
begin
  inherited Create;
  FMethodSummary := TJVCSLineHistorySummary.Create;
  FRevisionColorList := TObjectList<TRevisionColor>.Create;
  FRevisionMapping := TDictionary<TJVCSLineHistoryRevision, TJVCSLineHistoryRevision>.Create;
  FRevisions := TObjectList<TJVCSLineHistoryRevision>.Create;
  FSummary := TJVCSLineHistorySummary.Create;
end;

destructor TLiveBlameInformation.Destroy;
begin
  FSummary.Free;
  FRevisions.Free;
  FRevisionMapping.Free;
  FRevisionColorList.Free;
  FMethodSummary.Free;
  inherited Destroy;
end;

function TLiveBlameInformation.GetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FRevisionColorList.Count - 1 do
    if FRevisionColorList[I].LineHistoryRevision = ALineHistoryRevision then
    begin
      Result := FRevisionColorList[I];
      Break;
    end;
end;

procedure TLiveBlameInformation.Clear;
begin
  FLatestMethodRevision := nil;
  FLatestRevision := nil;
  FLineNo := 0;
  FLineRevision := nil;
  FMethodName := '';
  FMethodSummary.Clear;
  FRevisionColorList.Clear;
  FRevisionMapping.Clear;
  FRevisions.Clear;
  FSummary.Clear;
end;

procedure TLiveBlameInformation.AddRevisionColorsMapped(ARevisionColorList: TList<TRevisionColor>);
var
  I, Idx: Integer;
  NewRevision: TJVCSLineHistoryRevision;
begin
  FRevisionColorList.Clear;
  for I := 0 to ARevisionColorList.Count - 1 do
  begin
    Idx := FRevisionColorList.Add(TRevisionColor.Create(nil));
    FRevisionColorList[Idx].Assign(ARevisionColorList[I]);
    if FRevisionMapping.TryGetValue(FRevisionColorList[Idx].LineHistoryRevision, NewRevision) then
      FRevisionColorList[Idx].FLineHistoryRevision := NewRevision
    else
      FRevisionColorList[Idx].FLineHistoryRevision := nil;
  end;
end;

procedure TLiveBlameInformation.AddRevisions(ARevisions: TList<TJVCSLineHistoryRevision>);
var
  I: Integer;
  NewRevision: TJVCSLineHistoryRevision;
begin
  FRevisionMapping.Clear;
  FRevisions.Clear;
  for I := 0 to ARevisions.Count - 1 do
  begin
    NewRevision := FRevisions[FRevisions.Add(TJVCSLineHistoryRevision.Create)];
    NewRevision.Assign(ARevisions[I]);
    FRevisionMapping.Add(ARevisions[I], NewRevision);
  end;
end;

procedure TLiveBlameInformation.AssignMethodSummaryMapped(ASummary: TJVCSLineHistorySummary);
begin
  FMethodSummary.AssignMapped(ASummary, FRevisionMapping);
end;

procedure TLiveBlameInformation.AssignSummaryMapped(ASummary: TJVCSLineHistorySummary);
begin
  FSummary.AssignMapped(ASummary, FRevisionMapping);
end;

procedure TLiveBlameInformation.AssignTo(Dest: TPersistent);
begin
  if Dest is TLiveBlameInformation then
  begin
    TLiveBlameInformation(Dest).Clear;
    TLiveBlameInformation(Dest).AddRevisions(FRevisions);
    TLiveBlameInformation(Dest).AddRevisionColorsMapped(FRevisionColorList);
    TLiveBlameInformation(Dest).FMethodName := FMethodName;
    TLiveBlameInformation(Dest).AssignMethodSummaryMapped(FMethodSummary);
    TLiveBlameInformation(Dest).AssignSummaryMapped(FSummary);
    TLiveBlameInformation(Dest).SetLatestMethodRevisionMapped(FLatestMethodRevision);
    TLiveBlameInformation(Dest).SetLatestRevisionMapped(FLatestRevision);
    TLiveBlameInformation(Dest).SetLineRevisionMapped(FLineRevision);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TLiveBlameInformation.SetLatestMethodRevisionMapped(AValue: TJVCSLineHistoryRevision);
var
  NewRevision: TJVCSLineHistoryRevision;
begin
  FLatestMethodRevision := AValue;
  if Assigned(FLatestMethodRevision) then
  begin
    if FRevisionMapping.TryGetValue(FLatestMethodRevision, NewRevision) then
      FLatestMethodRevision := NewRevision
    else
      FLatestMethodRevision := nil;
  end;
end;

procedure TLiveBlameInformation.SetLatestRevisionMapped(AValue: TJVCSLineHistoryRevision);
var
  NewRevision: TJVCSLineHistoryRevision;
begin
  FLatestRevision := AValue;
  if Assigned(FLatestRevision) then
  begin
    if FRevisionMapping.TryGetValue(FLatestRevision, NewRevision) then
      FLatestRevision := NewRevision
    else
      FLatestRevision := nil;
  end;
end;

procedure TLiveBlameInformation.SetLineRevisionMapped(AValue: TJVCSLineHistoryRevision);
var
  NewRevision: TJVCSLineHistoryRevision;
begin
  FLineRevision := AValue;
  if Assigned(FLineRevision) then
  begin
    if FRevisionMapping.TryGetValue(FLineRevision, NewRevision) then
      FLineRevision := NewRevision
    else
      FLineRevision := nil;
  end;
end;

end.
