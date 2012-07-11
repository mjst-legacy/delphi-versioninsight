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
{ The Original Code is VerInsLiveBlame.pas.                                    }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2006 - 2011 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsLiveBlame;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Generics.Collections, Graphics,
  Controls, Buttons, ExtCtrls, ComCtrls, Forms, ActnList, Menus, Themes,
  ActiveX, TypInfo, ToolsAPI, DockForm, FileHistoryAPI,
  DiffUnit, HashUnit;

type
  TLiveBlameWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTAEditorNotifier)
  private
    NotifierIndex: Integer;
    NotifierIndex2: Integer;
    FPanelList: TList;
  public
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;

    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    { IOTAEditorNotifier }
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    constructor Create;
    destructor Destroy; override;

    procedure RemovePanel(AObject: TObject);
  end;

procedure Register;
function RegisterExpert: Integer;

implementation

uses
  {$IFDEF SVNINTERNAL}
  SvnIDEClient, SvnClient, SvnIDETypes,
  {$ENDIF SVNINTERNAL}
  VerInsIDETypes, VerInsIDEBlameAddInOptions, VerInsBlameSettings, Registry;

procedure Register;
begin
  RegisterPackageWizard(TLiveBlameWizard.Create);
  SetPresetsKey((BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight\Blame');
  RegisterAddInOptions;
end;

function RegisterExpert: Integer;
begin
  Result := (BorlandIDEServices as IOTAWizardServices).AddWizard(TLiveBlameWizard.Create);
  SetPresetsKey((BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\VersionInsight\Blame');
  RegisterAddInOptions;
end;

{$IFNDEF DEBUG}
//"disables" OutputDebugString in non debug mode
procedure OutputDebugString(const AMsg: string);
begin
end;
{$ENDIF ~DEBUG}

function UTCToTzDateTime(Value: TDateTime): TDateTime;
var
  TZ: TTimeZoneInformation;
begin
  Result := Value;
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result - (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result - (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result - TZ.Bias / MinsPerDay;
  end;
end;

type
  TLiveBlamePaintBox = class(TCustomControl)
  private
    FOnPaint: TNotifyEvent;
    FOnHintMessage: TWndMethod;
  protected
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InvalidateControl;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnHintMessage: TWndMethod read FOnHintMessage write FOnHintMessage;
  end;

procedure TLiveBlamePaintBox.CMHintShow(var Msg: TMessage);
begin
  if Assigned(FOnHintMessage) then
    FOnHintMessage(Msg)
  else
    inherited;
end;

constructor TLiveBlamePaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
end;

procedure TLiveBlamePaintBox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TLiveBlamePaintBox.InvalidateControl;
begin
  InvalidateRect(Handle, nil, False);
end;

procedure TLiveBlamePaintBox.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self)
end;

const
  WM_BLAME_SHOW = WM_USER + 1;
  WM_BLAME_UPDATE = WM_USER + 2;

type
  TJVCSLineHistoryRevision = class(TObject)
  private
    FDate: TDateTime;
    FDateStr: string;
    FRevisionStr: string;
    FOrgUserStr: string;
    FUserStr: string;
    FComment: string;
  public
    property Date: TDateTime read FDate;
    property DateStr: string read FDateStr;
    property OrgUserStr: string read FOrgUserStr;
    property RevisionStr: string read FRevisionStr;
    property UserStr: string read FUserStr;
    property Comment: string read FComment;
  end;

  TRevisionColor = class(TObject)
  private
    FDateColor: TColor;
    FRevisionColor: TColor;
    FLineHistoryRevision: TJVCSLineHistoryRevision;
    FUserColor: TColor;
  public
    constructor Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
    property DateColor: TColor read FDateColor write FDateColor;
    property RevisionColor: TColor read FRevisionColor write FRevisionColor;
    property LineHistoryRevision: TJVCSLineHistoryRevision read FLineHistoryRevision;
    property UserColor: TColor read FUserColor write FUserColor;
  end;

  TRevisionRectangle = class(TObject)
  private
    FRect: TRect;
    FRevisionIDStr: string;
  public
    constructor Create(ARect: TRect; ARevisionIDStr: string);
    property Rect: TRect read FRect;
    property RevisionIDStr: string read FRevisionIDStr;
  end;

  TRevisionRectangleList = class(TObject)
  private
    FItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ARect: TRect; ARevisionIDStr: string);
    procedure Clear(AY: Integer = -1);
    function Find(AX, AY: Integer; var ATopY: Integer; var ARevisionIDStr: string): Boolean;
    function FindRect(AX, AY: Integer): TRevisionRectangle;
  end;

  TDeletedLines = class(TObject)
  private
    FItems: TObjectList<TStringList>;
    FVer: Integer;
    function GetHint: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Hint: string read GetHint;
    property Items: TObjectList<TStringList> read FItems;
    property Ver: Integer read FVer write FVer;
  end;

  TDeletedLinesTriangle = class(TObject)
  private
    FDeletedLines: TDeletedLines;
    FPoint: TPoint;
  public
    constructor Create(APoint: TPoint; ADeletedLines: TDeletedLines);
    property DeletedLines: TDeletedLines read FDeletedLines;
    property Point: TPoint read FPoint;
  end;

  TDeletedLinesTriangleList = class(TObjectList<TDeletedLinesTriangle>)
  public
    procedure Add(APoint: TPoint; ADeletedLines: TDeletedLines);
    function Find(AX, AY: Integer): Integer;
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

{ TRevisionRectangle }

constructor TRevisionRectangle.Create(ARect: TRect; ARevisionIDStr: string);
begin
  FRect := ARect;
  FRevisionIDStr := ARevisionIDStr;
end;

{ TRevisionRectangleList }

constructor TRevisionRectangleList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TRevisionRectangleList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TRevisionRectangleList.Add(ARect: TRect; ARevisionIDStr: string);
begin
  FItems.Add(TRevisionRectangle.Create(ARect, ARevisionIDStr));
end;

procedure TRevisionRectangleList.Clear(AY: Integer = -1);
var
  I: Integer;
begin
  if AY = -1 then
    FItems.Clear
  else
  for I := FItems.Count - 1 downto 0 do
    if TRevisionRectangle(FItems[I]).Rect.Top <= AY then
      FItems.Delete(I);
end;

function TRevisionRectangleList.Find(AX, AY: Integer; var ATopY: Integer; var ARevisionIDStr: string): Boolean;
var
  I: Integer;
  Rect: TRect;
begin
  Result := False;
  ATopY := -1;
  ARevisionIDStr := '';
  for I := 0 to FItems.Count - 1 do
  begin
    Rect := TRevisionRectangle(FItems[I]).Rect;
    if (Rect.Left <= AX) and (Rect.Right >= AX) and (Rect.Top <= AY) and (Rect.Bottom >= AY) then
    begin
      ATopY := Rect.Top;
      ARevisionIDStr := TRevisionRectangle(FItems[I]).RevisionIDStr;
      Result := True;
      Break;
    end;
  end;
end;

function TRevisionRectangleList.FindRect(AX, AY: Integer): TRevisionRectangle;
var
  I: Integer;
  Rect: TRect;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    Rect := TRevisionRectangle(FItems[I]).Rect;
    if (Rect.Left <= AX) and (Rect.Right >= AX) and (Rect.Top <= AY) and (Rect.Bottom >= AY) then
    begin
      Result := TRevisionRectangle(FItems[I]);
      Break;
    end;
  end;
end;

{ TDeletedLines }

constructor TDeletedLines.Create;
begin
  inherited Create;
  FItems := TObjectList<TStringList>.Create;
end;

destructor TDeletedLines.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TDeletedLines.GetHint: string;
var
  I: Integer;
begin
  Result := Format('%d lines deleted', [Items[0].Count]);
  for I := 0 to Items[0].Count - 1 do
    Result := Result + #13#10 + Items[0][I];
end;

{ TDeletedLinesTriangle }

constructor TDeletedLinesTriangle.Create(APoint: TPoint; ADeletedLines: TDeletedLines);
begin
  inherited Create;
  FPoint := APoint;
  FDeletedLines := ADeletedLines;
end;

{ TDeletedLinesTriangleList }

procedure TDeletedLinesTriangleList.Add(APoint: TPoint; ADeletedLines: TDeletedLines);
begin
  inherited Add(TDeletedLinesTriangle.Create(APoint, ADeletedLines));
end;

//http://delphi.about.com/cs/adptips2001/a/bltip0601_5.htm
function PtInPoly(const Points: Array of TPoint; X,Y: Integer): Boolean;
var
  Count, K, J: Integer;
begin
  Result := False;
  Count := Length(Points) ;
  J := Count-1;
  for K := 0 to Count-1 do
  begin
   if ((Points[K].Y <=Y) and (Y < Points[J].Y)) or
      ((Points[J].Y <=Y) and (Y < Points[K].Y)) then
   begin
    if (x < (Points[j].X - Points[K].X) *
       (y - Points[K].Y) /
       (Points[j].Y - Points[K].Y) + Points[K].X) then
        Result := not Result;
   end;
    J := K;
  end;
end;

function TDeletedLinesTriangleList.Find(AX, AY: Integer): Integer;
var
  I, XS, YS: Integer;
  PolyPoints: array of TPoint;
begin
  Result := -1;
  SetLength(PolyPoints, 3);
  for I := 0 to Count - 1 do
  begin
    XS := Items[I].Point.X;
    YS := Items[I].Point.Y;
    PolyPoints[0] := Point(XS, YS);
    PolyPoints[1] := Point(XS + 8, YS + 4);
    PolyPoints[2] := Point(XS, YS + 8);
    if PtInPoly(PolyPoints, AX, AY) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

type
  TRevisionClickEvent = procedure(ASender: TObject; ARevisionIDStr: string) of object;

  PBlameHintData = ^TBlameHintData;
  TBlameHintData = record
    Revision: TRevisionColor;
    DeletedLines: TDeletedLines;
    Rect: TRect;
  end;

  TBlameHintWindow = class(THintWindow)
  private
    procedure PaintDeleteHint(ATargetCanvas: TCanvas; ADeleteLines: TDeletedLines; ARect: TRect);
    procedure PaintHint(ATargetCanvas: TCanvas; ARevision: TRevisionColor;
      ARect: TRect);
  protected
    FHintData: TBlameHintData;
    procedure WMKillFocus(var Msg: TMessage); message WM_ACTIVATE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: string; AData: Pointer): TRect; override;
    function IsHintMsg(var Msg: TMsg): Boolean; override;
  end;

  TCustomLiveBlameData = class(TComponent)
  private
    FFileName: string;
    FBlameInfoReady: Boolean;
    FBlameInfoAvailable: Boolean;
    FLastAge: TDateTime;
    FLastStreamSize: Integer;
    FRevisions: TObjectList<TJVCSLineHistoryRevision>;
    FLines: TList<TJVCSLineHistoryRevision>;
    FOrgLines: TList<TJVCSLineHistoryRevision>;
    FBufferRevision: TJVCSLineHistoryRevision;
    FFileRevision: TJVCSLineHistoryRevision;
    FRevisionColorList: TObjectList<TRevisionColor>;
    FDeletedLines: TObjectList<TDeletedLines>;
    FButtonDown: Boolean;
    FBlameCounter: Integer;
    FBlameRevision: Integer;
    FMaxRevision: Integer;
    FStage: Integer;
    FLoading: Boolean;
    FPaintBox: TLiveBlamePaintBox;
    FFirstRevisionIDStr: string;
    FLatestRevisionContent: RawByteString;
  public
    constructor Create(const AFileName: string); reintroduce;
    destructor Destroy; override;
    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); virtual; abstract;
    procedure Load; virtual; abstract;
    procedure UpdateSettings(ASettings: TJVCSLineHistorySettings);
  end;

  {$IFDEF SVNINTERNAL}
  TSvnLiveBlameData = class(TCustomLiveBlameData, IAsyncUpdate, IAnnotationCompletion)
  private
    FSvnItem: TSvnItem;
    procedure HandleBlameLoad(Sender: TObject);
  public
    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); override;
    procedure Load; override;

    { IAsyncUpdate}
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;

    { IAnnotationCompletion }
    procedure AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
  end;
  {$ENDIF SVNINTERNAL}

  TGenericLiveBlameData = class(TCustomLiveBlameData, IOTAAsynchronousHistoryUpdater, IOTAAnnotationCompletion)
  private
    FFileHistory: IOTAFileHistory;
    FAnnotationLineProvider: IOTAAnnotationLineProvider;
    FAnnotationLineProviderHelper: TList<string>;
    FAnnotationLineProviderHelperUpdateRequired: Boolean;
    FUpdateCheck: Boolean;
    FUpdateCheckTopRevisionID: string;
    FUpdateCheckRevisionCount: Integer;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    { IOTAAsynchronousHistoryUpdater }
    procedure Completed;
    function UpdateHistoryItems(FileHistory: IOTAFileHistory; FirstNewIndex, LastNewIndex: Integer): Boolean;
    { IOTAAnnotationCompletion }
    procedure AnnotationComplete(const AnnotationLineProvider: IOTAAnnotationLineProvider);

    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); override;
    procedure Load; override;
  end;

  TLiveBlameData = TGenericLiveBlameData;

  TOnGetLineColorEvent = function(ALine: Integer; AColorIndex: Integer): TColor of object;

  TLiveBlameEditorPanel = class(TPanel)
  private
    FTimer: TTimer;
    FCheckShowTimer: TTimer;
    FPaintBox: TLiveBlamePaintBox;
    FCnt: Integer;
    FCY: Integer;
    FCursorLine: Integer;
    FTopLine: Integer;
    FSpeedButton: TSpeedButton;
    FWizard: TLiveBlameWizard;
    FActiveEditView1: IOTAEditView;
    FModule: IOTAModule;
    FLiveBlameData: TLiveBlameData;
    FLiveBlameDataList: TObjectList<TLiveBlameData>;
    FLineX: Integer;
    FRevisionX: Integer;
    FRevisionRectX1: Integer;
    FRevisionRectX2: Integer;
    FDateX: Integer;
    FDateRectX1: Integer;
    FDateRectX2: Integer;
    FUserX: Integer;
    FUserRectX1: Integer;
    FUserRectX2: Integer;
    FSettings: TJVCSLineHistorySettings;
    FColorList: TStringList;
    FInstalledHook: Boolean;
    FPainting: Boolean;
    FRevisionRectangles: TRevisionRectangleList;
    FRevisionHintRectangles: TRevisionRectangleList;
    FDeletedLinesHintTriangles: TDeletedLinesTriangleList;
    FHighlightY: Integer;
    FLastHighlightY: Integer;
    FLastHighlightedRevisionIDStr: string;
    FOnRevisionClick: TRevisionClickEvent;

    FProgressBar: TProgressBar;
    FHintData: TBlameHintData;
    FPopupMenu: TPopupMenu;
    FMenuItem1: TMenuItem;
    FMenuItem2: TMenuItem;
    FMenuItem3: TMenuItem;
    FConfigMenuItem: TMenuItem;
    FLastPresetTimeStamp: TDateTime;
    FModificationColorFile: TColor;
    FModificationColorBuffer: TColor;
    procedure CheckInstallHook;
    procedure UnInstallHooks;
    procedure CreatePopupMenu;
    procedure BuildLineHistory;
    procedure UpdateLineHistory(ASourceEditor: IOTASourceEditor);
    procedure HandleDiffThreadReady(Sender: TObject);
    function DoGetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
    function GetLineColor(ALine: Integer; AColorIndex: Integer): TColor;
    function GetNextColor: TColor;
    procedure UpdateGutterWidth;
    procedure OnTimer(Sender: TObject);
    procedure OnCheckTimer(Sender: TObject);
    procedure PaintColorBar(ACanvas: TCanvas; ARect: TRect; ALinesCount,
      APenColor, ABackGroundColor: TColor; AOnGetLineColor: TOnGetLineColorEvent; AColorIndex: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure WMMyShow(var Msg: TMessage); message WM_BLAME_SHOW;
    procedure WMUpdateWnd(var Msg: TMessage); message WM_BLAME_UPDATE;
    function GetEditControl: TObject;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleOnClick(Sender: TObject);
    procedure HandleRevisionClick(ASender: TObject; ARevisionIDStr: string);
    procedure HandleMouseLeave(Sender: TObject);
    procedure HandleHintShow(var Msg: TMessage);
    function GetHintTextAndSize(AX, AY: Integer; var ARevision: TRevisionColor; var ABlockRect, AHintRect: TRect; var ADeletedLines: TDeletedLines): Boolean;
    procedure CMHintShow(var Msg: TMessage);
    function GetDeleteHintRect(ADeletedLines: TDeletedLines; var ARect: TRect): Boolean;
    function GetHintRect(ARevision: TRevisionColor; var ARect: TRect): Boolean;
    procedure HandlePopupMenu(Sender: TObject);
    procedure HandlePopupMenuPopup(Sender: TObject);
    procedure SetPreset(APresetID: Integer);
    function UpdateModificationColors: Boolean;
  protected
    procedure SetEnabled(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditControlVisible: Boolean;
    procedure ShowIfEditControl;
    procedure EnableCheckTimer;
    procedure ShowHidePanel(Sender: TObject);

    property Wizard: TLiveBlameWizard read FWizard write FWizard;
    property ActiveEditView1: IOTAEditView read FActiveEditView1 write FActiveEditView1;
    property ActiveModule: IOTAModule read FModule write FModule;
  end;

var
  CurrentPanel: TLiveBlameEditorPanel = nil;
  CurrentWindow: HWND = 0;
  CallWndProcHook: HHOOK;
  CallWndProcRetHook: HHOOK;
  GetMsgHook: HHOOK;
  EditControlHook: TControl = nil;
  ForceUpdate: Boolean = False;

constructor TBlameHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TBlameHintWindow.WMKillFocus(var Msg: TMessage);
begin
  Hide;
end;

procedure TBlameHintWindow.Paint;
begin
  if Assigned(FHintData.Revision) and (FHintData.Rect.Right - FHintData.Rect.Left > 0) and
    (FHintData.Rect.Bottom - FHintData.Rect.Top > 0) then
    PaintHint(Canvas, FHintData.Revision, FHintData.Rect)
  else
  if Assigned(FHintData.DeletedLines) and (FHintData.Rect.Right - FHintData.Rect.Left > 0) and
    (FHintData.Rect.Bottom - FHintData.Rect.Top > 0) then
    PaintDeleteHint(Canvas, FHintData.DeletedLines, FHintData.Rect);
end;

procedure TBlameHintWindow.PaintDeleteHint(ATargetCanvas: TCanvas;
  ADeleteLines: TDeletedLines; ARect: TRect);
var
  S: string;
  ClipRect, R: TRect;
begin
  R := ARect;
  if CheckWin32Version(6) and ThemeServices.ThemesEnabled and True then
  begin
    // Paint Vista gradient background if themes enabled
    ClipRect := R;
    ClipRect.Bottom := ClipRect.Bottom + 3;
    InflateRect(R, 4, 4);
    with ThemeServices do
      DrawElement(ATargetCanvas.Handle, GetElementDetails(tttStandardNormal), R, ClipRect);
    R := ClipRect;
  end
  else
  begin
    ATargetCanvas.Brush.Color := clInfoBk;
    ATargetCanvas.Rectangle(R);
  end;
  ATargetCanvas.Font.Name := 'Courier New';
  ATargetCanvas.Font.Size := 10;
  S := ADeleteLines.Hint;
  ATargetCanvas.TextRect(R, S, [tfNoPrefix]);
end;

procedure TBlameHintWindow.PaintHint(ATargetCanvas: TCanvas; ARevision: TRevisionColor; ARect: TRect);
var
  S: string;
  ClipRect, R, RLogMessage: TRect;
begin
  R := ARect;
  if CheckWin32Version(6) and ThemeServices.ThemesEnabled and True then
  begin
    // Paint Vista gradient background if themes enabled
    ClipRect := R;
    ClipRect.Bottom := ClipRect.Bottom + 3;
    InflateRect(R, 4, 4);
    with ThemeServices do
      DrawElement(ATargetCanvas.Handle, GetElementDetails(tttStandardNormal), R, ClipRect);
    R := ClipRect;
  end
  else
  begin
    ATargetCanvas.Brush.Color := clInfoBk;
    ATargetCanvas.Rectangle(R);
  end;
  ATargetCanvas.Font := Screen.HintFont;
  ATargetCanvas.Font.Style := [fsBold];
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(4, 4, 'Revision:');
  ATargetCanvas.TextOut(4, 22, 'Date:');
  ATargetCanvas.TextOut(4, 40, 'User:');
  ATargetCanvas.TextOut(4, 58, 'Logmessage:');
  ATargetCanvas.Font.Style := [];
  ATargetCanvas.Brush.Color := ARevision.RevisionColor;
  ATargetCanvas.Rectangle(Rect(60, 4, 74, 18));
  //ATargetCanvas.Brush.Color := clBtnFace;
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 4, ARevision.LineHistoryRevision.RevisionStr);
  ATargetCanvas.Brush.Color := ARevision.DateColor;
  ATargetCanvas.Rectangle(Rect(60, 22, 74, 36));
  //ATargetCanvas.Brush.Color := clBtnFace;
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 22, ARevision.LineHistoryRevision.DateStr);
  ATargetCanvas.Brush.Color := ARevision.UserColor;
  ATargetCanvas.Rectangle(Rect(60, 40, 74, 54));
  //ATargetCanvas.Brush.Color := clBtnFace;
  S := ARevision.LineHistoryRevision.UserStr;
  if (ARevision.LineHistoryRevision.OrgUserStr <> '') and (ARevision.LineHistoryRevision.OrgUserStr <> S) then
    S := S + ' (' + ARevision.LineHistoryRevision.OrgUserStr + ')';
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 40, S);
  //ATargetCanvas.TextOut(2, 74, FRevision.LineHistoryRevision.LogMessage);
  S :=  ARevision.LineHistoryRevision.Comment;
  {
  R := Rect(2, 74, 200, 200);
  ATargetCanvas.TextRect(R, S, [tfCalcRect]);
  }
  RLogMessage := Rect(4, 76, 300, 300);
  ATargetCanvas.TextRect(RLogMessage, S, [tfCalcRect, tfNoPrefix]);
  ATargetCanvas.TextRect(RLogMessage, S, [tfNoPrefix]);
end;

function TBlameHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
begin
  if not Assigned(AData) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    FHintData := PBlameHintData(AData)^;
    Result := FHintData.Rect;
  end;
end;

function TBlameHintWindow.IsHintMsg(var Msg: TMsg): Boolean;
begin
  Result := inherited IsHintMsg(Msg) and HandleAllocated and IsWindowVisible(Handle);
  // Avoid that mouse moves over the non-client area or key presses cancel the current hint.
  if Result and ((Msg.Message = WM_NCMOUSEMOVE) or ((Msg.Message >= WM_KEYFIRST) and (Msg.Message <= WM_KEYLAST))) then
    Result := False;
end;

function GetAgeStr(ADateTime: TDateTime): string;
var
  DateDiff, LNow: TDateTime;
  y1, m1, y2, m2, dummy: Word;
  ym1, ym2: DWord;
  AgeAmount: Integer;
  SingularAgeStr, PluralAgeStr: string;
begin
  LNow := Now;
  DateDiff := LNow - ADateTime;
  if DateDiff < 0 then
  begin
    AgeAmount := -1;
    SingularAgeStr := 'future';
  end
  else
  if DateDiff < (1 / (24 * 60 * 60)) then
  begin
    AgeAmount := -1;
    SingularAgeStr := 'now';
  end
  else
  if DateDiff < (1 / (24 * 60)) then
  begin
    AgeAmount := Round(DateDiff * 24 * 60 * 60);
    SingularAgeStr := 'second';
    PluralAgeStr := 'seconds';
  end
  else
  if DateDiff < (1 / 24) then
  begin
    AgeAmount := Round(DateDiff * 24 * 60);
    SingularAgeStr := 'minute';
    PluralAgeStr := 'minutes';
  end
  else
  if DateDiff < 1 then
  begin
    AgeAmount := Round(DateDiff * 24);
    SingularAgeStr := 'hour';
    PluralAgeStr := 'hours';
  end
  else
  if DateDiff < 7 then
  begin
    AgeAmount := Round(DateDiff);
    SingularAgeStr := 'day';
    PluralAgeStr := 'days';
  end
  else
  if DateDiff < (7 * 8) then
  begin
    AgeAmount := Round(DateDiff / 7);
    SingularAgeStr := 'week';
    PluralAgeStr := 'weeks';
  end
  else
  begin
    DecodeDate(LNow, y1, m1, dummy);
    ym1 := y1 * 12 + m1;
    DecodeDate(ADateTime, y2, m2, dummy);
    ym2 := y2 * 12 + m2;
    if ym1 - ym2 < 12 then
    begin
      AgeAmount := ym1 - ym2;
      SingularAgeStr := 'month';
      PluralAgeStr := 'months';
    end
    else
    begin
      AgeAmount := (ym1 - ym2) div 12;
      SingularAgeStr := 'year';
      PluralAgeStr := 'years';
    end;
  end;
  if AgeAmount < 0 then
    Result := SingularAgeStr
  else
  if AgeAmount = 1 then
    Result := Format('%d %s', [AgeAmount, SingularAgeStr])
  else
    Result := Format('%d %s', [AgeAmount, PluralAgeStr]);
end;

type
  TGetAgeResultKind = (garkYear, garkMonth, garkWeek, garkDay, garkHour, garkMinute, garkSecond, garkNow, garkFuture);

function GetAge(ABase, ADate: TDateTime; var AResultKind: TGetAgeResultKind; var ARest: TDateTime): Integer;
var
  DateDiff: TDateTime;
  y1, m1, y2, m2, dummy: Word;
  ym1, ym2: DWord;
begin
  DateDiff := ABase - ADate;
  if DateDiff < 0 then
  begin
    Result := -1;
    ARest := 0;
    AResultKind := garkFuture;
  end
  else
  if DateDiff < (1 / (24 * 60 * 60)) then
  begin
    Result := -1;
    ARest := 0;
    AResultKind := garkNow;
  end
  else
  if DateDiff < (1 / (24 * 60)) then
  begin
    Result := Round(DateDiff * 24 * 60 * 60);
    ARest := DateDiff - Result / ( 24 * 60 * 60);
    AResultKind := garkSecond;
  end
  else
  if DateDiff < (1 / 24) then
  begin
    Result := Round(DateDiff * 24 * 60);
    ARest :=  DateDiff - Result / (24 * 60);
    AResultKind := garkMinute;
  end
  else
  if DateDiff < 1 then
  begin
    Result := Round(DateDiff * 24);
    ARest := DateDiff - Result / 24;
    AResultKind := garkHour;
  end
  else
  if DateDiff < 7 then
  begin
    Result := Round(DateDiff);
    ARest := DateDiff - Result;
    AResultKind := garkDay;
  end
  else
  if DateDiff < (7 * 8) then
  begin
    Result := Round(DateDiff / 7);
    ARest := DateDiff - Result * 7;
    AResultKind := garkWeek;
  end
  else
  begin
    DecodeDate(ABase, y1, m1, dummy);
    ym1 := y1 * 12 + m1;
    DecodeDate(ADate, y2, m2, dummy);
    ym2 := y2 * 12 + m2;
    if ym1 - ym2 < 12 then
    begin
      Result := ym1 - ym2;
      ARest := DateDiff - (30.4375 * Result);
      AResultKind := garkMonth;
    end
    else
    begin
      Result := (ym1 - ym2) div 12;
      ARest := DateDiff - (365.25 * Result);
      AResultKind := garkYear;
    end;
  end;
  if ARest < 0 then
    ARest := 0;
end;

function GetAgeResultToStr(AnAgeInt: Integer; AResultKind: TGetAgeResultKind; ALong: Boolean): string;
const
  ShortSingularNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('yr', 'mo', 'wk', 'dy', 'hr', 'mn', 'sc', 'now', 'future');
  ShortPluralNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('yrs', 'mos', 'wks', 'dys', 'hrs', 'min', 'sc', 'now', 'future');
  LongSingularNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('year', 'month', 'week', 'day', 'hour', 'minute', 'second', 'now', 'future');
  LongPluralNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds', 'now', 'future');
var
  S: string;
begin
  if (AnAgeInt < 2) and ALong then
    S := LongSingularNames[AResultKind]
  else
  if ALong then
    S := LongPluralNames[AResultKind]
  else
  if AnAgeInt < 2 then
    S := ShortSingularNames[AResultKind]
  else
    S := ShortPluralNames[AResultKind];
  if AResultKind in [garkNow] then
    Result := S
  else
    Result := Format('%d %s', [AnAgeInt, S]);
end;

function GetAge2Str(ADateTime: TDateTime): string;
var
  LNow, DateDiffRest: TDateTime;
  ResultKind: TGetAgeResultKind;
  Age: Integer;
begin
  LNow := Now;
  Age := GetAge(LNow, ADateTime, ResultKind, DateDiffRest);
  Result := GetAgeResultToStr(Age, ResultKind, False);
  if not (ResultKind in [garkSecond, garkNow]) and (DateDiffRest > 0) then
  begin
    Age := GetAge(LNow, LNow - DateDiffRest, ResultKind, DateDiffRest);
    Result := Result + ' ' + GetAgeResultToStr(Age, ResultKind, False);
  end;
end;

function GetDateStr(ADateFormat: string; ADateTime: TDateTime): string;
begin
  if ADateFormat = AgeDateFormat then
    Result := GetAgeStr(ADateTime - 0.1 / 86400)
  else
  if ADateFormat = Age2DateFormat then
    Result := GetAge2Str(ADateTime - 0.1 / 86400)
  else
    Result := FormatDateTime(ADateFormat, ADateTime);
end;

constructor TCustomLiveBlameData.Create(const AFileName: string);
begin
  inherited Create(nil);
  FFileName := AFileName;
  FRevisions := TObjectList<TJVCSLineHistoryRevision>.Create;
  FLines := TList<TJVCSLineHistoryRevision>.Create;
  FOrgLines := TList<TJVCSLineHistoryRevision>.Create;
  FDeletedLines := TObjectList<TDeletedLines>.Create;
  FLastAge := 0;
  FBlameInfoAvailable := False;
  FBlameInfoReady := False;
  FRevisionColorList := TObjectList<TRevisionColor>.Create;
  FBlameCounter := 0;
end;

destructor TCustomLiveBlameData.Destroy;
begin
  FDeletedLines.Free;
  FRevisionColorList.Free;
  FOrgLines.Free;
  FLines.Free;
  FRevisions.Free;
  inherited Destroy;
end;

procedure TCustomLiveBlameData.UpdateSettings(ASettings: TJVCSLineHistorySettings);
var
  I, Idx: Integer;
  LHRevision: TJVCSLineHistoryRevision;
begin
  FRevisionColorList.Clear;
  for I := 0 to FRevisions.Count - 1 do
  begin
    LHRevision := FRevisions[I];
    Idx := ASettings.UserSettingsList.IndexOfUser(LHRevision.FOrgUserStr);
    if Idx <> -1 then
      LHRevision.FUserStr := ASettings.UserSettingsList[Idx].VisibleName
    else
      LHRevision.FUserStr := LHRevision.FOrgUserStr;
    LHRevision.FDateStr := GetDateStr(ASettings.DateFormat, LHRevision.FDate);
  end;
end;

{$IFDEF SVNINTERNAL}
{ TSvnLiveBlameData }

procedure TSvnLiveBlameData.AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
begin
  FBlameInfoReady := True;
  FStage := 3;
  FPaintBox.Invalidate;
end;

procedure TSvnLiveBlameData.BuildLineHistory(ASettings: TJVCSLineHistorySettings);
var
  I, Idx: Integer;
  LHRevision: TJVCSLineHistoryRevision;
  RevisionsDict: TDictionary<Integer, TJVCSLineHistoryRevision>;
begin
  RevisionsDict := TDictionary<Integer, TJVCSLineHistoryRevision>.Create;
  try
    FOrgLines.Clear;
    FLines.Clear;
    FRevisions.Clear;
    FRevisionColorList.Clear;
    if FSvnItem.HistoryCount > 0 then
    begin
      for I := FSvnItem.HistoryCount - 1 downto 0 do
      begin
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        LHRevision := FRevisions.Last;
        RevisionsDict.Add(FSvnItem.HistoryItems[I].Revision, LHRevision);
        LHRevision.FRevisionStr := IntToStr(FSvnItem.HistoryItems[I].Revision);
        LHRevision.FOrgUserStr := FSvnItem.HistoryItems[I].Author;
        Idx := ASettings.UserSettingsList.IndexOfUser(FSvnItem.HistoryItems[I].Author);
        if Idx <> -1 then
          LHRevision.FUserStr := ASettings.UserSettingsList[Idx].VisibleName
        else
          LHRevision.FUserStr := FSvnItem.HistoryItems[I].Author;
        LHRevision.FDateStr := GetDateStr(ASettings.DateFormat, FSvnItem.HistoryItems[I].Time);
        LHRevision.FDate := FSvnItem.HistoryItems[I].Time;
        LHRevision.FComment := TrimRight(FSvnItem.HistoryItems[I].LogMessage);
      end;
      FRevisions.Add(TJVCSLineHistoryRevision.Create);
      FBufferRevision := FRevisions.Last;
      FBufferRevision.FRevisionStr := 'Buff';
      FBufferRevision.FUserStr := 'User';//TODO:

      FBufferRevision.FDateStr := GetDateStr(ASettings.DateFormat, Now);
      FBufferRevision.FDate := Now;
      FRevisions.Add(TJVCSLineHistoryRevision.Create);
      FFileRevision := FRevisions.Last;
      FFileRevision.FRevisionStr := 'File';
      FFileRevision.FUserStr := 'User';//TODO:
      FFileRevision.FDateStr := GetDateStr(ASettings.DateFormat, Now);
      FFileRevision.FDate := Now;
      for I := 1 to FSvnItem.HistoryItems[0].BlameCount do
      begin
        if not RevisionsDict.TryGetValue(FSvnItem.HistoryItems[0].BlameItems[I].Revision, LHRevision) then
          LHRevision := nil;
        FOrgLines.Add(LHRevision);
        FLines.Add(LHRevision);
      end;
    end;
  finally
    RevisionsDict.Free;
  end;
end;

procedure TSvnLiveBlameData.Completed;
begin
  if FSvnItem.HistoryCount > 0 then
  begin
    FSvnItem.HistoryItems[0].StartLoadingBlame(Self);
    FMaxRevision := FSvnItem.HistoryItems[0].Revision;
    FFirstRevisionIDStr := IntToStr(FSvnItem.HistoryItems[Pred(FSvnItem.HistoryCount)].Revision);
    FStage := 2;
  end;
end;

procedure TSvnLiveBlameData.HandleBlameLoad(Sender: TObject);
begin
  FBlameCounter := FBlameCounter + TSvnClient(Sender).DataCounter;
  FBlameRevision := TSvnClient(Sender).BlameRevision;
  FPaintBox.Invalidate;
end;

procedure TSvnLiveBlameData.Load;
begin
  if IDEClient.SvnClient.IsPathVersioned(FFileName) then
  begin
    if not Assigned(FSvnItem) or (FSvnItem.PathName <> FFileName) then
    begin
      FBlameInfoAvailable := False;
      FBlameInfoReady := False;
      FMaxRevision := -1;
      FBlameCounter := 0;
      if Assigned(FSvnItem) then
        FSvnItem.Free;
      IDEClient.SvnClient.OnBlameLoad := HandleBlameLoad;
      FSvnItem := TSvnItem.Create(IDEClient.SvnClient, nil, FFileName);
      FSvnItem.AsyncUpdate := Self;
      FStage := 1;
      FSvnItem.AsyncReloadHistory;
    end;
  end;
end;

procedure TSvnLiveBlameData.UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex,
  LastNewIndex: Integer; ForceUpdate: Boolean);
begin
//
end;
{$ENDIF SVNINTERNAL}

{ TGenericLiveBlameData }

procedure TGenericLiveBlameData.AnnotationComplete(const AnnotationLineProvider: IOTAAnnotationLineProvider);
begin
  FAnnotationLineProvider := AnnotationLineProvider;
  FAnnotationLineProviderHelperUpdateRequired := True;
  FBlameInfoReady := True;
  FStage := 3;
  FPaintBox.Invalidate;
end;

procedure TGenericLiveBlameData.BuildLineHistory(ASettings: TJVCSLineHistorySettings);
var
  I, Idx: Integer;
  LHRevision: TJVCSLineHistoryRevision;
  RevisionsDict: TDictionary<string, TJVCSLineHistoryRevision>;
begin
  if Assigned(FAnnotationLineProvider) then
  begin
    RevisionsDict := TDictionary<string, TJVCSLineHistoryRevision>.Create;
    try
      FOrgLines.Clear;
      FLines.Clear;
      FRevisions.Clear;
      FRevisionColorList.Clear;
      if FFileHistory.Count > 0 then
      begin
        for I := FFileHistory.Count - 1 downto 0 do
        begin
          FRevisions.Add(TJVCSLineHistoryRevision.Create);
          LHRevision := FRevisions.Last;
          RevisionsDict.Add(FFileHistory.Ident[I], LHRevision);
          LHRevision.FRevisionStr := FFileHistory.Ident[I];
          LHRevision.FOrgUserStr := FFileHistory.Author[I];
          Idx := ASettings.UserSettingsList.IndexOfUser(FFileHistory.Author[I]);
          if Idx <> -1 then
            LHRevision.FUserStr := ASettings.UserSettingsList[Idx].VisibleName
          else
            LHRevision.FUserStr := FFileHistory.Author[I];
          LHRevision.FDateStr := GetDateStr(ASettings.DateFormat, UTCToTzDateTime(FFileHistory.Date[I]));
          LHRevision.FDate := UTCToTzDateTime(FFileHistory.Date[I]);
          LHRevision.FComment := TrimRight(FFileHistory.Comment[I]);
        end;
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        FBufferRevision := FRevisions.Last;
        FBufferRevision.FRevisionStr := 'Buff';
        FBufferRevision.FUserStr := 'User';//TODO:

        FBufferRevision.FDateStr := GetDateStr(ASettings.DateFormat, Now);
        FBufferRevision.FDate := Now;
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        FFileRevision := FRevisions.Last;
        FFileRevision.FRevisionStr := 'File';
        FFileRevision.FUserStr := 'User';//TODO:
        FFileRevision.FDateStr := GetDateStr(ASettings.DateFormat, Now);
        FFileRevision.FDate := Now;
        if FAnnotationLineProviderHelperUpdateRequired then
        begin
          FAnnotationLineProviderHelper.Clear;
          for I := 1 to FAnnotationLineProvider.Count do
            FAnnotationLineProviderHelper.Add(Trim(FAnnotationLineProvider.GutterInfo[I]));
          FAnnotationLineProviderHelperUpdateRequired := False;
        end;
        for I := 0 to FAnnotationLineProviderHelper.Count - 1 do
        begin
          if not RevisionsDict.TryGetValue(FAnnotationLineProviderHelper[I], LHRevision) then
            LHRevision := nil;
          FOrgLines.Add(LHRevision);
          FLines.Add(LHRevision);
        end;
      end;
    finally
      RevisionsDict.Free;
    end;
  end;
  FLoading := False;
end;

procedure TGenericLiveBlameData.Completed;
var
  MS: TMemoryStream;
  SA: TStreamAdapter;
  RC: IStream;
  R, W: Int64;
  StreamStat: TStatStg;
  UpdateRequired: Boolean;
begin
  if FUpdateCheck then
  begin
    UpdateRequired := ((FFileHistory.Count <> FUpdateCheckRevisionCount) or
    ((FFileHistory.Count > 0) and (FUpdateCheckTopRevisionID <> FFileHistory.Ident[0])));
    FUpdateCheck := False;
    if UpdateRequired then
    begin
      FAnnotationLineProvider := nil;
      FAnnotationLineProviderHelper.Clear;
    end;
  end
  else
    UpdateRequired := True;
  if UpdateRequired then
  begin
    if Assigned(FFileHistory) and Supports(FFileHistory, IOTAAsynchronousAnnotationProvider) and
      (FFileHistory as IOTAAsynchronousAnnotationProvider).CanAnnotateFile(FFileName) and
      (FFileHistory.Count > 0) then
    begin
      FUpdateCheckTopRevisionID := FFileHistory.Ident[0];
      FUpdateCheckRevisionCount := FFileHistory.Count;
      FFirstRevisionIDStr := FFileHistory.Ident[FFileHistory.Count - 1];
      (FFileHistory as IOTAAsynchronousAnnotationProvider).StartAsynchronousUpdate(FFileName, 0, Self);

      RC := FFileHistory.Content[0];
      MS := TMemoryStream.Create;
      SA := TStreamAdapter.Create(MS);
      RC.Seek(0, 0, R);
      if RC.Stat(StreamStat, 0) = S_OK then
        RC.CopyTo(SA, StreamStat.cbSize, R, W);
      MS.Position := 0;
      SetLength(FLatestRevisionContent, MS.Size);
      MS.Read(PAnsiChar(FLatestRevisionContent)^, MS.Size);
    end;
    FStage := 2;
    FPaintBox.Invalidate;
  end
  else
  begin
    FBlameInfoReady := True;
    FStage := 3;
    FPaintBox.Invalidate;
  end;
end;

constructor TGenericLiveBlameData.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FAnnotationLineProviderHelper := TList<string>.Create;
end;

destructor TGenericLiveBlameData.Destroy;
begin
  FAnnotationLineProviderHelper.Free;
  inherited Destroy;
end;

procedure TGenericLiveBlameData.Load;
var
  I: Integer;
  VersionControlServices: IOTAVersionControlServices;
  FileHistoryManager: IOTAFileHistoryManager;
  FileHistoryProvider: IOTAFileHistoryProvider;
  ProviderName: string;
  Idents: TStringList;
begin
  if (not FLoading) and (not Assigned(FAnnotationLineProvider) or FUpdateCheck) then
  begin
    if FUpdateCheck then
    begin
      FLastAge := 0;
      FLastStreamSize := 0;
      FBlameInfoAvailable := False;
      FBlameInfoReady := False;
    end
    else
      FAnnotationLineProvider := nil;

    ProviderName := '';
    VersionControlServices := BorlandIDEServices as IOTAVersionControlServices;
    Idents := TStringList.Create;
    try
      Idents.Add(FFileName);
      for I := 0 to VersionControlServices.Count - 1 do
        if VersionControlServices.Items[I].IsFileManaged(nil, Idents) then
        begin
          if Supports(VersionControlServices.Items[I], IOTAVersionControlNotifier150) then
          begin
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'embarcadero.subversion' then
              ProviderName := 'TOndrej.SubversionFileHistoryProvider'
            else
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'versioninsight.mercurial' then
              ProviderName := 'VersionInsight.HgFileHistoryProvider'
            else
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'versioninsight.git' then
              ProviderName := 'VersionInsight.GitFileHistoryProvider';
          end;
          Break;
        end;
    finally
      Idents.Free;
    end;
    if ProviderName <> '' then
    begin
      FileHistoryManager := BorlandIDEServices as IOTAFileHistoryManager;
      FileHistoryProvider := nil;
      for I := 0 to FileHistoryManager.Count - 1 do
        if FileHistoryManager.FileHistoryProvider[I].Ident = ProviderName then
        begin
          FileHistoryProvider := FileHistoryManager.FileHistoryProvider[I];
          Break;
        end;
      if Assigned(FileHistoryProvider) then
      begin
        FLoading := True;
        (FileHistoryProvider as IOTAAsynchronousHistoryProvider).StartAsynchronousUpdate(FFileName, Self);
      end;
    end;
  end;
end;

function TGenericLiveBlameData.UpdateHistoryItems(FileHistory: IOTAFileHistory;
  FirstNewIndex, LastNewIndex: Integer): Boolean;
begin
  FFileHistory := FileHistory;
  Result := True;
end;

{ TLiveBlameEditorPanel }

procedure TLiveBlameEditorPanel.BuildLineHistory;
begin
  FLiveBlameData.BuildLineHistory(FSettings);
end;

function CallWndProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(CallWndProcHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := PCWPStruct(lParam)^.message;
    Msg.LParam := PCWPStruct(lParam)^.lParam;
    Msg.WParam := PCWPStruct(lParam)^.wParam;
    if (Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST) then
      asm
        nop
      end;
    if Assigned(CurrentPanel) and (CurrentWindow = PCWPRetStruct(lParam)^.hwnd) and
      ({(Msg.Msg = WM_VSCROLL) or (Msg.Msg = WM_HSCROLL) or }(Msg.Msg = WM_KEYDOWN) or (Msg.Msg = VK_DOWN) or (Msg.Msg = VK_UP)
       or (Msg.Msg = WM_SYSKEYDOWN) or (Msg.Msg = CM_DIALOGKEY)) then
      CurrentPanel.PaintBoxPaint(nil);
  end;

  Result := CallNextHookEx(CallWndProcHook, nCode, wParam, lParam);
end;

function CallWndProcRet(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(CallWndProcRetHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := PCWPRetStruct(lParam)^.message;
    Msg.LParam := PCWPRetStruct(lParam)^.lParam;
    Msg.WParam := PCWPRetStruct(lParam)^.wParam;
    Msg.Result := PCWPRetStruct(lParam)^.lResult;
    if (Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST) then
      asm
        nop
      end;
    if Assigned(CurrentPanel) and (CurrentWindow = PCWPRetStruct(lParam)^.hwnd) and
      ((Msg.Msg = WM_VSCROLL) or (Msg.Msg = WM_HSCROLL) or (Msg.Msg = WM_KEYDOWN) or (Msg.Msg = VK_DOWN) or (Msg.Msg = VK_UP)
       or (Msg.Msg = WM_SYSKEYDOWN) or (Msg.Msg = CM_DIALOGKEY)) then
      CurrentPanel.PaintBoxPaint(nil);
    if (Msg.Msg = WM_SETCURSOR) and (CurrentPanel.Cursor = crHandPoint) then
      Windows.SetCursor(Screen.Cursors[crHandPoint]);
  end;

  Result := CallNextHookEx(CallWndProcRetHook, nCode, wParam, lParam);
end;

function GetMsgProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
  Msg2: TWMMouseMove absolute Msg;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(GetMsgHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    if wParam = PM_REMOVE then
    begin
      FillChar(Msg, SizeOf(Msg), 0);
      Msg.Msg := PMsg(lParam)^.message;
      Msg.LParam := PMsg(lParam)^.lParam;
      Msg.WParam := PMsg(lParam)^.wParam;
      if Assigned(CurrentPanel) and (CurrentWindow = PMsg(lParam)^.hwnd) then
      begin
        if Msg.Msg = WM_KEYDOWN then
          PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0)
        else
        if Msg.Msg = WM_LBUTTONDOWN then
          CurrentPanel.HandleOnClick(nil)
        else
        if Msg.Msg = WM_LBUTTONUP then
          PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0);
      end;
    end;
  end;

  Result := CallNextHookEx(GetMsgHook, nCode, wParam, lParam);
end;

procedure TLiveBlameEditorPanel.CheckInstallHook;
var
  EditControl: TObject;
begin
  if not FInstalledHook then
  begin
    EditControl := GetEditControl;
    if EditControl is TControl then
    begin
      EditControlHook := TControl(EditControl);
      CallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC, CallWndProc, 0, GetCurrentThreadId);
      CallWndProcRetHook := SetWindowsHookEx(WH_CALLWNDPROCRET, CallWndProcRet, 0, GetCurrentThreadId);
      GetMsgHook := SetWindowsHookEx(WH_GETMESSAGE, GetMsgProc, 0, GetCurrentThreadId);
      FInstalledHook := True;
    end;
  end;
end;

procedure TLiveBlameEditorPanel.CMHintShow(var Msg: TMessage);
begin
  inherited;
end;

constructor TLiveBlameEditorPanel.Create(AOwner: TComponent);
var
  User: TJVCSLineHistoryUserSettingsItem;
begin
  inherited Create(AOwner);
  FWizard := nil;
  Align := alLeft;
  Width := 30 + 190;
  BevelOuter := bvNone;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10000;
  FPaintBox := TLiveBlamePaintBox.Create(Self);
  FPaintBox.Parent := Self;
  FPaintBox.Align := alClient;
  FPaintBox.OnHintMessage := HandleHintShow;
  FPaintBox.Visible := True;
  //DoubleBuffered := True;
  FCnt := 0;
  FCY := 1;
  FCursorLine := 0;
  FTopLine := 0;
  FActiveEditView1 := nil;
  FModule := nil;
  FTimer.OnTimer := OnTimer;
  FPaintBox.OnPaint := PaintBoxPaint;
  FPaintBox.OnMouseLeave := HandleMouseLeave;
  FPaintBox.OnClick := HandleOnClick;
  FCheckShowTimer := TTimer.Create(Self);
  FCheckShowTimer.Interval := 2;
  FCheckShowTimer.Enabled := False;
  FCheckShowTimer.OnTimer := OnCheckTimer;
  FLiveBlameData := TLiveBlameData.Create('');
  FLiveBlameData.FPaintBox := FPaintBox;
  FLiveBlameDataList := TObjectList<TLiveBlameData>.Create;
  FLiveBlameDataList.Add(FLiveBlameData);
  FSettings := TJVCSLineHistorySettings.Create;
  FSettings.ShowLineNumbers := False;
  FSettings.DateFormat := 'AGE2';
  FSettings.DateStartColor := $F4F4F4;
  FSettings.DateEndColor := clRed;
  FSettings.RevisionStartColor := $F4F4F4;
  FSettings.RevisionEndColor := clAqua;

  User := FSettings.UserSettingsList.Add;
  User.UserName := 'uschuster';
  User.VisibleName := 'US';
  User.Color := clLime;//this is optional

  FColorList := TStringList.Create;
  FColorList.AddObject('', TObject(GetNextColor));
  FRevisionRectangles := TRevisionRectangleList.Create;
  FRevisionHintRectangles := TRevisionRectangleList.Create;
  FDeletedLinesHintTriangles := TDeletedLinesTriangleList.Create;
  FHighlightY := -1;
  FLastHighlightY := -1;
  FPaintBox.OnMouseMove := HandleMouseMove;
  FOnRevisionClick := HandleRevisionClick;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := Self;
  FProgressBar.Visible := False;
  FLastPresetTimeStamp := -1;
  if GetPresets <> nil then
    SetPreset(GetPresets.SelectedID);
  CreatePopupMenu;
  UpdateModificationColors;
  Visible := False;
end;

procedure TLiveBlameEditorPanel.CreatePopupMenu;
begin
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.OnPopup := HandlePopupMenuPopup;
  FMenuItem1 := TMenuItem.Create(FPopupMenu);
  FMenuItem1.Caption := 'Preset 1';
  FMenuItem1.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem1);
  FMenuItem2 := TMenuItem.Create(FPopupMenu);
  FMenuItem2.Caption := 'Preset 2';
  FMenuItem2.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem2);
  FMenuItem3 := TMenuItem.Create(FPopupMenu);
  FMenuItem3.Caption := 'Preset 3';
  FMenuItem3.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem3);
  FPaintBox.PopupMenu := FPopupMenu;
  PopupMenu := FPopupMenu;
end;

destructor TLiveBlameEditorPanel.Destroy;
begin
  UnInstallHooks;
  FLiveBlameDataList.Free;
  FDeletedLinesHintTriangles.Free;
  FRevisionHintRectangles.Free;
  FRevisionRectangles.Free;
  FColorList.Free;
  FSettings.Free;
  //FLiveBlameData.Free;
  FCheckShowTimer.Enabled := False;
  FTimer.Enabled := False;
  FPaintBox.OnPaint := nil;
  if Assigned(FWizard) then
    FWizard.RemovePanel(Self);
  inherited Destroy;
end;

function CalcColor(AStartColor, AEndColor: TColor; AFactor: Double): TColor;
var
  StartRGB: array [0..2] of Byte;
begin
  AStartColor := ColorToRGB(AStartColor);
  AEndColor := ColorToRGB(AEndColor);
  StartRGB[0] := GetRValue(AStartColor);
  StartRGB[1] := GetGValue(AStartColor);
  StartRGB[2] := GetBValue(AStartColor);
  Result := RGB(StartRGB[0] + Trunc((GetRValue(AEndColor) - StartRGB[0]) * AFactor),
    StartRGB[1] + Trunc((GetGValue(AEndColor) - StartRGB[1]) * AFactor),
    StartRGB[2] + Trunc((GetBValue(AEndColor) - StartRGB[2]) * AFactor));
end;

function TLiveBlameEditorPanel.DoGetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
var
  DC, DL: Double;
  I, Idx, IdxS: Integer;
  MinDate, MaxDate: TDateTime;
begin
  Result := nil;
  for I := 0 to Pred(FLiveBlameData.FRevisionColorList.Count) do
    if TRevisionColor(FLiveBlameData.FRevisionColorList[I]).LineHistoryRevision = ALineHistoryRevision then
    begin
      Result := TRevisionColor(FLiveBlameData.FRevisionColorList[I]);
      Break;
    end;
  if not Assigned(Result) and (ALineHistoryRevision.RevisionStr = 'Buff') then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);
    Result.DateColor := FModificationColorBuffer;
    Result.RevisionColor := FModificationColorBuffer;
    Result.UserColor := FModificationColorBuffer;
  end
  else
  if not Assigned(Result) and (ALineHistoryRevision.RevisionStr = 'File') then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);
    Result.DateColor := FModificationColorFile;
    Result.RevisionColor := FModificationColorFile;
    Result.UserColor := FModificationColorFile;
  end
  else
  if not Assigned(Result) then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);

    DL := FLiveBlameData.FRevisions.Count;
    if DL > 0 then
    begin
      DC := 0;
      for I := 0 to Pred(FLiveBlameData.FRevisions.Count) do
        if FLiveBlameData.FRevisions[I] = ALineHistoryRevision then
        begin
          DC := I + 1;
          Break;
        end;
    end
    else
      DC := 0;
    if DL = 0 then
      DC := 0
    else
      DC := DC / DL;
    Result.RevisionColor := CalcColor(FSettings.RevisionStartColor, FSettings.RevisionEndColor, DC);

    MinDate := MaxInt;
    MaxDate := 0;
    for I := 0 to Pred(FLiveBlameData.FRevisions.Count) do
    begin
      if FLiveBlameData.FRevisions[I].Date < MinDate then
        MinDate := FLiveBlameData.FRevisions[I].Date;
      if FLiveBlameData.FRevisions[I].Date > MaxDate then
        MaxDate := FLiveBlameData.FRevisions[I].Date;
    end;
    DC := 0;
    DL := 0;
    if (MinDate < MaxInt) and (MaxDate > 0) then
    begin
      if Assigned(ALineHistoryRevision) then
      begin
        DC := ALineHistoryRevision.Date - MinDate;
        DL := MaxDate - MinDate;
      end;
    end;
    if DL = 0 then
      DC := 0
    else
      DC := DC / DL;
    Result.DateColor := CalcColor(FSettings.DateStartColor, FSettings.DateEndColor, DC);

    Idx := 0;
    if Assigned(ALineHistoryRevision) then
    begin
      IdxS := FSettings.UserSettingsList.IndexOfUser(ALineHistoryRevision.OrgUserStr);
      if (IdxS <> -1) and (FSettings.UserSettingsList[IdxS].Color <> clNone) and
        (FSettings.UserSettingsList[IdxS].Color <> clDefault) then
      begin
        Idx := FColorList.IndexOf(ALineHistoryRevision.OrgUserStr);
        if Idx = -1 then
        begin
          FColorList.AddObject(ALineHistoryRevision.OrgUserStr, TObject(FSettings.UserSettingsList[IdxS].Color));
          Idx := Pred(FColorList.Count);
        end;
      end
      else
      begin
        Idx := FColorList.IndexOf(ALineHistoryRevision.OrgUserStr);
        if Idx = -1 then
        begin
          FColorList.AddObject(ALineHistoryRevision.OrgUserStr, TObject(GetNextColor));
          Idx := Pred(FColorList.Count);
        end;
      end;
    end;
    if Idx < FColorList.Count then
      Result.UserColor := Integer(FColorList.Objects[Idx])
    else
      Result.UserColor := clNone;
  end;
end;

function FindControlAtPt(Control: TWinControl; Pt: TPoint; MinClass: TClass): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := Control.ControlCount - 1 downto 0 do
    if (Control.Controls[I] is MinClass) and PtInRect(Control.Controls[I].BoundsRect, Pt) then
    begin
      Result := Control.Controls[I];
      Break;
    end;
end;

procedure TLiveBlameEditorPanel.OnCheckTimer(Sender: TObject);
begin
  OutputDebugString('OnCheckTimer');
  ShowIfEditControl;
  FCheckShowTimer.Enabled := False;
end;

function TLiveBlameEditorPanel.EditControlVisible: Boolean;
var
  I: Integer;
  tempControl: TWinControl;
  tempControl2: TControl;
begin
  Result := False;
  if Owner.ClassName = 'TEditWindow' then
  begin
   OutputDebugString('EditControlVisible: Owner is TEditWindow');
    with Owner do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinControl) and (Components[I].ClassName = 'TEditControl') then
        begin
          OutputDebugString('EditControlVisible: Found TEditControl');
          tempControl := TWinControl(Components[I]);
          Result := tempControl.Visible;
          if Result then
          begin
            tempControl2 := FindControlAtPt(tempControl.Parent,
              Point(tempControl.Left, tempControl.Top), TWinControl);
            Result := tempControl = tempControl2;
          end;
          Break;
        end;
  end;
end;

function TLiveBlameEditorPanel.GetDeleteHintRect(ADeletedLines: TDeletedLines;
  var ARect: TRect): Boolean;
var
  B: TBitmap;
  TargetCanvas: TCanvas;
  S: string;
  R: TRect;
begin
  B := TBitmap.Create;
  try
    TargetCanvas := B.Canvas;
    TargetCanvas.Font.Name := 'Courier New';
    TargetCanvas.Font.Size := 10;
    R := TargetCanvas.ClipRect;
    R := Rect(0, 0, 80, 76);
    S := ADeletedLines.Hint;
    TargetCanvas.TextRect(R, S, [tfCalcRect, tfNoPrefix]);
    R.Bottom := R.Bottom + 4;
    ARect := R;
    Result := True;
  finally
    B.Free;
  end;
end;

function TLiveBlameEditorPanel.GetEditControl: TObject;
var
  I: Integer;
begin
  Result := nil;
  if Owner.ClassName = 'TEditWindow' then
  begin
    with Owner do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinControl) and (Components[I].ClassName = 'TEditControl') then
        begin
          Result := Components[I];
          CurrentWindow := TWinControl(Components[I]).Handle;
          Break;
        end;
  end;
end;

function TLiveBlameEditorPanel.GetHintRect(ARevision: TRevisionColor; var ARect: TRect): Boolean;
var
  B: TBitmap;
  TargetCanvas: TCanvas;
  S: string;
  R, RLogMessage: TRect;
  TextExt: TSize;
begin
  B := TBitmap.Create;
  try
    TargetCanvas := B.Canvas;
    TargetCanvas.Font := Screen.HintFont;
    R := TargetCanvas.ClipRect;
    R := Rect(0, 0, 80, 76);
    S := ARevision.LineHistoryRevision.Comment;
    RLogMessage := Rect(4, 76, 300, 300);
    TargetCanvas.TextRect(RLogMessage, S, [tfCalcRect, tfNoPrefix]);
    R.Bottom := RLogMessage.Bottom + 4;
    if R.Right < RLogMessage.Right + 4 then
      R.Right := RLogMessage.Right + 4;
    TextExt := TargetCanvas.TextExtent(ARevision.LineHistoryRevision.RevisionStr);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    TextExt := TargetCanvas.TextExtent(ARevision.LineHistoryRevision.DateStr);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    S := ARevision.LineHistoryRevision.UserStr;
    if (ARevision.LineHistoryRevision.OrgUserStr <> '') and (ARevision.LineHistoryRevision.OrgUserStr <> S) then
      S := S + ' (' + ARevision.LineHistoryRevision.OrgUserStr + ')';
    TextExt := TargetCanvas.TextExtent(S);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    ARect := R;
    Result := True;
  finally
    B.Free;
  end;
end;

function TLiveBlameEditorPanel.GetHintTextAndSize(AX, AY: Integer; var ARevision: TRevisionColor;
  var ABlockRect, AHintRect: TRect; var ADeletedLines: TDeletedLines): Boolean;
var
  I, Idx: Integer;
  RevisionRect: TRevisionRectangle;
begin
  AHintRect := Rect(0, 0, 0, 0);
  ABlockRect := Rect(0, 0, 0, 0);
  ARevision := nil;
  ADeletedLines := nil;
  Result := False;
  Idx := FDeletedLinesHintTriangles.Find(AX, AY);
  if Idx <> -1 then
  begin
    if GetDeleteHintRect(FDeletedLinesHintTriangles[Idx].DeletedLines, AHintRect) then
    begin
      ADeletedLines := FDeletedLinesHintTriangles[Idx].DeletedLines;
      ABlockRect.Left := FDeletedLinesHintTriangles[Idx].Point.X;
      ABlockRect.Top := FDeletedLinesHintTriangles[Idx].Point.Y;
      ABlockRect.Right := ABlockRect.Left + 8;
      ABlockRect.Top := ABlockRect.Bottom + 8;
      Result := True;
    end;
  end
  else
  begin
    RevisionRect := FRevisionHintRectangles.FindRect(AX, AY);
    if Assigned(RevisionRect) then
    begin
      for I := 0 to FLiveBlameData.FRevisionColorList.Count - 1 do
        if FLiveBlameData.FRevisionColorList[I].LineHistoryRevision.RevisionStr = RevisionRect.RevisionIDStr then
        begin
          ARevision := FLiveBlameData.FRevisionColorList[I];
          Break;
        end;
      if Assigned(ARevision) and GetHintRect(ARevision, AHintRect) then
      begin
        ABlockRect := RevisionRect.Rect;
        Result := True;
      end;
    end;
  end;
end;

function TLiveBlameEditorPanel.GetLineColor(ALine, AColorIndex: Integer): TColor;
var
  RevisionColor: TRevisionColor;
begin
  Result := clNone;
  if AColorIndex in [1, 2, 3] then
  begin
    if (FLiveBlameData.FLines.Count > ALine) then
      RevisionColor := DoGetRevisionColor(FLiveBlameData.FLines[ALine])
    else
      RevisionColor := DoGetRevisionColor(nil);
    if Assigned(RevisionColor) then
    begin
      case AColorIndex of
        1: Result := RevisionColor.RevisionColor;
        2: Result := RevisionColor.DateColor;
        3: Result := RevisionColor.UserColor;
      end;
    end;
  end;
end;

function TLiveBlameEditorPanel.GetNextColor: TColor;
begin
  case (14 - FColorList.Count mod 15) of
    0: Result := RGB(128, 128, 128);
    1: Result := RGB(255, 128, 128);
    2: Result := RGB(128, 255, 128);
    3: Result := RGB(128, 128, 255);
    4: Result := RGB(255, 255, 128);
    5: Result := RGB(128, 255, 255);
    6: Result := RGB(255, 128, 255);
    7: Result := RGB(192, 192, 192);
    8: Result := RGB(255, 192, 192);
    9: Result := RGB(192, 255, 192);
   10: Result := RGB(192, 192, 255);
   11: Result := RGB(255, 255, 192);
   12: Result := RGB(192, 255, 255);
   13: Result := RGB(255, 192, 255);
   14: Result := RGB(255, 255, 255);
   else
     Result := RGB(255, 255, 255);
  end;
end;

procedure TLiveBlameEditorPanel.HandleHintShow(var Msg: TMessage);
var
  BlockRect, HintRect: TRect;
  P, PClient: TPoint;
  FoundHint: Boolean;
  Revision: TRevisionColor;
  DeletedLines: TDeletedLines;
begin
  GetCursorPos(P);
  PClient := ScreenToClient(P);
  FoundHint := GetHintTextAndSize(PClient.X, PClient.Y, Revision, BlockRect, HintRect, DeletedLines);
  if FoundHint then
  begin
    InflateRect(BlockRect, 2, 2);
    FHintData.Revision := Revision;
    FHintData.Rect := HintRect;
    FHintData.DeletedLines := DeletedLines;
    with PHintInfo(Msg.LParam)^ do
    begin
      HintStr := ' ';
      //HideTimeOut :=
      HintWindowClass := TBlameHintWindow;
      CursorRect := BlockRect;
      //HintPos.Y := Max(HintPos.Y, ClientToScreen(BlockRect.BottomRight).Y) + 2;
      HintData := @FHintData;
    end;
    Msg.Result := 0;
  end
  else
    Msg.Result := 1;
end;

procedure TLiveBlameEditorPanel.HandleMouseLeave(Sender: TObject);
begin
  Cursor := crDefault;
  FPaintBox.Cursor := crDefault;
  FHighlightY := -1;
  FPaintBox.Invalidate;
end;

procedure TLiveBlameEditorPanel.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewCursor: TCursor;
  TopY: Integer;

  EditControl: TControl;
begin
  if Assigned(FOnRevisionClick) then
  begin
    if FRevisionRectangles.Find(X, Y, TopY, FLastHighlightedRevisionIDStr) and
      (FLiveBlameData.FFirstRevisionIDStr <> FLastHighlightedRevisionIDStr) then
      NewCursor := crHandPoint
    else
    begin
      NewCursor := crDefault;
      TopY := -1;
    end;
    if (NewCursor <> Cursor) or (FHighlightY <> TopY) then
    begin
      FPaintBox.Cursor := NewCursor;
      FHighlightY := TopY;
      FPaintBox.Invalidate;

      EditControl := TControl(GetEditControl);
      if Assigned(EditControl) then
        EditControl.Cursor := NewCursor;
      Cursor := NewCursor;
      Windows.SetCursor(Screen.Cursors[NewCursor]);
    end;
  end;
end;

procedure TLiveBlameEditorPanel.HandleOnClick(Sender: TObject);
var
  PT: TPoint;
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
  NewTopRow: Integer;
begin
  if (Cursor = crHandPoint) and Assigned(FOnRevisionClick) and
    (FLastHighlightedRevisionIDStr <> '') then
    FOnRevisionClick(Self, FLastHighlightedRevisionIDStr)
  else
  if (Cursor <> crHandPoint) and (FSettings.ColorBarOrderList.Count > 0) and
    GetCursorPos(PT) then
  begin
    PT := ScreenToClient(PT);
    if PT.X < FSettings.ColorBarOrderList.Count * 8 then
    begin
      EditView := (BorlandIDEServices as IOTAEditorServices).TopView;
      if Assigned(EditView) then
      begin
        EditPos := EditView.CursorPos;
        EditPos.Line := (PT.Y * EditView.Position.LastRow) div FPaintBox.Height;
        EditView.CursorPos := EditPos;
        if (EditPos.Line < EditView.TopRow) or (EditPos.Line > EditView.BottomRow) then
        begin
          NewTopRow := EditPos.Line - (EditView.BottomRow - EditView.TopRow) div 2;
          if NewTopRow < 1 then
            NewTopRow := 1
          else
          if NewTopRow > EditView.Position.LastRow then
            NewTopRow := EditView.Position.LastRow;
          EditView.SetTopLeft(NewTopRow, 1);
        end;
        EditView.Paint;
        FPaintBox.Invalidate;
      end;
    end;
  end;
end;

procedure TLiveBlameEditorPanel.HandlePopupMenu(Sender: TObject);
var
  PresetID: Integer;
  MenuItem: TMenuItem;
  Presets: TJVCSLineHistoryPresets;
begin
  PresetID := -1;
  if Sender = FMenuItem1 then
  begin
    FSettings.ShowRevisionInfoText := False;
    FSettings.ShowRevisionInfoColor := True;
    FSettings.ShowDateInfoText := False;
    FSettings.ShowDateInfoColor := False;
    FSettings.ShowUserInfoText := False;
    FSettings.ShowUserInfoColor := False;
  end
  else
  if Sender = FMenuItem2 then
  begin
    FSettings.ShowRevisionInfoText := True;
    FSettings.ShowRevisionInfoColor := True;
    FSettings.ShowDateInfoText := False;
    FSettings.ShowDateInfoColor := False;
    FSettings.ShowUserInfoText := False;
    FSettings.ShowUserInfoColor := False;
  end
  else
  if Sender = FMenuItem3 then
  begin
    FSettings.ShowRevisionInfoText := True;
    FSettings.ShowRevisionInfoColor := True;
    FSettings.ShowDateInfoText := True;
    FSettings.ShowDateInfoColor := True;
    FSettings.ShowUserInfoText := True;
    FSettings.ShowUserInfoColor := True;
  end
  else
  if Sender = FConfigMenuItem then
  begin
    (BorlandIDEServices as IOTAServices).GetEnvironmentOptions.EditOptions('Version Control', 'Blame');
    Presets := GetPresets;
    if Assigned(Presets) then
    begin
      if Presets.IndexOfID(Presets.SelectedID) <> -1 then
        PresetID := Presets.SelectedID
      else
      if Presets.Count > 0 then
        PresetID := Presets[0].ID;
    end;
  end
  else
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    if (MenuItem.GroupIndex = 128) and (MenuItem.Tag > 0) then
    begin
      PresetID := MenuItem.Tag;
      MenuItem.Checked := True;
    end;
  end;
  SetPreset(PresetID);
end;

procedure TLiveBlameEditorPanel.HandlePopupMenuPopup(Sender: TObject);
var
  Presets: TJVCSLineHistoryPresets;
  I: Integer;
  MenuItem: TMenuItem;
begin
  Presets := GetPresets;
  if (Presets <> nil) and (Presets.ModificationTimestamp <> FLastPresetTimeStamp) then
  begin
    FPopupMenu.Items.Clear;
    FMenuItem1 := nil;
    FMenuItem2 := nil;
    FMenuItem3 := nil;
    for I := 0 to Pred(Presets.Count) do
    begin
      MenuItem := TMenuItem.Create(FPopupMenu);
      MenuItem.Caption := Presets[I].Name;
      MenuItem.Tag := Presets[I].ID;
      MenuItem.GroupIndex := 128;
      MenuItem.OnClick := HandlePopupMenu;
      MenuItem.RadioItem := True;
      MenuItem.Checked := (Presets.SelectedID > 0) and (Presets[I].ID = Presets.SelectedID);
      FPopupMenu.Items.Add(MenuItem);
    end;
    MenuItem := TMenuItem.Create(FPopupMenu);
    MenuItem.Caption := '-';
    FPopupMenu.Items.Add(MenuItem);
    FConfigMenuItem := TMenuItem.Create(FPopupMenu);
    FConfigMenuItem.Caption := 'P&roperties';
    FConfigMenuItem.OnClick := HandlePopupMenu;
    FPopupMenu.Items.Add(FConfigMenuItem);
    FLastPresetTimeStamp := Presets.ModificationTimestamp;
  end;
end;

procedure TLiveBlameEditorPanel.HandleRevisionClick(ASender: TObject;
  ARevisionIDStr: string);
var
  CompareRevisionThread: TCompareRevisionThread;
begin
  {$IFDEF SVNINTERNAL}
  CompareRevisionThread := TCompareRevisionThread.Create;
  CompareRevisionThread.AddFile(FLiveBlameData.FSvnItem.PathName, StrToIntDef(ARevisionIDStr, 0), StrToIntDef(ARevisionIDStr, 0) - 1);
  {$ELSE}
  CompareRevisionThread := TCompareRevisionThread.Create;
  CompareRevisionThread.AddFile(FLiveBlameData.FFileName, ARevisionIDStr, '', FLiveBlameData.FFileHistory);
  {$ENDIF}
  CompareRevisionThread.Start;
end;

procedure TLiveBlameEditorPanel.SetEnabled(AValue: Boolean);
begin
//
end;

procedure TLiveBlameEditorPanel.SetPreset(APresetID: Integer);
var
  Idx: Integer;
  Presets: TJVCSLineHistoryPresets;
begin
  Presets := GetPresets;
  if Assigned(Presets) then
  begin
    Idx := Presets.IndexOfID(APresetID);
    if Idx <> -1 then
    begin
      FColorList.Clear;
      FColorList.AddObject('', TObject(GetNextColor));
      FSettings.Assign(Presets[Idx].Settings);
      FLiveBlameData.UpdateSettings(FSettings);
      Presets.SelectedID := APresetID;
    end;
  end;
  UpdateGutterWidth;
  FPaintBox.Invalidate;
  if Width < 5 then
    Width := 5;
end;

procedure TLiveBlameEditorPanel.ShowHidePanel(Sender: TObject);
var
  I: Integer;
begin
  if Sender is TSpeedButton then
    Visible := TSpeedButton(Sender).Down
  else
  if Sender is TAction then
  begin
    TAction(Sender).Checked := not TAction(Sender).Checked;
    Visible := TAction(Sender).Checked;
  end;
  if Assigned(FLiveBlameData) then
  begin
    if Visible and not FLiveBlameData.FButtonDown and (GetKeyState(VK_SHIFT) < 0) then
      FLiveBlameData.FUpdateCheck := True;
    FLiveBlameData.FButtonDown := Visible;
    if Visible and UpdateModificationColors then
      for I := 0 to Pred(FLiveBlameDataList.Count) do
        FLiveBlameDataList[I].FRevisionColorList.Clear;
  end;
end;

procedure TLiveBlameEditorPanel.ShowIfEditControl;
var
  I: Integer;
  State, ButtonState, ModuleChanged: Boolean;
  CurrentFileName: string;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
begin
  State := EditControlVisible;
  ButtonState := State;
  ModuleChanged := False;
  if State then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    if Assigned(ModuleServices) then
    begin
      Module := ModuleServices.CurrentModule;
      CurrentFileName := Module.CurrentEditor.FileName;
    end
    else
      CurrentFileName := '';
    if CurrentFileName <> '' then
    begin
      if (not Assigned(FLiveBlameData)) or (FLiveBlameData.FFileName <> CurrentFileName) then
      begin
        FLiveBlameData := nil;
        for I := 0 to FLiveBlameDataList.Count - 1 do
          if FLiveBlameDataList[I].FFileName = CurrentFileName then
          begin
            FLiveBlameData := FLiveBlameDataList[I];
            Break;
          end;
        if not Assigned(FLiveBlameData) then
        begin
          FLiveBlameDataList.Add(TLiveBlameData.Create(CurrentFileName));
          FLiveBlameData := FLiveBlameDataList.Last;
          FLiveBlameData.FPaintBox := FPaintBox;
        end;
        FSpeedButton.Down := FLiveBlameData.FButtonDown;
        TAction(FSpeedButton.Action).Checked := FLiveBlameData.FButtonDown;
        ModuleChanged := True;
      end;
    end;
    State := ((not Assigned(FSpeedButton)) or FSpeedButton.Down);
  end;
  Visible := State;
  FSpeedButton.Visible := ButtonState;
  if Visible and ModuleChanged then
  begin
    UpdateGutterWidth;
    FPaintBox.Invalidate;
  end;
end;

procedure TLiveBlameEditorPanel.UnInstallHooks;
begin
  if FInstalledHook then
  begin
    CurrentPanel := nil;
    CurrentWindow := 0;
    EditControlHook := nil;
    UnhookWindowsHookEx(CallWndProcHook);
    UnhookWindowsHookEx(CallWndProcRetHook);
    UnhookWindowsHookEx(GetMsgHook);
    FInstalledHook := False;
  end;
end;

procedure TLiveBlameEditorPanel.UpdateGutterWidth;
var
  MaxWidthRevision, MaxLineWidth, MaxWidthUser, MaxWidthDate, CurrentWidth: Integer;
  I, NextX: Integer;
  LineInformation: TJVCSLineHistoryRevision;
  FontBitmap: TBitmap;
  OrderList: TList;
  {$IFDEF LINEINFOEX}
  MaxWidthRevisionCount, MaxWidthFirstRevision: Integer;
  LineInformationEx: TJVCSLineHistoryLineInfo;
  {$ENDIF LINEINFOEX}

  procedure CalcTextPosAndColorRect(AShowText, AShowColor: Boolean;
    AMaxTextWidth: Integer; var ACurrentX, ATextX, AColorRectX1, AColorRectX2: Integer);
  begin
    if AShowText then
    begin
      ATextX := ACurrentX + 5;
    end
    else
    begin
      AMaxTextWidth := 0;
      ATextX := -1;
    end;
    if AShowColor then
    begin
      AColorRectX1 := ACurrentX;
      AColorRectX2 := ACurrentX + AMaxTextWidth + 11;
    end
    else
    begin
      AColorRectX1 := -1;
      AColorRectX2 := -1;
    end;
    if AShowText or AShowColor then
      Inc(ACurrentX, AMaxTextWidth + 10);
  end;

begin
  MaxWidthRevision := 0;
  {$IFDEF LINEINFOEX}
  MaxWidthRevisionCount := 0;
  MaxWidthFirstRevision := 0;
  {$ENDIF LINEINFOEX}
  MaxWidthUser := 0;
  MaxWidthDate := 0;
  MaxLineWidth := 0;
  FontBitmap := TBitmap.Create;
  try
    FontBitmap.Canvas.Font.Name := 'Courier New';
    FontBitmap.Canvas.Font.Size := 10;
    if Assigned(FLiveBlameData.FLines) then
      for I := 0 to Pred(FLiveBlameData.FLines.Count) do
      begin
        LineInformation := FLiveBlameData.FLines[I];
        if Assigned(LineInformation) then
        begin
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.RevisionStr);
          if CurrentWidth > MaxWidthRevision then
            MaxWidthRevision := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.UserStr);
          if CurrentWidth > MaxWidthUser then
            MaxWidthUser := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.DateStr);
          if CurrentWidth > MaxWidthDate then
            MaxWidthDate := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(IntToStr(I + 1));
          if CurrentWidth > MaxLineWidth then
            MaxLineWidth := CurrentWidth;
        end;
      end;
    {$IFDEF LINEINFOEX}
    if Assigned(LineInformationListEx) then
      for I := 0 to Pred(LineInformationListEx.Count) do
        begin
          LineInformationEx := LineInformationListEx[I];
          if Assigned(LineInformationEx) then
          begin
            CurrentWidth := FontBitmap.Canvas.TextWidth(IntToStr(LineInformationEx.Count));
            if CurrentWidth > MaxWidthRevisionCount then
              MaxWidthRevisionCount := CurrentWidth;
            if (LineInformationEx.Count > 0) and Assigned(LineInformationEx.Revision[0]) then
            begin
              CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformationEx.Revision[0].RevisionStr);
              if CurrentWidth > MaxWidthFirstRevision then
                MaxWidthFirstRevision := CurrentWidth;
            end;
          end;
        end;
    {$ENDIF LINEINFOEX}
    NextX := 0;
    for I := 0 to Pred(FSettings.ColorBarOrderList.Count) do
      Inc(NextX, 8);
    OrderList := TList.Create;
    try
      for I := 0 to Pred(FSettings.ShowOrderList.Count) do
        OrderList.Add(FSettings.ShowOrderList[I]);
      for I := 1 to {$IFDEF LINEINFOEX} 6 {$ELSE} 4 {$ENDIF} do
        if OrderList.IndexOf(Pointer(I)) = -1 then
          OrderList.Add(Pointer(I));
      for I := 0 to Pred(OrderList.Count) do
      begin
        if OrderList[I] = Pointer(1) then
        begin
          if FSettings.ShowLineNumbers then
          begin
            FLineX := NextX + 5;
            Inc(NextX, MaxLineWidth + 10);
          end
          else
            FLineX := -1;
        end
        else
        if OrderList[I] = Pointer(2) then
          CalcTextPosAndColorRect(FSettings.ShowRevisionInfoText, FSettings.ShowRevisionInfoColor,
            MaxWidthRevision, NextX, FRevisionX, FRevisionRectX1, FRevisionRectX2)
        else
        if OrderList[I] = Pointer(3) then
          CalcTextPosAndColorRect(FSettings.ShowDateInfoText, FSettings.ShowDateInfoColor,
            MaxWidthDate, NextX, FDateX, FDateRectX1, FDateRectX2)
        else
        {$IFDEF LINEINFOEX}
        if OrderList[I] = Pointer(5) then
          CalcTextPosAndColorRect(FSettings.ShowRevisionCountInfoText, FSettings.ShowRevisionCountInfoColor,
            MaxWidthRevisionCount, NextX, FRevisionCountX, FRevisionCountRectX1, FRevisionCountRectX2)
        else
        if OrderList[I] = Pointer(6) then
          CalcTextPosAndColorRect(FSettings.ShowFirstRevisionInfoText, FSettings.ShowFirstRevisionInfoColor,
            MaxWidthFirstRevision, NextX, FFirstRevisionX, FFirstRevisionRectX1, FFirstRevisionRectX2)
        else
        {$ENDIF LINEINFOEX}
        if OrderList[I] = Pointer(4) then
          CalcTextPosAndColorRect(FSettings.ShowUserInfoText, FSettings.ShowUserInfoColor,
            MaxWidthUser, NextX, FUserX, FUserRectX1, FUserRectX2);
      end;
    finally
      OrderList.Free;
    end;
    Width := NextX;
  finally
    FontBitmap.Free;
  end;
end;

procedure BuildDiff(ALines1, ALines2: TStringList; ANewObject: TObject; ADeletedLines1, ADeletedLines2: TObjectList<TDeletedLines>; ANewDel: Integer);
var
  HashList1, HashList2: TList;
  I, J, K, L: Integer;
  Diff: TDiff;
begin
  HashList1 := TList.Create;
  HashList2 := TList.Create;
  Diff := TDiff.Create(nil);
  try
    for I := 0 to ALines1.Count - 1 do
      HashList1.Add(HashLine(ALines1[I], True, True));
    for I := 0 to ALines2.Count - 1 do
      HashList2.Add(HashLine(ALines2[I], True, True));
    while ADeletedLines1.Count < HashList1.Count do
      ADeletedLines1.Add(nil);
    while ADeletedLines2.Count < HashList2.Count do
      ADeletedLines2.Add(nil);
    Diff.Execute(PIntArray(HashList1.List),PIntArray(HashList2.List),
      HashList1.count, HashList2.count);
    J := 0;
    K := 0;
    with Diff do
      for I := 0 to ChangeCount-1 do
        with Changes[I] do
        begin
          while J < x do
          begin
            ALines2.Objects[K] := Alines1.Objects[J];
            if Assigned(ADeletedLines1[J]) then
            begin
              if not Assigned(ADeletedLines2[K]) then
                ADeletedLines2[K] := TDeletedLines.Create;
              ADeletedLines2[K].Ver := ADeletedLines2[K].Ver or ADeletedLines1[J].Ver;
              for L := 0 to ADeletedLines1[J].Items.Count - 1 do
              begin
                ADeletedLines2[K].Items.Add(TStringList.Create);
                ADeletedLines2[K].Items.Last.Assign(ADeletedLines1[J].Items[L]);
              end;
            end;
            Inc(J);
            Inc(K);
          end;
          if Kind = ckAdd then
          begin
            for J := K to K + Range - 1 do
              Alines2.Objects[J] := ANewObject;
            J := x;
            K := y + Range;
          end
          else
          if Kind = ckModify then
          begin
            for J := 0 to Range - 1 do
              Alines2.Objects[K + J] := ANewObject;
            J := x + Range;
            K := y + Range;
          end
          else //Kind = ckDel
          begin
            J := x + Range;
            if not Assigned(ADeletedLines2[K]) then
              ADeletedLines2[K] := TDeletedLines.Create;
            ADeletedLines2[K].Ver := ANewDel;
            ADeletedLines2[K].Items.Add(TStringList.Create);
            for L := x to x + Range - 1 do
              ADeletedLines2[K].Items.Last.Add(ALines1[L]);
          end;
        end;
    while J < Alines1.count do
    begin
      Alines2.Objects[K] := Alines1.Objects[J];
      if Assigned(ADeletedLines1[J]) then
      begin
        if not Assigned(ADeletedLines2[K]) then
          ADeletedLines2[K] := TDeletedLines.Create;
        ADeletedLines2[K].Ver := ADeletedLines2[K].Ver or ADeletedLines1[J].Ver;
        for L := 0 to ADeletedLines1[J].Items.Count - 1 do
        begin
          ADeletedLines2[K].Items.Add(TStringList.Create);
          ADeletedLines2[K].Items.Last.Assign(ADeletedLines1[J].Items[L]);
        end;
      end;
      Inc(J);
      Inc(K);
    end;
  finally
    Diff.Free;
    HashList2.Free;
    HashList1.Free;
  end;
end;

type
  TLineUpdateThread = class(TThread)
  private
    FFileName: string;
    FLatestRevisionContent: RawByteString;
    FEditorContent: RawByteString;
    FLines: TList<TJVCSLineHistoryRevision>;
    FOrgLines: TList<TJVCSLineHistoryRevision>;
    FFileRevision: TJVCSLineHistoryRevision;
    FBufferRevision: TJVCSLineHistoryRevision;
    FDeletedLines2: TObjectList<TDeletedLines>;
  protected
    procedure Execute; override;
  public
    constructor Create(AFileName: string; ALatestRevisionContent, AEditorContent: RawByteString; AOnFinished: TNotifyEvent;
      AOrgLines: TList<TJVCSLineHistoryRevision>; AFileRevision, ABufferRevision: TJVCSLineHistoryRevision;
      ADeletedLines2: TObjectList<TDeletedLines>);
    destructor Destroy; override;
  end;

{ TLineUpdateThread }

constructor TLineUpdateThread.Create(AFileName: string; ALatestRevisionContent, AEditorContent: RawByteString; AOnFinished: TNotifyEvent;
  AOrgLines: TList<TJVCSLineHistoryRevision>; AFileRevision, ABufferRevision: TJVCSLineHistoryRevision;
  ADeletedLines2: TObjectList<TDeletedLines>);
begin
  inherited Create(False);
  FFileName := AFileName;
  FLatestRevisionContent := ALatestRevisionContent;
  FEditorContent := AEditorContent;
  FreeOnTerminate := True;
  OnTerminate := AOnFinished;
  FLines := TList<TJVCSLineHistoryRevision>.Create;
  FFileRevision := AFileRevision;
  FBufferRevision := ABufferRevision;
  FOrgLines := AOrgLines;
  FDeletedLines2 := ADeletedLines2;
end;

destructor TLineUpdateThread.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TLineUpdateThread.Execute;
var
  I: Integer;
  TSL, TSL2, SL: TStringList;
  DT: TDateTime;
  DeletedLines1, DeletedLines2: TObjectList<TDeletedLines>;
  MS: TMemoryStream;
begin
  TSL := TStringList.Create;
  TSL2 := TStringList.Create;
  SL := TStringList.Create;
  DeletedLines1 := TObjectList<TDeletedLines>.Create;
  DeletedLines2 := TObjectList<TDeletedLines>.Create;
  try
    TSL.LoadFromFile(FFileName);
    FileAge(FFileName, DT);
    FFileRevision.FDate := DT - 1 / 86400;
    {
    TSL2.LoadFromFile(ExtractFilePath(FFileName) + '.svn\text-base\' + ExtractFileName(FFileName) + '.svn-base');//TODO:remove
    }
    //TSL2.Text := UTF8ToString(FLatestRevisionContent);
    MS := TMemoryStream.Create;
    try
      MS.Write(FLatestRevisionContent[1], Length(FLatestRevisionContent));
      MS.Position := 0;
      TSL2.LoadFromStream(MS);
    finally
      MS.Free;
    end;
    for I := 0 to TSL2.Count - 1 do
      if FOrgLines.Count > I then
        TSL2.Objects[I] := FOrgLines[I];
    BuildDiff(TSL2, TSL, TObject(2), DeletedLines1, DeletedLines2, 2);
    SL.Text := UTF8ToString(FEditorContent);
    FDeletedLines2.Clear;
    BuildDiff(TSL, SL, TObject(1), DeletedLines2, FDeletedLines2, 1);
    FLines.Clear;
    for I := 0 to SL.Count - 1 do
      if SL.Objects[I] = TObject(1) then
        FLines.Add(FBufferRevision)
      else
      if SL.Objects[I] = TObject(2) then
        FLines.Add(FFileRevision)
      else
        FLines.Add(TJVCSLineHistoryRevision(SL.Objects[I]));
  finally
    DeletedLines1.Free;
    DeletedLines2.Free;
    SL.Free;
    TSL2.Free;
    TSL.Free;
  end;
end;

procedure TLiveBlameEditorPanel.HandleDiffThreadReady(Sender: TObject);
begin
  FLiveBlameData.FLines.Clear;
  FLiveBlameData.FLines.AddRange(TLineUpdateThread(Sender).FLines);
  FPaintBox.Invalidate;
end;

procedure TLiveBlameEditorPanel.UpdateLineHistory(ASourceEditor: IOTASourceEditor);
var
  //I, Idx: Integer;
  EC: IOTAEditorContent;
  C: IStream;
  S: RawByteString;
  R, W: Int64;
  MS: TMemoryStream;
  SA: TStreamAdapter;
  //TSL, TSL2, SL: TStringList;
  StreamStat: TStatStg;
  Update: Boolean;
  //DT: TDateTime;
begin
  EC := ASourceEditor as IOTAEditorContent;
  Update := (FLiveBlameData.FLastAge = 0) or (EC.GetContentAge <> FLiveBlameData.FLastAge);
  C := EC.Content;
  if C.Stat(StreamStat, 0) = S_OK then
    Update := FLiveBlameData.FLastStreamSize <> StreamStat.cbSize;
  Update := Update or ForceUpdate;
  ForceUpdate := False;
  if Update then
  begin
    FLiveBlameData.FLastAge := EC.GetContentAge;
    FLiveBlameData.FLastStreamSize := StreamStat.cbSize;
    FLiveBlameData.FBufferRevision.FDate := Now;//FLastAge + 1 / 24 - 1 / 86400;
    FLiveBlameData.FBufferRevision.FDateStr := GetDateStr(FSettings.DateFormat, FLiveBlameData.FLastAge);
    MS := TMemoryStream.Create;
    try
      SA := TStreamAdapter.Create(MS);
      try
        C.Seek(0, 0, R);
        C.CopyTo(SA, FLiveBlameData.FLastStreamSize, R, W);
        MS.Position := 0;
        SetLength(S, MS.Size);
        MS.Read(PAnsiChar(S)^, MS.Size);
      finally
        //SA.Free;
      end;

      TLineUpdateThread.Create(ASourceEditor.FileName, FLiveBlameData.FLatestRevisionContent, S,
        HandleDiffThreadReady, FLiveBlameData.FOrgLines, FLiveBlameData.FFileRevision, FLiveBlameData.FBufferRevision,
        FLiveBlameData.FDeletedLines);
      {//TODO:remove
      TSL := TStringList.Create;
      TSL2 := TStringList.Create;
      SL := TStringList.Create;
      try
        TSL.LoadFromFile(ASourceEditor.FileName);
        FileAge(ASourceEditor.FileName, DT);
        FFileRevision.FDate := DT - 1 / 86400;
        TSL2.LoadFromFile(ExtractFilePath(ASourceEditor.FileName) + '.svn\text-base\' + ExtractFileName(ASourceEditor.FileName) + '.svn-base');
        for I := 0 to TSL2.Count - 1 do
          if FOrgLines.Count > I then
            TSL2.Objects[I] := FOrgLines[I];
        BuildDiff(TSL2, TSL, TObject(2));
        SL.Text := UTF8ToString(S);
        BuildDiff(TSL, SL, TObject(1));
        FLines.Clear;
        for I := 0 to SL.Count - 1 do
          if SL.Objects[I] = TObject(1) then
            FLines.Add(FBufferRevision)
          else
          if SL.Objects[I] = TObject(2) then
            FLines.Add(FFileRevision)
          else
            FLines.Add(TJVCSLineHistoryRevision(SL.Objects[I]));
      finally
        SL.Free;
        TSL2.Free;
        TSL.Free;
      end;
      }
    finally
      MS.Free;
    end;
  end;
end;

function GetModificationColor(ABackgroundColor: Boolean; ADefaultColor: TColor): TColor;
var
  Key, Ident, ColorStr: string;
  RegIniFile: TRegIniFile;
begin
  Result := ADefaultColor;
  Key := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Editor\Highlight';
  RegIniFile := TRegIniFile.Create(Key);
  try
    if ABackgroundColor then
      Ident := 'Background Color New'
    else
      Ident := 'Foreground Color New';
    ColorStr := RegIniFile.ReadString('Modified line', Ident, '');
    if ColorStr <> '' then
      try
        Result := StringToColor(ColorStr);
      except
      //
      end;
  finally
    RegIniFile.Free;
  end;
end;

function TLiveBlameEditorPanel.UpdateModificationColors: Boolean;
var
  OldColor: TColor;
begin
  OldColor := FModificationColorFile;
  FModificationColorFile := GetModificationColor(False, clLime);
  Result := OldColor <> FModificationColorFile;
  OldColor := FModificationColorBuffer;
  FModificationColorBuffer := GetModificationColor(True, clYellow);
  Result := Result or (OldColor <> FModificationColorBuffer);
end;

procedure TLiveBlameEditorPanel.EnableCheckTimer;
begin
  FCheckShowTimer.Enabled := True;
end;

procedure TLiveBlameEditorPanel.WMMyShow(var Msg: TMessage);
begin
  ShowIfEditControl;
end;

procedure TLiveBlameEditorPanel.WMUpdateWnd(var Msg: TMessage);
begin
  PaintBoxPaint(nil);
end;

type
  TIdeNotifier = class(TNotifierObject,IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50,IOTAEditorNotifier)
  private
    FList: TList;
    FWizard: TLiveBlameWizard;
  protected
    constructor Create(AWizard: TLiveBlameWizard; AList: TList);
    procedure AfterCompile(Succeeded: Boolean);overload;
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean);overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean);overload;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);
  end;

  TMyModuleNotifier = class(TNotifierObject, IOTANotifier, IOTAModuleNotifier,IOTAEditorNotifier)
  private
    FInfo: string;
    FList: TList;
    FWizard: TLiveBlameWizard;
    FModule: IOTAModule;
  public
    constructor Create(AModule: IOTAModule; AWizard: TLiveBlameWizard; AList: TList; AInfo: string);
    procedure ModuleRenamed(const NewName: string);
    function CheckOverwrite: Boolean;

    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    property Module: IOTAModule read FModule;
  end;

  TMyINTANotifier = class(TNotifierObject, INTAEditServicesNotifier)
  private
    FList: TList;
    FWizard: TLiveBlameWizard;
  public
    constructor Create(AWizard: TLiveBlameWizard; AList: TList);
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

{$IFDEF DEBUGMESSAGES}
function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;
{$ENDIF DEBUGMESSAGES}

constructor TIdeNotifier.Create(AWizard: TLiveBlameWizard; AList: TList);
begin
  inherited Create;
  FList := AList;
  FWizard := AWizard;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I, J: Integer;
  Editor: IOTAEditor;
  InfoStr: string;
  Dummy: IUnknown;
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddTitleMessage(Format('%s: %s',
    [GetEnumName(TypeInfo(TOTAFIleNotification), Ord(NotifyCode)), FileName]));
  MsgServices.AddToolMessage('',Format('%s: %s',
    [GetEnumName(TypeInfo(TOTAFIleNotification), Ord(NotifyCode)), FileName]), 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}

  if NotifyCode = ofnFileOpened then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    if Assigned(ModuleServices) then
      for I := 0 to Pred(ModuleServices.ModuleCount) do
      begin
        Module := ModuleServices.Modules[I];
        if Assigned(Module) and SameText(Module.FileName, FileName) then
        begin
          Module.AddNotifier(TMyModuleNotifier.Create(Module, FWizard, FList, ' ModuleNotifier[' +  FileName + ']'));
          {$IFDEF DEBUGMESSAGES}
          MsgServices.AddToolMessage('','FileNotification added Notifier', 'Notifier',0,0);
          {$ENDIF DEBUGMESSAGES}
          for J := 0 to Pred(Module.GetModuleFileCount) do
          begin
            Editor := Module.GetModuleFileEditor(J);
            InfoStr := FileName;
            if Supports(Editor, IOTASourceEditor, Dummy) then
              InfoStr := InfoStr + ', IOTASourceEditor';
            if Supports(Editor, IOTAFormEditor, Dummy) then
              InfoStr := InfoStr + ', IOTAFormEditor';
            if Supports(Editor, IOTAProjectResource, Dummy) then
              InfoStr := InfoStr + ', IOTAProjectResource';
            if Supports(Editor, IOTATypeLibEditor, Dummy) then
              InfoStr := InfoStr + ', IOTATypeLibEditor';
            Editor.AddNotifier(TMyModuleNotifier.Create(Module, FWizard, FList, ' ModuleEditorNotifier[' +  InfoStr + ']'));
            {$IFDEF DEBUGMESSAGES}
            MsgServices.AddToolMessage('','FileNotification added EditorNotifier', 'Notifier',0,0);
            {$ENDIF DEBUGMESSAGES}
          end;
        end;
      end;
  end;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
begin

end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewNotification', 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}
end;

procedure TIdeNotifier.ViewActivated(const View: IOTAEditView);
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewActivated', 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}
end;

constructor TMyModuleNotifier.Create(AModule: IOTAModule; AWizard: TLiveBlameWizard; AList: TList; AInfo: string);
begin
  inherited Create;
  FList := AList;
  FInfo := AInfo;
  FWizard := AWizard;
  FModule := AModule;
end;

procedure TMyModuleNotifier.AfterSave;
begin
  ForceUpdate := True;
  //PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0);
  if Assigned(CurrentPanel) then
    CurrentPanel.PaintBoxPaint(nil);
end;

procedure TMyModuleNotifier.BeforeSave;
begin

end;

procedure TMyModuleNotifier.Destroyed;
begin

end;

procedure TMyModuleNotifier.Modified;
begin

end;

constructor TMyINTANotifier.Create(AWizard: TLiveBlameWizard; AList: TList);
begin
  inherited Create;
  FList := AList;
  FWizard := AWizard;
end;

procedure TMyINTANotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  OutputDebugString('TMyINTANotifier.WindowShow');
end;

procedure TMyINTANotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  OutputDebugString('TMyINTANotifier.WindowNotification');
end;

procedure TMyINTANotifier.WindowActivated(const EditWindow: INTAEditWindow);
var
  I: Integer;
begin
  OutputDebugString(PChar(Format('TMyINTANotifier.WindowActivated', [])));
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));      
    end;
end;

procedure TMyINTANotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
begin
  OutputDebugString('TMyINTANotifier.WindowCommand');
end;

function CheckAddPanel(AWizard: TLiveBlameWizard; View: IOTAEditView; AList: TList): TLiveBlameEditorPanel; forward;
function GetEditViewModule(AEditView: IOTAEditView): IOTAModule; forward;

procedure TMyINTANotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
var
  I: Integer;
  EP: TLiveBlameEditorPanel;
begin
  OutputDebugString('TMyINTANotifier.EditorViewActivated');
  EP := CheckAddPanel(FWizard, EditView, FList);
  EP.ActiveEditView1 := EditView;
  EP.ActiveModule := GetEditViewModule(EditView);
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));
      TLiveBlameEditorPanel(FList[I]).EnableCheckTimer;
    end;
end;

procedure TMyINTANotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  OutputDebugString('TMyINTANotifier.EditorViewModified');
end;

procedure TMyINTANotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  OutputDebugString('TMyINTANotifier.DockFormVisibleChanged');
end;

procedure TMyINTANotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
var
  I: Integer;
begin
  OutputDebugString('TMyINTANotifier.DockFormUpdated');
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));
      TLiveBlameEditorPanel(FList[I]).EnableCheckTimer;
      PostMessage(TLiveBlameEditorPanel(FList[I]).Handle, WM_BLAME_SHOW, 0, 0);
    end;
end;

procedure TMyINTANotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  OutputDebugString('TMyINTANotifier.DockFormRefresh');
end;

procedure AddBlameButton(EditorPanel: TLiveBlameEditorPanel);
var
  tempComponent, P, C, ImgList: TComponent;
  BlameButtonPanel: TPanel;
  SpeedButton: TSpeedButton;
  ActionLst: TActionList;
  Action: TAction;
  I, X: Integer;
begin
  tempComponent := Application.FindComponent('EditWindow_0');
  if Assigned(tempComponent) then
  begin
    P := tempComponent.FindComponent('StatusBar');
    X := 0;
    if Assigned(P) then
    begin
      {
      if (TStatusBar(P).Panels.Count > 3) and (TStatusBar(P).Panels[3].Width < 80) then
        TStatusBar(P).Panels[3].Width := TStatusBar(P).Panels[3].Width + 32;
      }
      if (TStatusBar(P).Panels.Count > 3) and (TStatusBar(P).Panels[3].Width < 80 + 84) then
        TStatusBar(P).Panels[3].Width := TStatusBar(P).Panels[3].Width + 32 + 84;//TODO:84?
      if (TStatusBar(P).Panels.Count > 3) then
      begin
        for I := 0 to 2 do
          X := X + TStatusBar(P).Panels[I].Width;
        X := X + 75;
        X := X + 84;//TODO:84?
      end;
    end;
    tempComponent := tempComponent.FindComponent('Panel2');
    if Assigned(tempComponent) then
    begin
      //{
      P := tempComponent.FindComponent('BlameButtonPanel');
      if not Assigned(P) then
      begin
        BlameButtonPanel := TPanel.Create(tempComponent);
        BlameButtonPanel.Align := alNone;
        BlameButtonPanel.Width := 32;
        BlameButtonPanel.Parent := TWinControl(tempComponent);
        BlameButtonPanel.Name := 'BlameButtonPanel';
        BlameButtonPanel.Left := X;
        BlameButtonPanel.Top := 1;
        BlameButtonPanel.Height := 20;
        BlameButtonPanel.Visible := True;
        BlameButtonPanel.Caption := '';
        BlameButtonPanel.BevelOuter := bvNone;
        BlameButtonPanel.ParentBackground := False;


        ImgList := nil;
        I := 0;
        while (I < Screen.FormCount) do
        begin
          C := Screen.Forms[I].FindComponent('FileHistoryFrame');
          if Assigned(C) then
          begin
            ImgList := C.FindComponent('ImageList1');
            Break;
          end;
          Inc(I);
        end;
        ActionLst := TActionList.Create(BlameButtonPanel);
        ActionLst.Images := TImageList(ImgList);
        Action := TAction.Create(BlameButtonPanel);
        Action.Name := 'BlameEditorAction';
        Action.ImageIndex := 16;
        Action.Caption := 'Show Blame';
        Action.Hint := 'Show Blame';
        Action.ActionList := ActionLst;
        Action.OnExecute := EditorPanel.ShowHidePanel;
        Action.AutoCheck := False;//AutoCheck requires GroupIndex to be 0, it is mimicked in ShowHidePanel
        Action.GroupIndex := 123;

        SpeedButton := TSpeedButton.Create(BlameButtonPanel);
        SpeedButton.Name := 'BlameSpeedButton';
        SpeedButton.Parent := BlameButtonPanel;
        SpeedButton.Left := 0;
        SpeedButton.Top := 0;
        SpeedButton.Height := 20;
        SpeedButton.Width := 20;
        SpeedButton.Flat := True;
        SpeedButton.Action := Action;
        SpeedButton.Caption := '';
        SpeedButton.AllowAllUp := True;
        SpeedButton.OnClick := EditorPanel.ShowHidePanel;
        P := BlameButtonPanel;
      end
      else
      if not TPanel(P).Visible then
        TPanel(P).Visible := True
      else
        TPanel(P).BringToFront;

      P := P.FindComponent('BlameSpeedButton');
      if Assigned(P) then
      begin
        TSpeedButton(P).Down := EditorPanel.Visible or
          (Assigned(EditorPanel.FLiveBlameData) and EditorPanel.FLiveBlameData.FButtonDown);
        EditorPanel.FSpeedButton := TSpeedButton(P);
      end;
    end;
  end;
end;

procedure RemoveBlamePanel;
var
  tempComponent, P: TComponent;
begin
  tempComponent := Application.FindComponent('EditWindow_0');
  if Assigned(tempComponent) then
  begin
    tempComponent := tempComponent.FindComponent('Panel2');
    if Assigned(tempComponent) then
    begin
      P := tempComponent.FindComponent('BlameButtonPanel');
      P.Free;
    end;
  end;
end;

function CheckAddPanel(AWizard: TLiveBlameWizard; View: IOTAEditView; AList: TList): TLiveBlameEditorPanel;
var
  EW: INTAEditWindow;
  EF: TCustomForm;
  I, Idx: Integer;
  EP: TLiveBlameEditorPanel;
  EFEP: TWinControl;
  OStr: string;
begin
  Result := nil;
  if Assigned(View) then
  begin
    EW := View.GetEditWindow;
    EF := EW.GetForm;
    if Assigned(EF) then
    begin
      OStr := OStr + ', ' + EF.Name;
      EP := nil;
      if Assigned(AList) and Assigned(EF) then
      begin
        Idx := -1;
        for I := 0 to Pred(AList.Count) do
          if Assigned(AList[I]) and (TLiveBlameEditorPanel(AList[I]).Owner = EF) then
          begin
            Idx := I;
            Break;
          end;
        if Idx = -1 then
        begin
          EP := TLiveBlameEditorPanel.Create(EF);
          AList.Add(EP);
          EP.Wizard := AWizard;
          EFEP := TWinControl(EF.FindComponent('EditorPanel'));
          for I := 0 to EF.ComponentCount - 1 do
            if (EF.Components[I] is TWinControl) and
              (EF.Components[I].ClassName = 'TEditControl') then
            begin
              EFEP := TWinControl(EF.Components[I]).Parent;
              OutputDebugString(PChar('Found TEditControl.Parent: ' + EFEP.ClassName + ' ' + EFEP.Name));
              Break;
            end;
          EP.Parent := EFEP;
        end
        else
          EP := TLiveBlameEditorPanel(AList[Idx]);
        Result := EP;
        //EP.ShowIfEditControl;
      end;
      if Assigned(EP) then
        AddBlameButton(EP);//TODO:?
    end;
  end;
end;

procedure TMyModuleNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
var
  OStr: string;
begin
  if Operation = opRemove then
    OStr := 'opRemove'
  else
    OStr := 'opInsert';

  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewNotification[' + OStr + ']' + FInfo, 'MyModuleNotifier',0,0);
  {$ENDIF DEBUGMESSAGES}
  OutputDebugString(PChar('ViewNotification[' + OStr + ']' + FInfo));
  {//TODO:?
  if Operation = opInsert then
    CheckAddPanel(FWizard, View, FList);
  }
end;

procedure TMyModuleNotifier.ViewActivated(const View: IOTAEditView);
var
  OStr: string;
  EW: INTAEditWindow;
  EP: TLiveBlameEditorPanel;
begin
  if Assigned(View) then
  begin
    EW := View.GetEditWindow;
    if Assigned(EW.GetForm) then
      OStr := OStr + ', ' + EW.GetForm.Name;
  end;
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewActivated[' + OStr + ']' + FInfo, 'MyModuleNotifier',0,0);
  {$ENDIF DEBUGMESSAGES}
  OutputDebugString(PChar('ViewActivated[' + OStr + ']' + FInfo));
  EP := CheckAddPanel(FWizard, View, FList);
  if Assigned(EP) then
  begin
    OutputDebugString(PChar(Format('ViewActivated EP.ActiveView=%p bf', [Pointer(View)])));
    EP.ActiveEditView1 := View;
    EP.ActiveModule := FModule;
    OutputDebugString(PChar(Format('ViewActivated EP.ActiveView=%p af', [Pointer(View)])));
  end
  else
    OutputDebugString(PChar('ViewActivated EP not assigned'));
end;

procedure TMyModuleNotifier.ModuleRenamed(const NewName: string);
begin
end;

function TMyModuleNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

const
  DiffFontColor = $CC9999;
  DiffBackGroundColor = $F4F4F4;
  DiffColorBarLineColor = clLime;

procedure TLiveBlameEditorPanel.OnTimer(Sender: TObject);
begin
  FPaintBox.Invalidate;
end;

function GetEditViewModule(AEditView: IOTAEditView): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  I, J, K: Integer;
  SourceEdit: IOTASourceEditor;
  Found: Boolean;
begin
  Result := nil;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if Assigned(ModuleServices) and Assigned(AEditView) then
  begin
    Found := False;
    for I := 0 to Pred(ModuleServices.ModuleCount) do
    begin
      if Supports(ModuleServices.Modules[I].CurrentEditor, IOTASourceEditor, SourceEdit) then
      begin
        for J := 0 to Pred(SourceEdit.EditViewCount) do
          if SourceEdit.EditViews[J] = AEditView then
          begin
            Found := True;
            Break;
          end;
      end;
      if not Found then
      begin
        for K := 0 to Pred(ModuleServices.Modules[I].GetModuleFileCount) do
        begin
          if Supports(ModuleServices.Modules[I].GetModuleFileEditor(K), IOTASourceEditor, SourceEdit) then
            for J := 0 to Pred(SourceEdit.EditViewCount) do
              if SourceEdit.EditViews[J] = AEditView then
              begin
                Found := True;
                Break;
              end;
          if Found then
            Break;
        end;
      end;
      if Supports(ModuleServices.Modules[I].CurrentEditor, IOTASourceEditor, SourceEdit) then
      begin
        for J := 0 to Pred(SourceEdit.EditViewCount) do
        begin
          if SourceEdit.EditViews[J].SameView(AEditView) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
      if not Found then
      begin
        for K := 0 to Pred(ModuleServices.Modules[I].GetModuleFileCount) do
        begin
          if Supports(ModuleServices.Modules[I].GetModuleFileEditor(K), IOTASourceEditor, SourceEdit) then
            for J := 0 to Pred(SourceEdit.EditViewCount) do
            begin
              if SourceEdit.EditViews[J].SameView(AEditView) then
              begin
                Found := True;
                Break;
              end;
            end;
          if Found then
            Break;
        end;
      end;

      if Found then
      begin
        Result := ModuleServices.Modules[I];
        Break;
      end;
    end;
  end;
end;

function GetEditViewFilename(AEditView: IOTAEditView): string;
var
  Module: IOTAModule;
begin
  Module := GetEditViewModule(AEditView);
  if Assigned(Module) then
    Result := ExtractFileName(Module.FileName)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

const
  {$IFDEF VER220}
  coreide = 'coreide150.bpl';
  {$ENDIF VER220}
  {$IFDEF VER230}
  coreide = 'coreide160.bpl';
  {$ENDIF VER230}
  SLineIsElidedName = '@Editorcontrol@TCustomEditControl@LineIsElided$qqri';

function LineIsElided(Self: TObject; LineNum: Integer): Boolean; external coreide name SLineIsElidedName;

//------------------------------------------------------------------------------

type
  TPaintBlock = class(TObject)
  private
    FStartRow: Integer;
    FEndRow: Integer;
    FViewStartRow: Integer;
    FViewEndRow: Integer;
  public
    constructor Create(AStartRow, AViewStartRow: Integer);
    property StartRow: Integer read FStartRow;
    property EndRow: Integer read FEndRow;
    property ViewStartRow: Integer read FViewStartRow;
    property ViewEndRow: Integer read FViewEndRow;
  end;

{ TPaintBlock }

constructor TPaintBlock.Create(AStartRow, AViewStartRow: Integer);
begin
  FStartRow := AStartRow;
  FViewStartRow := AViewStartRow;
  FEndRow := -1;
  FViewEndRow := -1;
end;

//------------------------------------------------------------------------------

procedure TLiveBlameEditorPanel.PaintBoxPaint(Sender: TObject);

var
  LH: Integer;

type
  TBlockType = (btRevision, btDate, btUser);

  procedure PaintBlocksX(ADestCanvas: TCanvas; ABlockType: TBlockType; ARectX1, ARectX2, ATextX: Integer; APaintBlock: TPaintBlock);

    function IsSameInfo(AInfo1, AInfo2: TJVCSLineHistoryRevision): Boolean;
    begin
      if FSettings.PaintMethod = 0 then
        Result := False
      else
      if FSettings.PaintMethod = 1 then
      begin
        Result := AInfo1 = AInfo2;
        if ABlockType <> btRevision then
        begin
          if Assigned(AInfo1) and Assigned(AInfo2) then
          begin
            case ABlockType of
              btDate: Result := AInfo1.DateStr = AInfo2.DateStr;
              btUser: Result := AInfo1.UserStr = AInfo2.UserStr;
              else
                Result := False;
            end;
          end
          else
            Result := False;
        end;
      end
      else
      if FSettings.PaintMethod = 2 then
      begin
        Result := AInfo1 = AInfo2;
        if Result then
        begin
          if Assigned(AInfo1) and Assigned(AInfo2) then
          begin
            if Result then
              Result := AInfo1.DateStr = AInfo2.DateStr;
            if Result then
              Result := AInfo1.UserStr = AInfo2.UserStr;
          end
          else
            Result := AInfo1 <> AInfo2;
        end;
      end
      else
        Result := False;
    end;

    function GetColor(AInfo: TJVCSLineHistoryRevision): TColor;
    var
      RevisionColor: TRevisionColor;
    begin
      RevisionColor := DoGetRevisionColor(AInfo);
      case ABlockType of
        btRevision: Result := RevisionColor.RevisionColor;
        btDate: Result := RevisionColor.DateColor;
        btUser: Result := RevisionColor.UserColor;
        else
          Result := clBtnFace;
      end;
    end;

  var
    LastBlockRevision: TJVCSLineHistoryRevision;
    LastBlockStartY, LastBlockEndY{, LH}, LastBlockStartLine, LastBlockEndLine: Integer;

    procedure PaintLastBlock;
    var
      I: Integer;
      S, RevisionIDStr: string;
      RevisionRect, RevisionHintRect: TRect;
      RevisionTextExtent: TSize;
      OldFontColor: TColor;
      OldFontStyle: TFontStyles;
      OldFont: TFont;
    begin
      if (LastBlockEndY > -1) and Assigned(LastBlockRevision) then
      begin
        ADestCanvas.Brush.Style := bsSolid;
        ADestCanvas.Brush.Color := GetColor(LastBlockRevision);
        ADestCanvas.Rectangle(ARectX1, LastBlockStartY, ARectX2, LastBlockEndY);
        if ATextX <> -1 then
        begin
          ADestCanvas.Brush.Style := bsClear;
          case ABlockType of
            btRevision: S := LastBlockRevision.RevisionStr;
            btDate: S := LastBlockRevision.DateStr;
            btUser: S := LastBlockRevision.UserStr;
            else
              S := '';
          end;
          if ABlockType = btRevision then
          begin
            OldFontColor := ADestCanvas.Font.Color;
            OldFontStyle := ADestCanvas.Font.Style;
            try
              if (LastBlockStartY + LastBlockEndY - LH) shr 1 = FHighlightY then
              begin
                ADestCanvas.Font.Color := clHighlight;
                ADestCanvas.Font.Style := [fsUnderline];
              end;
              ADestCanvas.TextOut(ATextX, (LastBlockStartY + LastBlockEndY - LH) shr 1, S);
              RevisionTextExtent := ADestCanvas.TextExtent(S);
              RevisionRect.Left := ATextX;
              RevisionRect.Right := RevisionRect.Left + RevisionTextExtent.cx;
              RevisionRect.Top := (LastBlockStartY + LastBlockEndY - LH) shr 1;
              RevisionRect.Bottom := RevisionRect.Top + RevisionTextExtent.cy;
              {
              if LastBlockRevision.RevisionID > 0 then
                RevisionIDStr := IntToStr(LastBlockRevision.RevisionID)
              else
                RevisionIDStr := LastBlockRevision.OrgRevisionStr;
              }
              RevisionIDStr := S;
              FRevisionRectangles.Add(RevisionRect, RevisionIDStr);
            finally
              ADestCanvas.Font.Color := OldFontColor;
              ADestCanvas.Font.Style := OldFontStyle;
            end;
          end
          else
            ADestCanvas.TextOut(ATextX, (LastBlockStartY + LastBlockEndY - LH) shr 1, S);
        end;
        //TODO: check if rectangle is added multiple times
        RevisionHintRect.Left := ARectX1;
        RevisionHintRect.Right := ARectX2;
        if FDateRectX2 > RevisionHintRect.Right then
          RevisionHintRect.Right := FDateRectX2;
        if FUserRectX2 > RevisionHintRect.Right then
          RevisionHintRect.Right := FUserRectX2;
        RevisionHintRect.Top := LastBlockStartY;
        RevisionHintRect.Bottom := LastBlockEndY;
        FRevisionHintRectangles.Add(RevisionHintRect, LastBlockRevision.RevisionStr);

        for I := LastBlockStartLine to LastBlockEndLine do
          if (FLiveBlameData.FDeletedLines.Count > I) and Assigned(FLiveBlameData.FDeletedLines[I]) and (FLiveBlameData.FDeletedLines[I].Ver <> 0) then
          begin
            OldFont := TFont.Create;
            OldFont.Assign(ADestCanvas.Font);
            try
              ADestCanvas.Brush.Style := bsClear;
              ADestCanvas.Font.Size := 12;
              if FLiveBlameData.FDeletedLines[I].Ver and 2 <> 0 then
                ADestCanvas.Font.Color := clBlack
              else
                ADestCanvas.Font.Color := clGray;
              ADestCanvas.TextOut(1, LastBlockStartY + (I - LastBlockStartLine) * LH - 8, #9658);
              FDeletedLinesHintTriangles.Add(Point(1, LastBlockStartY + (I - LastBlockStartLine) * LH - 8 + 4), FLiveBlameData.FDeletedLines[I]);
            finally
              ADestCanvas.Font.Assign(OldFont);
              OldFont.Free;
            end;
          end;
      end;
    end;

  var
    Revision: TJVCSLineHistoryRevision;
    PaintLine, RealLine, Y: Integer;
  begin
    if (ARectX1 <> -1) or (ATextX <> -1) and (FLiveBlameData.FLines.Count > 0) then
    begin
      //LH := FSynEdit.LineHeight;
      LastBlockStartY := -1;
      LastBlockEndY := -1;
      LastBlockStartLine := -1;
      LastBlockEndLine := -1;
      LastBlockRevision := nil;
      for PaintLine := APaintBlock.FViewStartRow to APaintBlock.FViewEndRow do
      begin
        RealLine := PaintLine - APaintBlock.FViewStartRow + APaintBlock.FStartRow - 1;
        Y := LH * (PaintLine - 1);
        if RealLine < FLiveBlameData.FLines.Count then
        begin
          Revision := FLiveBlameData.FLines[RealLine];
          if not (IsSameInfo(LastBlockRevision, Revision) {and not IsDifferent(PaintLine)}) then//TODO:IsDifferent
          begin
            PaintLastBlock;
            LastBlockStartY := Y;
            LastBlockStartLine := RealLine;
            LastBlockRevision := Revision;
          end;
          LastBlockEndY := Y + LH + 1;
          LastBlockEndLine := RealLine;
        end;
      end;
      PaintLastBlock;
    end;
  end;

var
  I, EH, {LH, }LN: Integer;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  CurrentEditor: IOTAEditor;
  SourceEdit: IOTASourceEditor;
  Canvas: TCanvas;
  Bitmap: TBitmap;
  EditControl: TObject;
  Elided: Boolean;
  LastRow, BottomRow: Integer;
  PaintBlocks: TObjectList<TPaintBlock>;
  PaintBlock: TPaintBlock;
  FileName: string;
  EditViewRect: TRect;
begin
  if FPainting then
    Exit;
  FPainting := True;
  try
    CurrentPanel := Self;
    CheckInstallHook;
    ShowIfEditControl;
    if not Visible then
      Exit;
    try
      ModuleServices := BorlandIDEServices as IOTAModuleServices;
      LastRow := -1;
      BottomRow := -1;
      if Assigned(ModuleServices) then
      begin
        Module := ModuleServices.CurrentModule;
        FileName := Module.CurrentEditor.FileName;
        FLiveBlameData.Load;
        if Assigned(Module) then
        begin
          CurrentEditor := Module.CurrentEditor;
          if Supports(CurrentEditor, IOTASourceEditor, SourceEdit) and
            (SourceEdit.EditViewCount > 0) then
          begin
            for I := 0 to Pred(SourceEdit.EditViewCount) do
            if Assigned(SourceEdit.EditViews[I]) and Assigned(SourceEdit.EditViews[I].GetEditWindow) and
              (SourceEdit.EditViews[I].GetEditWindow.Form = Owner) then
            begin
              FCursorLine := SourceEdit.EditViews[I].CursorPos.Line - 1;
              FTopLine := SourceEdit.EditViews[I].TopRow - 1;
              BottomRow := SourceEdit.EditViews[I].BottomRow;
              LastRow := SourceEdit.EditViews[I].Position.LastRow;
              FCY := SourceEdit.EditViews[I].ViewSize.cy - 1;
            end;
            if FLiveBlameData.FBlameInfoAvailable then
              UpdateLineHistory(SourceEdit);
          end;
        end;
      end;
      Bitmap := TBitmap.Create;
      Bitmap.Width := FPaintBox.Width;
      Bitmap.Height := FPaintBox.Height;
      Canvas := Bitmap.Canvas;
      Canvas.Brush.Color := DiffBackGroundColor;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Font.Color := DiffFontColor;
      Canvas.Pen.Width := 5;
      Canvas.Pen.Color := DiffColorBarLineColor;
      EH := Height - GetSystemMetrics(SM_CYHSCROLL);
      LH := EH div FCY;
      Canvas.Font.Name := 'Courier New';
      Canvas.Font.Size := 10;
      LN := FTopLine + 1;
      EditControl := GetEditControl;
      {//moved down
      if FBlameInfoReady then
      begin
        BuildLineHistory;
        UpdateGutterWidth;
        FBlameInfoReady := False;
        FBlameInfoAvailable := True;
      end;
      }
      if FLiveBlameData.FBlameInfoReady then
        FLiveBlameData.FStage := 3;
      if FLiveBlameData.FBlameInfoAvailable then
      begin
        FLiveBlameData.FBufferRevision.FDateStr := GetDateStr(FSettings.DateFormat, FLiveBlameData.FBufferRevision.FDate);
        FLiveBlameData.FFileRevision.FDateStr := GetDateStr(FSettings.DateFormat, FLiveBlameData.FFileRevision.FDate);
      end;

      FRevisionRectangles.Clear;
      FRevisionHintRectangles.Clear;
      FDeletedLinesHintTriangles.Clear;
      PaintBlocks := TObjectList<TPaintBlock>.Create;
      try
        PaintBlock := nil;
        for I := FTopLine to BottomRow - 1 do
          if LN <= LastRow then
          begin
            Elided := LineIsElided(EditControl, LN + 1);
            if Elided or not Assigned(PaintBlock) then
              PaintBlock := PaintBlocks[PaintBlocks.Add(TPaintBlock.Create(LN, I - FTopLine + 1))];
            if Elided then
            begin
              PaintBlock.FViewEndRow := PaintBlock.ViewStartRow;
              while Elided do
              begin
                Inc(LN);
                Elided := LineIsElided(EditControl, LN);
                if Elided then
                  PaintBlock.FEndRow := LN;
              end;
              PaintBlock := nil;
            end
            else
            begin
              PaintBlock.FEndRow := LN;
              PaintBlock.FViewEndRow := I - FTopLine + 1;
              Inc(LN);
            end;
          end;
        if (PaintBlocks.Count > 0) and (PaintBlocks.Last.EndRow = -1) then
          PaintBlocks.Delete(PaintBlocks.Count - 1);
        if FLiveBlameData.FBlameInfoAvailable and (PaintBlocks.Count > 0) then
        begin
          Canvas.Brush.Color := clBtnFace;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
          Canvas.Brush.Style := bsClear;
          Canvas.Font.Color := clWindowText;
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clSilver;
          Canvas.Pen.Style := psSolid;
          for I := 0 to PaintBlocks.Count - 1 do
            if (PaintBlocks[I].ViewStartRow <> PaintBlocks[I].ViewEndRow) or
              ((PaintBlocks[I].EndRow - PaintBlocks[I].StartRow) =
              (PaintBlocks[I].ViewStartRow - PaintBlocks[I].ViewEndRow)) then
            begin
              PaintBlocksX(Canvas, btRevision, FRevisionRectX1, FRevisionRectX2, FRevisionX, PaintBlocks[I]);
              PaintBlocksX(Canvas, btDate, FDateRectX1, FDateRectX2, FDateX, PaintBlocks[I]);
              PaintBlocksX(Canvas, btUser, FUserRectX1, FUserRectX2, FUserX, PaintBlocks[I]);
            end
            else
            begin
              Canvas.TextOut(2, LH * PaintBlocks[I].ViewStartRow - LH, Format('... %d lines hidden', [PaintBlocks[I].EndRow - PaintBlocks[I].StartRow]));
            end;
          if FSettings.ColorBarOrderList.Count > 0 then
          begin
            for I := 0 to FSettings.ColorBarOrderList.Count - 1 do
              PaintColorBar(Canvas, Rect(8 * I, 0, 8 + 8 * I, Bitmap.Height),
                FLiveBlameData.FLines.Count, clNone, clWindow, GetLineColor, Integer(FSettings.ColorBarOrderList[I]));
            if FCursorLine >= 0 then
            begin
              I := ((Bitmap.Height - 2) * FCursorLine) div FLiveBlameData.FLines.Count + 1;
              Canvas.Pen.Color := clBlack;
              Canvas.MoveTo(0, I);
              Canvas.LineTo(FSettings.ColorBarOrderList.Count * 8, I);
            end;
            LN := BottomRow;
            if LastRow < LN then
              LN := LastRow;
            if (FTopLine >= 0) and (LN >= FTopLine) then
            begin
              I := ((Bitmap.Height - 2) * FTopLine) div FLiveBlameData.FLines.Count + 1;
              EditViewRect := Rect(0, I, FSettings.ColorBarOrderList.Count * 8, 0);
              I := ((Bitmap.Height - 2) * LN) div FLiveBlameData.FLines.Count + 1;
              EditViewRect.Bottom := I;
              if EditViewRect.Bottom >= EditViewRect.Top then
              begin
                Canvas.Pen.Color := clBlack;
                Canvas.Brush.Style := bsClear;
                Canvas.Rectangle(EditViewRect);
              end;
            end;
          end;
        end;
        if not FLiveBlameData.FBlameInfoAvailable then
        begin
          if FLiveBlameData.FStage = 1 then
            Canvas.TextOut(6, 0, 'Loading History...')
          else
          if FLiveBlameData.FStage = 2 then
            Canvas.TextOut(6, 0, 'Loading Blame..')
          else
          if FLiveBlameData.FStage = 3 then
            Canvas.TextOut(6, 0, 'Prepare View..')
          else
            Canvas.TextOut(6, 0, 'Loading...');
          if FLiveBlameData.FBlameCounter > 0 then
            Canvas.TextOut(6, LH * 2, FormatFloat('#,#', FLiveBlameData.FBlameCounter) + ' Bytes received');
          if (FLiveBlameData.FBlameRevision <> -1) and (FLiveBlameData.FMaxRevision <> 0) then
          begin
            if not FProgressBar.Visible and (FLiveBlameData.FMaxRevision > 0) then
            begin
              FProgressBar.Position := 0;
              FProgressBar.Max := FLiveBlameData.FMaxRevision + FLiveBlameData.FMaxRevision div 5;
              FProgressBar.Top := LH * 3;
              FProgressBar.Left := 6;
              FProgressBar.Width := Self.Width - 12;
              FProgressBar.Visible := True;
            end;
            FProgressBar.Position := FLiveBlameData.FBlameRevision + FLiveBlameData.FMaxRevision div 10;
          end;
        end
        else
          FProgressBar.Visible := False;
      finally
        PaintBlocks.Free;
      end;
      FPaintBox.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      Application.ProcessMessages;
      if FLiveBlameData.FBlameInfoReady then
      begin
        BuildLineHistory;
        UpdateGutterWidth;
        FLiveBlameData.FBlameInfoReady := False;
        FLiveBlameData.FBlameInfoAvailable := True;
        FPaintBox.Invalidate;
      end;
      Bitmap.Free;
    except
      OutputDebugString('Exception in PaintBoxPaint');
    end;
  finally
    FPainting := False;
  end;
end;

procedure TLiveBlameEditorPanel.PaintColorBar(ACanvas: TCanvas; ARect: TRect;
  ALinesCount, APenColor, ABackGroundColor: TColor;
  AOnGetLineColor: TOnGetLineColorEvent; AColorIndex: Integer);
var
  HeightRatio: Single;
  TruncedHeightRatio: Integer;
  I, Line, Y, Y1, Y2, StartY, X1, X2: Integer;
  RepaintLocalChanges: Boolean;
  LocalLines: TList<Integer>;
begin
  if ALinesCount = 0 then
  begin
    ACanvas.Brush.Color := ABackGroundColor;
    ACanvas.Pen.Style := psClear;
    ACanvas.Rectangle(ARect);
  end
  else
  begin
    if APenColor = clNone then
    begin
      HeightRatio := (ARect.Bottom - ARect.Top) / ALinesCount;
      StartY := ARect.Top;
      X1 := ARect.Left;
      X2 := ARect.Right;
      ACanvas.Pen.Style := psClear;
    end
    else
    begin
      HeightRatio := (ARect.Bottom - ARect.Top - 2) / ALinesCount;
      StartY := ARect.Top + 1;
      X1 := ARect.Left + 1;
      X2 := ARect.Right - 1;
      ACanvas.Pen.Width := 1;
      ACanvas.Pen.Color := APenColor;
    end;
    TruncedHeightRatio := Trunc(HeightRatio);
    ACanvas.Brush.Color := ABackGroundColor;
    ACanvas.Rectangle(ARect);
    ACanvas.Pen.Style := psSolid;
    if Assigned(AOnGetLineColor) then
    begin
      RepaintLocalChanges := HeightRatio < 1.0;
      LocalLines := TList<Integer>.Create;
      try
        for I := 0 to Pred(ALinesCount) do
        begin
          ACanvas.Pen.Color := AOnGetLineColor(I, AColorIndex);
          Y1 := StartY + Trunc(I * HeightRatio);
          Y2 := Y1 + TruncedHeightRatio;
          for Y := Y1 to Y2 do
          begin
            ACanvas.MoveTo(X1, Y);
            ACanvas.LineTo(X2, Y);
          end;
          if RepaintLocalChanges and (FLiveBlameData.FLines.Count > I) then
            if (FLiveBlameData.FLines[I] = FLiveBlameData.FFileRevision) or
              (FLiveBlameData.FLines[I] = FLiveBlameData.FBufferRevision) then
            LocalLines.Add(I);
        end;
        if LocalLines.Count > 0 then
          for I := 0 to LocalLines.Count - 1 do
          begin
            Line := LocalLines[I];
            ACanvas.Pen.Color := AOnGetLineColor(Line, AColorIndex);
            Y1 := StartY + Trunc(Line * HeightRatio);
            Y2 := Y1 + TruncedHeightRatio;
            for Y := Y1 to Y2 do
            begin
              ACanvas.MoveTo(X1, Y);
              ACanvas.LineTo(X2, Y);
            end;
          end;
      finally
        LocalLines.Free;
      end;
    end;
  end;
end;

{ TLiveBlameWizard }

function TLiveBlameWizard.GetIDString: string;
begin
  Result := 'USc.Live Blame';
end;

function TLiveBlameWizard.GetName: string;
begin
  Result := 'Live Blame';
end;

procedure TLiveBlameWizard.AfterSave;
begin
end;

procedure TLiveBlameWizard.BeforeSave;
begin
end;

procedure TLiveBlameWizard.Destroyed;
begin
end;

procedure TLiveBlameWizard.Execute;
begin
end;

function TLiveBlameWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TLiveBlameWizard.Modified;
begin
end;

procedure TLiveBlameWizard.ViewNotification(const View: IOTAEditView; Operation: TOperation); //IOTAEditorNotifier
begin
  if Operation = opInsert then
    OutputDebugString('TLiveBlameWizard.ViewActivated inserted')
  else
    OutputDebugString('TLiveBlameWizard.ViewActivated removed');
end;

procedure TLiveBlameWizard.ViewActivated(const View: IOTAEditView); //IOTAEditorNotifier
begin
  OutputDebugString('TLiveBlameWizard.ViewActivated');
end;

constructor TLiveBlameWizard.Create;
var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
begin
  inherited;
  FPanelList := TList.Create;
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  NotifierIndex := EditorServices.AddNotifier(TMyINTANotifier.Create(Self, FPanelList));

  Services := BorlandIDEServices as IOTAServices;
  NotifierIndex2 := Services.AddNotifier(TIdeNotifier.Create(Self, FPanelList));//TODO:?
end;

destructor TLiveBlameWizard.Destroy;
var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
  I: Integer;
begin
  RemoveBlamePanel;
  if NotifierIndex <> -1 then
  begin
    EditorServices := BorlandIDEServices as IOTAEditorServices;
    EditorServices.RemoveNotifier(NotifierIndex);
  end;
  if NotifierIndex2 <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    Services.RemoveNotifier(NotifierIndex2);
  end;
  for I := Pred(FPanelList.Count) downto 0 do
    TObject(FPanelList[I]).Free;
  FPanelList.Free;
  inherited Destroy;
end;

procedure TLiveBlameWizard.RemovePanel(AObject: TObject);
var
  I: Integer;
begin
  OutputDebugString('TLiveBlameWizard.RemovePanel');
  for I := Pred(FPanelList.Count) downto 0 do
    if FPanelList[I] = AObject then
    begin
      FPanelList.Delete(I);
      Break;
    end;
end;

end.
