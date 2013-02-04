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
{ The Original Code is VerInsIDEDockInfo.pas.                                  }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 Uwe Schuster.          }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEDockInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Dockform, ActnList,
  StdCtrls, ExtCtrls, VerInsLiveBlameTypes, Generics.Collections, Generics.Defaults;

type
  TfmLiveBlameInfo = class(TDockableForm)
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    Label3: TLabel;
    Label4: TLabel;
    Panel2: TPanel;
    Label5: TLabel;
    PaintBox4: TPaintBox;
    PaintBox5: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox2Paint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox3Paint(Sender: TObject);
    procedure PaintBox4Paint(Sender: TObject);
    procedure PaintBox5Paint(Sender: TObject);
  private
    { Private declarations }
    FInformation: TLiveBlameInformation;
    procedure PaintRevision(ATargetCanvas: TCanvas; AX, AY: Integer; ARevision: TJVCSLineHistoryRevision;
      W1: Integer = 0; W2: Integer = 0);
    procedure PaintSummary(ATargetCanvas: TCanvas; AX, AY: Integer; ASummary: TJVCSLineHistorySummary);
    procedure SetInformation(const Value: TLiveBlameInformation);
  public
    { Public declarations }
    property Information: TLiveBlameInformation read FInformation write SetInformation;
  end;

var
  fmLiveBlameInfo: TfmLiveBlameInfo;

procedure ShowLiveBlameInfo;
procedure UpdateLiveBlameInfo(ALiveBlameInformation: TLiveBlameInformation);

implementation

{$R *.dfm}

procedure ShowLiveBlameInfo;
begin
  if not Assigned(fmLiveBlameInfo) then
    fmLiveBlameInfo := TfmLiveBlameInfo.Create(Application);
  fmLiveBlameInfo.Show;
end;

procedure UpdateLiveBlameInfo(ALiveBlameInformation: TLiveBlameInformation);
begin
  if Assigned(fmLiveBlameInfo) then
    fmLiveBlameInfo.Information := ALiveBlameInformation;
end;

procedure TfmLiveBlameInfo.FormCreate(Sender: TObject);
var
  C, ImgList: TComponent;
  I: Integer;
  Icon: TIcon;
begin
  inherited;
  FInformation := TLiveBlameInformation.Create;
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
  if Assigned(ImgList) then
  begin
    Icon := TIcon.Create;
    try
      TImageList(ImgList).GetIcon(16, Icon);
      Self.Icon.Assign(Icon);
    finally
      Icon.Free;
    end;
  end;
end;

procedure TfmLiveBlameInfo.FormDestroy(Sender: TObject);
begin
  inherited;
  FInformation.Free;
end;

procedure TfmLiveBlameInfo.PaintBox1Paint(Sender: TObject);
var
  W: Integer;
  S: string;
begin
  inherited;
  if Assigned(FInformation.LatestMethodRevision) then
  begin
    S := 'Latest Revision:';
    W := PaintBox1.Canvas.TextWidth(S);
    PaintBox1.Canvas.TextOut(0, 4, S);
    W := W + 2;
    PaintRevision(PaintBox1.Canvas, W, 0, FInformation.LatestMethodRevision);
  end;
end;

procedure TfmLiveBlameInfo.PaintBox2Paint(Sender: TObject);
var
  W: Integer;
  S: string;
begin
  inherited;
  if Assigned(FInformation.LineRevision) then
  begin
    S := 'Revision:';
    W := PaintBox2.Canvas.TextWidth(S);
    PaintBox2.Canvas.TextOut(0, 4, S);
    W := W + 2;
    PaintRevision(PaintBox2.Canvas, W, 0, FInformation.LineRevision);
  end;
end;

type
  TIntegerComparer = class(TInterfacedObject, IComparer<TJVCSLineHistoryRevisionSummary>)
    function Compare(const Left, Right: TJVCSLineHistoryRevisionSummary): Integer;
  end;

{ TIntegerComparer }

function TIntegerComparer.Compare(const Left, Right: TJVCSLineHistoryRevisionSummary): Integer;
begin
  Result := Right.LineCount - Left.LineCount;
end;

procedure TfmLiveBlameInfo.PaintBox3Paint(Sender: TObject);
begin
  inherited;
  if Assigned(FInformation.LatestMethodRevision) then
    PaintSummary(PaintBox3.Canvas, 0, 0, FInformation.MethodSummary);
end;

procedure TfmLiveBlameInfo.PaintBox4Paint(Sender: TObject);
begin
  inherited;
  if Assigned(FInformation.LatestRevision) then
    PaintSummary(PaintBox4.Canvas, 0, 0, FInformation.Summary);
end;

procedure TfmLiveBlameInfo.PaintBox5Paint(Sender: TObject);
var
  W: Integer;
  S: string;
begin
  inherited;
  if Assigned(FInformation.LatestRevision) then
  begin
    S := 'Latest Revision:';
    W := PaintBox5.Canvas.TextWidth(S);
    PaintBox5.Canvas.TextOut(0, 4, S);
    W := W + 2;
    PaintRevision(PaintBox5.Canvas, W, 0, FInformation.LatestRevision);
  end;
end;

procedure TfmLiveBlameInfo.PaintRevision(ATargetCanvas: TCanvas; AX, AY: Integer; ARevision: TJVCSLineHistoryRevision;
  W1: Integer = 0; W2: Integer = 0);
var
  W, X: Integer;
  RevisionColors: TRevisionColor;
begin
  X := 2 + AX;

  RevisionColors := FInformation.GetRevisionColor(ARevision);

  if Assigned(RevisionColors) then
  ATargetCanvas.Brush.Color := RevisionColors.RevisionColor;
  ATargetCanvas.Rectangle(Rect(X, 4 + AY, X + 14, 18 + AY));
  Inc(X, 16);
  if W1 > 0 then
    W := W1
  else
    W := ATargetCanvas.TextWidth(ARevision.RevisionStr);
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(X, 4 + AY, ARevision.RevisionStr);
  Inc(X, W + 2);

  if Assigned(RevisionColors) then
  ATargetCanvas.Brush.Color := RevisionColors.DateColor;
  ATargetCanvas.Brush.Style := bsSolid;
  ATargetCanvas.Rectangle(Rect(X, 4 + AY, X + 14, 18 + AY));
  Inc(X, 16);
  if W2 > 0 then
    W := W2
  else
    W := ATargetCanvas.TextWidth(ARevision.DateStr);
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(X, 4 + AY, ARevision.DateStr);
  Inc(X, W + 2);

  if Assigned(RevisionColors) then
  ATargetCanvas.Brush.Color := RevisionColors.UserColor;
  ATargetCanvas.Brush.Style := bsSolid;
  ATargetCanvas.Rectangle(Rect(X, 4 + AY, X + 14, 18 + AY));
  Inc(X, 16);
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(X, 4 + AY, ARevision.UserStr);
end;

procedure TfmLiveBlameInfo.PaintSummary(ATargetCanvas: TCanvas; AX, AY: Integer; ASummary: TJVCSLineHistorySummary);
var
  I, L, W, OffsetX, W0, W1, W2: Integer;
  Rev: TJVCSLineHistoryRevision;
  Summary: TList<TJVCSLineHistoryRevisionSummary>;
  S: string;
begin
  OffsetX := 0;
  S := 'Top revisions:';
  W := ATargetCanvas.TextWidth(S);
  ATargetCanvas.TextOut(OffsetX, 4, S);
  OffsetX := OffsetX + W + 2;
  L := 2;
  if ASummary.RevisionCount - 1 < L then
    L := ASummary.RevisionCount - 1;
  W0 := 0;
  W1 := 0;
  W2 := 0;
  Summary := TList<TJVCSLineHistoryRevisionSummary>.Create(TIntegerComparer.Create);
  try
    for I := 0 to ASummary.RevisionCount - 1 do
      Summary.Add(ASummary.RevisionSummary[I]);
    Summary.Sort;
    for I := 0 to L do
    begin
      W := ATargetCanvas.TextWidth(Format('%.2f%%', [Summary[I].Percent])) + 2;
      if W > W0 then
        W0 := W;
      Rev := Summary[I].LineHistoryRevision;
      W := ATargetCanvas.TextWidth(Rev.RevisionStr);
      if W > W1 then
        W1 := W;
      W := ATargetCanvas.TextWidth(Rev.DateStr);
      if W > W2 then
        W2 := W;
    end;
    for I := 0 to L do
    begin
      S := Format('%.2f%%', [Summary[I].Percent]);
      W := ATargetCanvas.TextWidth(S);
      ATargetCanvas.TextOut(OffsetX + W0 - W, 4 + I * 16, S);
      PaintRevision(ATargetCanvas, W0 + OffsetX, I * 16, Summary[I].LineHistoryRevision, W1, W2);
    end;
  finally
    Summary.Free;
  end;
end;

procedure TfmLiveBlameInfo.SetInformation(const Value: TLiveBlameInformation);
begin
  FInformation.Assign(Value);
  if Assigned(FInformation.LineRevision) then
    Memo1.Lines.Text := FInformation.LineRevision.Comment
  else
    Memo1.Lines.Text := '';

  Label3.Caption := Value.LineMethodName;
  Label4.Caption := IntToStr(Value.LineNo);

  PaintBox1.Invalidate;
  PaintBox2.Invalidate;
  PaintBox3.Invalidate;
  PaintBox4.Invalidate;
  PaintBox5.Invalidate;
end;

end.
