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
{ The Original Code is SvnIDETypes.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit GitIDETypes;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Classes, Graphics, Generics.Collections, ActiveX, GitIDEClient,
  GitClientProgress;

type
  TCustomProgressThread = class(TThread)
  protected
    FAborted: Boolean;
    FSyncMax: Integer;
    FSyncPosition: Integer;
    FSyncText: string;
    FSvnProgressDialog: TGitProgressDialog;
    procedure AbortCallBack;
    procedure SyncCloseDialog;
    procedure SyncUpdateCaption;
    procedure SyncUpdateProgress;
    procedure UpdateCaption(const AText: string);
    procedure UpdateProgress(APosition, AMax: Integer);
  public
    constructor Create;
  end;

  TCompareRevisionThread = class(TCustomProgressThread)
  private
    type
      TCompareFile = class(TObject)
      private
        FFileName: string;
        FRevision1: string;
        FRevision2: string;
        FStream1: IStream;
        FStream2: IStream;
      public
        constructor Create(const AFileName: string; ARevision1, ARevision2: string);
        property FileName: string read FFileName;
        property Revision1: string read FRevision1;
        property Revision2: string read FRevision2;
        property Stream1: IStream read FStream1 write FStream1;
        property Stream2: IStream read FStream2 write FStream2;
      end;
    var
      FFiles: TObjectList<TCompareFile>;
    procedure SyncCallCompare;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddFile(const AFileName: string; ARevision1, ARevision2: string);
  end;

  TSaveRevisionThread = class(TCustomProgressThread)
  private
    FFiles: TStringList;
    FPath: string;
    FRevision: string;
  public
    constructor Create(ARevision: string; const APath: string);
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddFile(const AFileName: string);
  end;

implementation

uses
  SysUtils, GitClient, GitIDEConst, Forms, ToolsAPI, FileCtrl;

const
  cFileNameTag = 'fn';
  cFileNameTagOpenStr = '<' + cFileNameTag + '>';
  cFileNameTagCloseStr = '</' + cFileNameTag + '>';

{ TCustomProgressThread }

constructor TCustomProgressThread.Create;
begin
  inherited Create(True);
  FAborted := False;
  FSvnProgressDialog := TGitProgressDialog.Create(Application);
  FSvnProgressDialog.AbortCallBack := AbortCallBack;
  FSvnProgressDialog.Show;
  FreeOnTerminate := True;
end;

procedure TCustomProgressThread.AbortCallBack;
begin
  FAborted := True;
end;

procedure TCustomProgressThread.SyncCloseDialog;
begin
  FSvnProgressDialog.Close;
end;

procedure TCustomProgressThread.SyncUpdateCaption;

  function ShortenFileNameInString(const AInput, ATag: string; ACanvas: TCanvas; AMaxWidth: Integer): string;
  var
    P1, P2, TagLength, WidthWithoutFileName: Integer;
    FileName: string;
  begin
    P1 := Pos('<' + ATag + '>', AInput);
    if P1 > 0 then
      P2 := Pos('</' + ATag + '>', AInput)
    else
      P2 := 0;
    if P2 > 0 then
    begin
      Result := AInput;
      TagLength := Length(ATag) + 2;
      FileName := Copy(Result, P1 + TagLength, P2 - P1 - TagLength);
      Delete(Result, P1, P2 - P1 + TagLength + 1);
      if FileName <> '' then
      begin
        WidthWithoutFileName := ACanvas.TextWidth(Result);
        FileName := FileCtrl.MinimizeName(FileName, ACanvas, AMaxWidth - WidthWithoutFileName);
        Insert(FileName, Result, P1);
      end;
    end
    else
      Result := AInput;
  end;

var
  S: string;
begin
  S := ShortenFileNameInString(FSyncText, cFileNameTag, FSvnProgressDialog.lbInfo.Canvas,
    FSvnProgressDialog.ProgressBar1.Width);
  FSvnProgressDialog.lbInfo.Caption := S;
end;

procedure TCustomProgressThread.SyncUpdateProgress;
begin
  FSvnProgressDialog.ProgressBar1.Max := FSyncMax;
  FSvnProgressDialog.ProgressBar1.Position := FSyncPosition;
end;

procedure TCustomProgressThread.UpdateCaption(const AText: string);
begin
  FSyncText := AText;
  Synchronize(SyncUpdateCaption);
end;

procedure TCustomProgressThread.UpdateProgress(APosition, AMax: Integer);
begin
  FSyncPosition := APosition;
  FSyncMax := AMax;
  Synchronize(SyncUpdateProgress);
end;

{ TCompareRevisionThread.TCompareFile }

constructor TCompareRevisionThread.TCompareFile.Create(const AFileName: string; ARevision1,
  ARevision2: string);
begin
  FFileName := AFileName;
  FRevision1 := ARevision1;
  FRevision2 := ARevision2;
end;

{ TCompareRevisionThread }

constructor TCompareRevisionThread.Create;
begin
  inherited Create;
  FFiles := TObjectList<TCompareFile>.Create;
end;

destructor TCompareRevisionThread.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TCompareRevisionThread.AddFile(const AFileName: string; ARevision1,
  ARevision2: string);
begin
  FFiles.Add(TCompareFile.Create(AFileName, ARevision1, ARevision2));
end;

procedure TCompareRevisionThread.Execute;
var
  I: Integer;
  FileName: string;
  MemStream: TMemoryStream;
begin
  for I := 0 to FFiles.Count - 1 do
  begin
    FileName := StringReplace(FFiles[I].FileName, '/', '\', [rfReplaceAll]);
    UpdateCaption(Format(sRetrievingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, Copy(FFiles[I].Revision1, 1, 7)]));
    MemStream := TMemoryStream.Create;
    FFiles[I].Stream1 := TStreamAdapter.Create(MemStream, soOwned);
    IDEClient.GitClient.SaveFileContentToStream(FFiles[I].FileName, FFiles[I].Revision1, MemStream);
    UpdateProgress(I * 2 + 1, FFiles.Count * 2);
    if FAborted then
      Break;
    UpdateCaption(Format(sRetrievingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, Copy(FFiles[I].Revision2, 1, 7)]));
    MemStream := TMemoryStream.Create;
    FFiles[I].Stream2 := TStreamAdapter.Create(MemStream, soOwned);
    IDEClient.GitClient.SaveFileContentToStream(FFiles[I].FileName, FFiles[I].Revision2, MemStream);
    UpdateProgress(I * 2 + 2, FFiles.Count * 2);
    if FAborted then
      Break;
  end;
  Synchronize(SyncCallCompare);
end;

procedure TCompareRevisionThread.SyncCallCompare;
var
  I: Integer;
begin
  FSvnProgressDialog.Free;
  if not FAborted then
    for I := 0 to FFiles.Count - 1 do
    begin
      (BorlandIDEServices as IOTACustomDifferenceManager).
        ShowDifference(FFiles[I].Stream1, FFiles[I].Stream2, FFiles[I].FileName + '-' + Copy(FFiles[I].Revision1, 1, 7),
          FFiles[I].FileName  + '-' + Copy(FFiles[I].Revision2, 1, 7), '', '', dfOTARevision, dfOTARevision, dtOTADefault);
      FFiles[I].Stream1 := nil;
      FFiles[I].Stream2 := nil;
    end;
end;

{ TSaveRevisionThread }

constructor TSaveRevisionThread.Create(ARevision: string; const APath: string);
begin
  inherited Create;
  FFiles := TStringList.Create;
  FPath := APath;
  FRevision := ARevision;
end;

destructor TSaveRevisionThread.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TSaveRevisionThread.AddFile(const AFileName: string);
begin
  FFiles.Add(AFileName);
end;

procedure TSaveRevisionThread.Execute;
var
  I: Integer;
  FileName, DestFileName: string;
  FileStream: TFileStream;
begin
  for I := 0 to FFiles.Count - 1 do
  begin
    FileName := StringReplace(FFiles[I], '/', '\', [rfReplaceAll]);
    UpdateCaption(Format(sSavingFileRevision,
      [cFileNameTagOpenStr + FileName + cFileNameTagCloseStr, Copy(FRevision, 1, 7)]));
    DestFileName := IncludeTrailingPathDelimiter(FPath) + ExtractFileName(FileName);
    DestFileName := ChangeFileExt(DestFileName, Format('-%s%s', [Copy(FRevision, 1, 7), ExtractFileExt(DestFileName)]));
    FileStream := TFileStream.Create(DestFileName, fmCreate);
    try
      IDEClient.GitClient.SaveFileContentToStream(FFiles[I], FRevision, FileStream);
    finally
      FileStream.Free;
    end;
    UpdateProgress(I + 1, FFiles.Count);
    if FAborted then
      Break;
  end;
  Synchronize(SyncCloseDialog);
end;

end.
