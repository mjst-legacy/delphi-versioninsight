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
unit HgClientUpdate;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommCtrl, ComCtrls, ExtCtrls, Menus, ActnList;

type

  TAbortCallBack = procedure of object;
  TResolveCallBack = function(const FileName: string): Boolean of object;
  TFileRefreshCallBack = procedure(const FileList: TStringList) of object;

  THgUpdateDialog = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    Files: TListView;
    Ok: TButton;
    Abort: TButton;
    Help: TButton;
    ActionList1: TActionList;
    PopupMenu: TPopupMenu;
    ResolveAction: TAction;
    Resolve1: TMenuItem;
    procedure AbortClick(Sender: TObject);
    procedure ResolveActionUpdate(Sender: TObject);
    procedure ResolveActionExecute(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure FilesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    const
      TextMarginUndefined = -1;
    var
      FColumnWidths: array [0..1] of Integer;
      FColumnTextMargins: array [0..1] of Integer;
  protected
    FCompleted: Boolean;
    FAbortCallBack: TAbortCallBack;
    FResolveCallBack: TResolveCallBack;
    FFileRefreshCallBack: TFileRefreshCallBack;
  public
    procedure Add(const FileName, Action: string; Conflicted: Boolean; AColor: TColor = clNone);
    procedure Completed;
    property AbortCallBack: TAbortCallBack read FAbortCallBack write FAbortCallBack;
    property ResolveCallBack: TResolveCallBack read FResolveCallBack write FResolveCallBack;
    property FileRefreshCallBack:TFileRefreshCallBack read FFileRefreshCallBack write FFileRefreshCallBack;
  end;

function GetUpdateDialog(const Url: string; AAbortCallBack: TAbortCallBack;
  AResolveCallBack: TResolveCallBack;
  AFileRefreshCallBack: TFileRefreshCallBack): THgUpdateDialog;

implementation

//uses SvnUIConst, svnconst;

{$R *.dfm}

function GetUpdateDialog(const Url: string; AAbortCallBack: TAbortCallBack;
  AResolveCallBack: TResolveCallBack; AFileRefreshCallBack: TFileRefreshCallBack): THgUpdateDialog;
begin
  Result := THgUpdateDialog.Create(Application);
  Result.Caption := Format(Result.Caption, [Url]);
  Result.AbortCallBack := AAbortCallBack;
  Result.ResolveCallBack := AResolveCallBack;
  Result.FileRefreshCallBack := AFileRefreshCallBack;
end;

{ TUpdateDialog }

procedure THgUpdateDialog.AbortClick(Sender: TObject);
begin
  FAbortCallBack;
end;

procedure THgUpdateDialog.Add(const FileName, Action: string;
  Conflicted: Boolean; AColor: TColor = clNone);

  //emulation of LVSCW_AUTOSIZE
  procedure UpdateColumnWidth(AColumn: TListColumn; AItem: TListItem);
  var
    Index, ColumnWidth, TextWidth: Integer;
    Text: string;
  begin
    Index := AColumn.Index;
    if Index = 0 then
      Text := AItem.Caption
    else
      Text := AItem.SubItems[Index - 1];
    //get the width for the column text (does not include the text margin)
    TextWidth := Files.StringWidth(Text);
    //get the text margin of the column by using LVSCW_AUTOSIZE once
    if FColumnTextMargins[Index] = TextMarginUndefined then
    begin
      AColumn.Width := LVSCW_AUTOSIZE;
      ColumnWidth := ListView_GetColumnWidth(Files.Handle, Index);
      FColumnTextMargins[Index] := ColumnWidth - TextWidth;
      FColumnWidths[Index] := 0;
    end;
    TextWidth := TextWidth + FColumnTextMargins[Index];
    //compare cached column width with width for current column text + margin
    if FColumnWidths[Index] < TextWidth then
    begin
      AColumn.Width := TextWidth;
      FColumnWidths[Index] := TextWidth;
    end;
  end;

var
  Item: TListItem;
  I: Integer;
begin
  Item := Files.Items.Add;
  Item.Caption := Action;
  Item.SubItems.Add(FileName);
  Item.Data := Pointer(AColor);
  if Conflicted then
    Item.GroupID := 0
  else
    Item.GroupID := 1;
  //Autoscoll
  if not Conflicted then
    Item.MakeVisible(False);
  for I := 0 to Files.Columns.Count - 1 do
    UpdateColumnWidth(Files.Columns[I], Item);
end;

procedure THgUpdateDialog.Completed;
var
  FileList: TStringList;
  I: Integer;
begin
  Ok.Enabled := True;
  Abort.Enabled := False;
  FCompleted := True;
  FileList := TStringList.Create;
  try
    for I := 0 to Files.Items.Count - 1 do
      FileList.AddObject(Files.Items[I].SubItems[0], TObject(Files.Items[I].GroupID));
    if Assigned(FileRefreshCallBack) then
      FileRefreshCallBack(FileList);
  finally
    FileList.Free;
  end;
end;

procedure THgUpdateDialog.FilesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  TextColor: TColor;
begin
  DefaultDraw := True;
  TextColor := TColor(Item.Data);
  if TextColor <> clNone then
    Files.Canvas.Font.Color := TextColor;
end;

procedure THgUpdateDialog.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  Constraints.MinHeight := MulDiv(350, Screen.PixelsPerInch, 96);
  Constraints.MinWidth := MulDiv(450, Screen.PixelsPerInch, 96);
  for I := Low(FColumnTextMargins) to High(FColumnTextMargins) do
    FColumnTextMargins[I] := TextMarginUndefined;
end;

procedure THgUpdateDialog.HelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure THgUpdateDialog.OkClick(Sender: TObject);
{
var
  I: Integer;
}
begin
  {
  for I := 0 to Files.Items.Count - 1 do
    if Files.Items[I].GroupID = 0 then
    begin
      if MessageDlg(sConflictsRemaining, mtInformation, mbYesNo, 0) = mrYes then
        Close;
      Exit;
    end;
  }
  Close;
end;

procedure THgUpdateDialog.ResolveActionExecute(Sender: TObject);
begin
  {
  if Assigned(Files.Selected) then
    if FResolveCallBack(Files.Items[Files.ItemIndex].SubItems[0]) then
    begin
      Files.Items[Files.ItemIndex].GroupID := 1;
      Files.Items[Files.ItemIndex].Caption := SWcNotifyResolved;
      Files.Items[Files.ItemIndex].Data := Pointer(clNone);
    end;
  }
end;

procedure THgUpdateDialog.ResolveActionUpdate(Sender: TObject);
begin
  if Assigned(Files.Selected) then
    ResolveAction.Visible := Files.Items[Files.ItemIndex].GroupID = 0
  else
    ResolveAction.Visible := False;
end;

end.
