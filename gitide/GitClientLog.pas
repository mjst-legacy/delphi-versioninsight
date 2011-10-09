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
unit GitClientLog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommCtrl, ComCtrls, ExtCtrls, Generics.Collections, ToolWin,
  ImgList, GitUITypes, Menus, ActnList;

const
  DefaultRange = 100;

type
  TRevision = class
  protected
    FRevision: string;
    FTime: string;
    FAuthor: string;
    FComment: string;
    FBugID: string;
    FFiles: TStringList;
  public
    constructor Create(const Revision, Time, Author, Comment, BugID: string;
      const Files: TStringList);
    destructor Destroy; override;
  end;

  TLoadRevisionsCallBack = procedure(FirstRevision, LastRevision: string; Count: Integer) of object;
  TFileColorCallBack = function(Action: Char): TColor of object;
  TReverseMergeCallBack = procedure(const APathName: string; ARevision1, ARevision2: string) of object;
  TCompareRevisionCallBack = procedure(AFileList: TStringList; ARevision1, ARevision2: string) of object;
  TSaveRevisionCallBack = procedure(AFileList: TStringList; ARevision: string; const ADestPath: string) of object;

  TGitLogFrame = class(TFrame)
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    CenterPanel: TPanel;
    BottomPanel: TPanel;
    TopPanel: TPanel;
    Revisions: TListView;
    Files: TListView;
    Comment: TMemo;
    ToolBar1: TToolBar;
    Refresh: TToolButton;
    Next: TToolButton;
    Search: TButtonedEdit;
    ImageList: TImageList;
    ActionList1: TActionList;
    ReverseMergeRevisionAction: TAction;
    ReverseMergeToRevisionAction: TAction;
    RevisionPopupMenu: TPopupMenu;
    FilesPopupMenu: TPopupMenu;
    RevertChangesFromThisRevision1: TMenuItem;
    RevertToThisRevision1: TMenuItem;
    FileReverseMergeRevisionAction: TAction;
    RevertChangesFromThisRevision2: TMenuItem;
    CompareWithPreviousRevisionAction: TAction;
    CompareWithPreviousRevision1: TMenuItem;
    FileCompareWithPreviousRevisionAction: TAction;
    FileCompareWithPreviousRevision1: TMenuItem;
    FileSaveRevisionAction: TAction;
    FileSaveRevisionAction1: TMenuItem;
    Range: TToolButton;
    CompareRevisionsAction: TAction;
    CompareRevisions1: TMenuItem;
    procedure RevisionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RefreshClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure SearchKeyPress(Sender: TObject; var Key: Char);
    procedure SearchRightButtonClick(Sender: TObject);
    procedure FilesData(Sender: TObject; Item: TListItem);
    procedure RevisionsData(Sender: TObject; Item: TListItem);
    procedure FilesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ReverseMergeRevisionActionUpdate(Sender: TObject);
    procedure ReverseMergeToRevisionActionUpdate(Sender: TObject);
    procedure FileReverseMergeRevisionActionUpdate(Sender: TObject);
    procedure ReverseMergeRevisionActionExecute(Sender: TObject);
    procedure ReverseMergeToRevisionActionExecute(Sender: TObject);
    procedure FileReverseMergeRevisionActionExecute(Sender: TObject);
    procedure CompareWithPreviousRevisionActionUpdate(Sender: TObject);
    procedure CompareWithPreviousRevisionActionExecute(Sender: TObject);
    procedure FileCompareWithPreviousRevisionActionUpdate(Sender: TObject);
    procedure FileCompareWithPreviousRevisionActionExecute(Sender: TObject);
    procedure FileSaveRevisionActionExecute(Sender: TObject);
    procedure FileSaveRevisionActionUpdate(Sender: TObject);
    procedure RangeClick(Sender: TObject);
    procedure CompareRevisionsActionUpdate(Sender: TObject);
    procedure CompareRevisionsActionExecute(Sender: TObject);
    procedure RevisionsDataStateChange(Sender: TObject; StartIndex,
      EndIndex: Integer; OldState, NewState: TItemStates);
  protected
    FBugIDColumnNo: Integer;
    FCount: Integer;
    FDoingSearch: Boolean;
    FFileColorCallBack: TFileColorCallBack;
    FLoadRevisionsCallBack: TLoadRevisionsCallBack;
    FReverseMergeCallBack: TReverseMergeCallBack;
    FRootPath: string;
    FRootRelativePath: string;
    FCompareRevisionCallBack: TCompareRevisionCallBack;
    FSaveRevisionCallBack: TSaveRevisionCallBack;
    FRevisionColumns: array [0..4] of TListColumn;
    FRevisionColumnWidths: array [0..4] of Integer;
    FRevisionList: TObjectList<TRevision>;
    FVisibleRevisions: TList<TRevision>;
    FSaveCursor: TCursor;
    FFromRevision: Integer;
    FToRevision: Integer;
    FRevisionFiles: TStringList;
    FExecutingSelectAll: Boolean;
    class var FUseCount: Integer;
    procedure AddRevisionToListView(ARevision: TRevision);
    procedure DoCancelSearch;
    procedure DoSearch(const Text: string);
    function GetCommentColumn: Integer;
    function GetShowBugIDColumn: Boolean;
    function GetSvnEditState: TSvnEditState;
    procedure InitRevisionColumnWidths;
    procedure RestoreRevisions;
    procedure SetShowBugIDColumn(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRevisions(Revision: string; Time: TDateTime; const Author: string;
      const Comment, BugID: string; const Files: TStringList);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure StartAsync;
    procedure NextCompleted;
    function PerformEditAction(AEditAction: TSvnEditAction): Boolean;
    property FileColorCallBack: TFileColorCallBack read FFileColorCallBack write FFileColorCallBack;
    property LoadRevisionsCallBack: TLoadRevisionsCallBack read FLoadRevisionsCallBack write FLoadRevisionsCallBack;
    property ReverseMergeCallBack: TReverseMergeCallBack read FReverseMergeCallBack write FReverseMergeCallBack;
    property RootPath: string read FRootPath write FRootPath;
    property RootRelativePath: string read FRootRelativePath write FRootRelativePath;
    property CompareRevisionCallBack: TCompareRevisionCallBack read FCompareRevisionCallBack write FCompareRevisionCallBack;
    property SaveRevisionCallBack: TSaveRevisionCallBack read FSaveRevisionCallBack write FSaveRevisionCallBack;
    property ShowBugIDColumn: Boolean read GetShowBugIDColumn write SetShowBugIDColumn;
    property SvnEditState: TSvnEditState read GetSvnEditState;
  end;

implementation

uses GitUIConst, GitUIUtils, Clipbrd, FileCtrl;

{$R *.dfm}

{ TRevision }

constructor TRevision.Create(const Revision, Time, Author, Comment, BugID: string;
      const Files: TStringList);
begin
  FRevision := Revision;
  FTime := Time;
  FAuthor := Author;
  FComment := Comment;
  FBugID := BugID;
  if Files <> nil then
  begin
    FFiles := TStringList.Create;
    FFiles.Assign(Files);
  end;
end;

destructor TRevision.Destroy;
begin
  if FFiles <> nil then
    FFiles.Free;
  inherited;
end;

{ TSvnLogFrame }

procedure TGitLogFrame.CompareRevisionsActionExecute(Sender: TObject);
var
  I, J, Idx, FirstRevisionIndex, SecondRevisionIndex: Integer;
  FirstRevision, SecondRevision: TRevision;
  FilesSL, FileList: TStringList;
  S: string;
  A: Char;
begin
  FirstRevision := nil;
  SecondRevision := nil;
  FirstRevisionIndex := -1;
  SecondRevisionIndex := -1;
  for I := 0 to Revisions.Items.Count - 1 do
    if Revisions.Items[I].Selected then
    begin
      if not Assigned(FirstRevision) then
        FirstRevision := FVisibleRevisions[I]
      else
      begin
        SecondRevision := FVisibleRevisions[I];
        Break;
      end;
    end;
  if Assigned(SecondRevision) then
  begin
    for I := 0 to FRevisionList.Count -1 do
    begin
      if FRevisionList[I] = FirstRevision then
        FirstRevisionIndex := I
      else
      if FRevisionList[I] = SecondRevision then
        SecondRevisionIndex := I;
      if (FirstRevisionIndex <> -1) and (SecondRevisionIndex <> -1) then
        Break;
    end;
  end;
  if (FirstRevisionIndex <> -1) and (SecondRevisionIndex <> -1) then
  begin
    FileList := TStringList.Create;
    try
      FileList.Sorted := True;
      for I := SecondRevisionIndex - 1 downto FirstRevisionIndex do
      begin
        FilesSL := FRevisionList[I].FFiles;
        for J := 0 to FilesSL.Count - 1 do
        begin
          A := FilesSL[J][1];
          S := Copy(FilesSL[J], 2, MaxInt);
          Idx := FileList.IndexOf(S);
          if Idx <> -1 then
          begin
            if A = 'D' then
              FileList.Delete(Idx);
          end
          else
          if A <> 'D' then
            FileList.AddObject(S, TObject(Ord(A)));
        end;
      end;
      for I := FileList.Count - 1 downto 0 do
        if FileList.Objects[I] = TObject(Ord('A')) then
          FileList.Delete(I);
      if FileList.Count > 0 then
        FCompareRevisionCallBack(FileList, FirstRevision.FRevision, SecondRevision.FRevision);
    finally
      FileList.Free;
    end;
  end;
end;

procedure TGitLogFrame.CompareRevisionsActionUpdate(Sender: TObject);
begin
  CompareRevisionsAction.Visible := Assigned(FCompareRevisionCallBack) and (Revisions.SelCount = 2);
  CompareRevisionsAction.Enabled := CompareRevisionsAction.Visible;
end;

procedure TGitLogFrame.CompareWithPreviousRevisionActionExecute(Sender: TObject);
var
  I: Integer;
  Revision: TRevision;
  FilesSL, FileList: TStringList;
  S: string;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  FileList := TStringList.Create;
  try
    FilesSL := TStringList(Revisions.Selected.Data);
    for I := 0 to FilesSL.Count - 1 do
    begin
      S := FilesSL[I];
      if S[1] = 'M' then
        FileList.Add(Copy(S, 2, MaxInt));
    end;
    if FileList.Count > 0 then
      FCompareRevisionCallBack(FileList, Revision.FRevision, Revision.FRevision + '~');
  finally
    FileList.Free;
  end;
end;

procedure TGitLogFrame.CompareWithPreviousRevisionActionUpdate(Sender: TObject);
begin
  CompareWithPreviousRevisionAction.Visible := Assigned(FCompareRevisionCallBack) and (Revisions.SelCount = 1);
  CompareWithPreviousRevisionAction.Enabled := Assigned(Revisions.Selected);
end;

procedure TGitLogFrame.FileCompareWithPreviousRevisionActionExecute(
  Sender: TObject);
var
  I: Integer;
  Revision: TRevision;
  FilesSL, FileList: TStringList;
  S: string;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  FileList := TStringList.Create;
  try
    FilesSL := TStringList(Revisions.Selected.Data);
    for I := 0 to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        S := FilesSL[I];
        if S[1] = 'M' then
          FileList.Add(Copy(S, 2, MaxInt));
      end;
    if FileList.Count > 0 then
      FCompareRevisionCallBack(FileList, Revision.FRevision, Revision.FRevision + '~');
  finally
    FileList.Free;
  end;
end;

procedure TGitLogFrame.FileCompareWithPreviousRevisionActionUpdate(Sender: TObject);
begin
  FileCompareWithPreviousRevisionAction.Visible := Assigned(FCompareRevisionCallBack) and (Revisions.SelCount = 1);
  FileCompareWithPreviousRevisionAction.Enabled := Assigned(Files.Selected);
end;

procedure TGitLogFrame.FileReverseMergeRevisionActionExecute(Sender: TObject);
var
  Revision: TRevision;
  PathName, PathMsg: string;
  FilesSL: TStringList;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  if (Revisions.Selected <> nil) and (Revisions.Selected.Data <> nil) then
  begin
    FilesSL := TStringList(Revisions.Selected.Data);
    PathName := System.Copy(FilesSL[Files.Selected.Index], 2, MaxInt);
    PathMsg := StringReplace(PathName, '/', '\', [rfReplaceAll]);
    if FRootRelativePath <> '' then
      Delete(PathMsg, 1, Length(FRootRelativePath));
    {
    if MessageDlg(Format(sRunFileReverseMergeRevision, [Revision.FRevision, FRootPath + PathMsg]),
      mtConfirmation, mbYesNo, 0) = mrYes then
      FReverseMergeCallBack(PathName, StrToInt(Revision.FRevision), StrToInt(Revision.FRevision) - 1);
    }
  end;
end;

procedure TGitLogFrame.FileReverseMergeRevisionActionUpdate(Sender: TObject);
var
  ActionEnabled: Boolean;
  FilesSL: TStringList;
begin
  FileReverseMergeRevisionAction.Visible := Assigned(FReverseMergeCallBack) and (Revisions.SelCount = 1);
  ActionEnabled := False;
  if FileReverseMergeRevisionAction.Visible and Assigned(Files.Selected) and (Files.SelCount = 1) then
  begin
    if FRootRelativePath = '' then
      ActionEnabled := True
    else
    if (Revisions.Selected <> nil) and (Revisions.Selected.Data <> nil) then
    begin
      FilesSL := TStringList(Revisions.Selected.Data);
      ActionEnabled := Pos(FRootRelativePath, System.Copy(FilesSL[Files.Selected.Index], 2, MaxInt)) = 1;
    end;
  end;
  FileReverseMergeRevisionAction.Enabled := ActionEnabled;
end;

procedure TGitLogFrame.FileSaveRevisionActionExecute(Sender: TObject);
var
  I: Integer;
  Revision: TRevision;
  FilesSL, FileList: TStringList;
  S, DestPath: string;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  FileList := TStringList.Create;
  try
    FilesSL := TStringList(Revisions.Selected.Data);
    for I := 0 to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        S := FilesSL[I];
        if S[1] <> 'D' then
          FileList.Add(Copy(S, 2, MaxInt));
      end;
    if (FileList.Count > 0) and
      SelectDirectory(Format(sSaveRevisionDestination, [Copy(Revision.FRevision, 1, 7)]), '', DestPath, [sdNewFolder, sdNewUI, sdValidateDir]) then
      FSaveRevisionCallBack(FileList, Revision.FRevision, DestPath);
  finally
    FileList.Free;
  end;
end;

procedure TGitLogFrame.FileSaveRevisionActionUpdate(Sender: TObject);
begin
  FileSaveRevisionAction.Visible := Assigned(FSaveRevisionCallBack) and (Revisions.SelCount = 1);
  FileSaveRevisionAction.Enabled := Assigned(Files.Selected);
end;

procedure TGitLogFrame.ReverseMergeRevisionActionExecute(Sender: TObject);
var
  Revision: TRevision;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  {
  if MessageDlg(Format(sRunReverseMergeRevision, [Revision.FRevision, FRootPath]),
    mtConfirmation, mbYesNo, 0) = mrYes then
    FReverseMergeCallBack('', StrToInt(Revision.FRevision), StrToInt(Revision.FRevision) - 1);
  }
end;

procedure TGitLogFrame.ReverseMergeRevisionActionUpdate(Sender: TObject);
begin
  ReverseMergeRevisionAction.Visible := Assigned(FReverseMergeCallBack) and (Revisions.SelCount = 1);
  ReverseMergeRevisionAction.Enabled := Assigned(Revisions.Selected);
end;

procedure TGitLogFrame.ReverseMergeToRevisionActionExecute(Sender: TObject);
var
  Revision: TRevision;
begin
  Revision := FVisibleRevisions[Revisions.Selected.Index];
  {
  if MessageDlg(Format(sRunReverseMergeToRevision, [FRootPath, Revision.FRevision]),
    mtConfirmation, mbYesNo, 0) = mrYes then
    FReverseMergeCallBack('', -1, StrToInt(Revision.FRevision));
  }
end;

procedure TGitLogFrame.ReverseMergeToRevisionActionUpdate(Sender: TObject);
begin
  ReverseMergeToRevisionAction.Visible := Assigned(FReverseMergeCallBack) and (Revisions.SelCount = 1);
  ReverseMergeToRevisionAction.Enabled := Assigned(Revisions.Selected);
end;

procedure TGitLogFrame.AddRevisions(Revision: string; Time: TDateTime;
  const Author, Comment, BugID: string; const Files: TStringList);
var
  TempRev: string;
  TempTime: string;
  FirstItem: Boolean;
begin
  FirstItem := Revisions.Items.Count = 0;
  TempRev := Revision;
  TempTime := DateTimeToStr(Time);
  FRevisionList.Add(TRevision.Create(TempRev, TempTime, Author, Comment, BugID, Files));
  AddRevisionToListView(FRevisionList.Last);
  if FirstItem then
    Revisions.Items[0].Selected := True;
end;

procedure TGitLogFrame.AddRevisionToListView(ARevision: TRevision);
var
  I, W, LastColumn: Integer;
  S: string;
begin
  if FVisibleRevisions.Count = 0 then
    InitRevisionColumnWidths;
  FVisibleRevisions.Add(ARevision);
  Revisions.Items.Count := FVisibleRevisions.Count;
  //emulation of LVSCW_AUTOSIZE
  LastColumn := High(FRevisionColumnWidths);
  if FBugIDColumnNo = -1 then
    Dec(LastColumn);
  for I := Low(FRevisionColumnWidths) to LastColumn do
  begin
    case I of
      0: W := Revisions.StringWidth(Copy(ARevision.FRevision, 1, 7));
      1: W := Revisions.StringWidth(ARevision.FAuthor);
      2: W := Revisions.StringWidth(ARevision.FTime);
      3: begin
           S := ARevision.FComment;
           //use only the first 259 chars for the column width, because a listview displays
           // only the first 259 chars - see http://support.microsoft.com/kb/321104
           if Length(S) > CBEMAXSTRLEN - 1 then
             S := Copy(S, 1, CBEMAXSTRLEN - 1);
           //the listview omitts line breaks and they would lead to a bigger string width
           S := StringReplace(S, #10, '', [rfReplaceAll]);
           W := Revisions.StringWidth(S);
         end;
      4: W := Revisions.StringWidth(ARevision.FBugID);
      else
        W := 0;
    end;
    if W > FRevisionColumnWidths[I] then
    begin
      FRevisionColumnWidths[I] := W;
      if Assigned(FRevisionColumns[I]) then
        FRevisionColumns[I].Width := W + 14;
    end;
  end;
end;

procedure TGitLogFrame.BeginUpdate;
begin
  Revisions.Items.BeginUpdate
end;

procedure TGitLogFrame.SearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      DoSearch(Search.Text);
      Key := 0;
    end;
    VK_ESCAPE:
    begin
      DoCancelSearch;
      Key := 0;
    end;
  end;
end;

procedure TGitLogFrame.SearchKeyPress(Sender: TObject; var Key: Char);
begin
  // Prevent the windows beep.
  if (Key = #27) or (Key = #13) then
    Key := #0;
end;

procedure TGitLogFrame.SearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Search.Text = '' then
    RestoreRevisions
  else
    DoSearch(Search.Text);
end;

procedure TGitLogFrame.StartAsync;
begin
end;

procedure TGitLogFrame.RangeClick(Sender: TObject);
var
  I, FromRevision, ToRevision: Integer;
  Update: Boolean;
begin
  {
  SvnRangeSelectDialog := TSvnRangeSelectDialog.Create(Application);
  try
    SvnRangeSelectDialog.FromSelectRevision.Text := IntToStr(FFromRevision);
    SvnRangeSelectDialog.ToCurrentRevision.Checked := FToRevision = -1;
    if FToRevision <> -1 then
      SvnRangeSelectDialog.ToSelectRevision.Text := IntToStr(FToRevision);
    Update := SvnRangeSelectDialog.ShowModal = mrOK;
    FromRevision := StrToIntDef(SvnRangeSelectDialog.FromSelectRevision.Text, -1);
    if SvnRangeSelectDialog.ToCurrentRevision.Checked then
      ToRevision := -1
    else
      ToRevision := StrToIntDef(SvnRangeSelectDialog.ToSelectRevision.Text, -2);
  finally
    SvnRangeSelectDialog.Free;
  end;
  if Update and (FromRevision >= 0) and (ToRevision >= -1) then
  begin
    FFromRevision := FromRevision;
    FToRevision := ToRevision;
    Revisions.Clear;
    for I := 1 to Revisions.Columns.Count - 1 do
      Revisions.Columns[I].Width := -2;
    Application.ProcessMessages;
    Files.Clear;
    Comment.Lines.Text := '';
    FVisibleRevisions.Clear;
    FRevisionList.Clear;
    Next.Enabled := False;
    Refresh.Enabled := False;
    Range.Enabled := False;
    if ToRevision <> -1 then
      Inc(ToRevision);//workaround that the last revision is shown too
    FLoadRevisionsCallBack(ToRevision, FromRevision, 0);
  end;
  }
end;

procedure TGitLogFrame.SearchRightButtonClick(Sender: TObject);
begin
  DoCancelSearch;
end;

procedure TGitLogFrame.SetShowBugIDColumn(const Value: Boolean);
var
  BugIDColumn: TListColumn;
begin
  if (FBugIDColumnNo <> -1) <> Value then
  begin
    if FBugIDColumnNo = -1 then
    begin
      BugIDColumn := TListColumn(Revisions.Columns.Insert(3));
      FBugIDColumnNo := 3;
      //BugIDColumn.Caption := sBugIDCaption;
      BugIDColumn.Width := -2;
      FRevisionColumns[4] := BugIDColumn;
      InitRevisionColumnWidths;
    end
    else
    begin
      FRevisionColumns[4] := nil;
      Revisions.Columns.Delete(FBugIDColumnNo);
      FBugIDColumnNo := -1;
    end;
  end;
end;

constructor TGitLogFrame.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  Name := Format('%s_%d', [Name, FUseCount]);
  Inc(FUseCount);
  FRevisionList := TObjectList<TRevision>.Create;
  FVisibleRevisions := TList<TRevision>.Create;
  FDoingSearch := False;
  FCount := DefaultRange;
  FFromRevision := 0;
  FToRevision := -1;
  FRevisionFiles := TStringList.Create;
  FExecutingSelectAll := False;
  FBugIDColumnNo := -1;
  Assert(Revisions.Columns.Count = 4);
  for I := 0 to 3 do
    FRevisionColumns[I] := Revisions.Columns[I];
  FRevisionColumns[4] := nil;
  InitRevisionColumnWidths;
end;

destructor TGitLogFrame.Destroy;
begin
  FRevisionFiles.Free;
  Revisions.Clear;//there is no sanity check in RevisionsData and freeing FVisibleRevisions would lead to an AV
  FVisibleRevisions.Free;
  FRevisionList.Free;
  inherited;
end;

procedure TGitLogFrame.DoCancelSearch;
begin
  Search.Clear;
  RestoreRevisions;
  Revisions.SetFocus;
end;

procedure TGitLogFrame.DoSearch(const Text: string);

  function CheckItem(const Item: TRevision): Boolean;
  var
    I: Integer;
    S: string;
  begin
    S := AnsiLowerCase(Text);
    Result := Pos(S, AnsiLowerCase(Item.FAuthor)) <> 0;
    if Result then
      Exit;
    Result := Pos(S, AnsiLowerCase(Item.FComment)) <> 0;
    if Result then
      Exit;
    if Item.FFiles <> nil then
    begin
      for I := 0 to Item.FFiles.Count - 1 do
      begin
        Result := Pos(S, AnsiLowerCase(Item.FFiles[I])) <> 0;
        if Result then
          Exit;
      end;
    end;
  end;

var
  I: Integer;
begin
  if Text <> '' then
  begin
    Revisions.Items.BeginUpdate;
    try
      FDoingSearch := True;
      Revisions.Clear;
      Files.Clear;
      FVisibleRevisions.Clear;
      Comment.Lines.Text := '';
      for I := 0 to FRevisionList.Count - 1 do
        if CheckItem(FRevisionList[I]) then
          AddRevisionToListView(FRevisionList[I]);
    finally
      Revisions.Items.EndUpdate
    end;
  end;
  Search.RightButton.Visible := True;
end;

procedure TGitLogFrame.EndUpdate;
begin
  Revisions.Items.EndUpdate;
end;

procedure TGitLogFrame.FilesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  TextColor: TColor;
  FilesSL: TStringList;
begin
  DefaultDraw := True;
  if Assigned(FFileColorCallBack) and
    (Revisions.Selected <> nil) and (Revisions.Selected.Data <> nil) then
  begin
    if Revisions.SelCount < 2 then
      FilesSL := TStringList(Revisions.Selected.Data)
    else
      FilesSL := FRevisionFiles;
    TextColor := FFileColorCallBack(FilesSL[Item.Index][1]);
    if TextColor <> clNone then
      Files.Canvas.Font.Color := TextColor;
  end;
end;

procedure TGitLogFrame.FilesData(Sender: TObject; Item: TListItem);
var
  S: string;
  FilesSL: TStringList;
begin
  if (Revisions.Selected <> nil) and (Revisions.Selected.Data <> nil) then
  begin
    if Revisions.SelCount < 2 then
      FilesSL := TStringList(Revisions.Selected.Data)
    else
      FilesSL := FRevisionFiles;
    S := System.Copy(FilesSL[Item.Index], 2, MaxInt);
    case FilesSL[Item.Index][1] of
      'M': Item.Caption := sModified;
      'A': Item.Caption := sAdded;
      'D': Item.Caption := sDeleted;
      'R': Item.Caption := sReplaced;
    end;
    Item.SubItems.Add(S);
  end;
end;

function TGitLogFrame.GetCommentColumn: Integer;
begin
  Result := 3;
end;

function TGitLogFrame.GetShowBugIDColumn: Boolean;
begin
  Result := FBugIDColumnNo <> -1;
end;

function TGitLogFrame.GetSvnEditState: TSvnEditState;
begin
  if Search.Focused then
    Result := ControlToSvnEditState(Search)
  else
  if Comment.Focused then
    Result := ControlToSvnEditState(Comment)
  else
  if Revisions.Focused and (Revisions.SelCount > 0) then
  begin
    Result := [sesCanCopy];
    if Revisions.MultiSelect then
      Include(Result, sesCanSelectAll);
  end
  else
  if Files.Focused and (Files.SelCount > 0) then
  begin
    Result := [sesCanCopy];
    if Files.MultiSelect then
      Include(Result, sesCanSelectAll);
  end
  else
    Result := [];
end;

procedure TGitLogFrame.InitRevisionColumnWidths;
var
  I, LastColumn: Integer;
begin
  LastColumn := High(FRevisionColumnWidths);
  if FBugIDColumnNo = -1 then
    Dec(LastColumn);
  for I := Low(FRevisionColumnWidths) to LastColumn do
    if Assigned(FRevisionColumns[I]) then
    begin
      FRevisionColumnWidths[I] := Revisions.StringWidth(FRevisionColumns[I].Caption);
      FRevisionColumns[I].Width := FRevisionColumnWidths[I] + 14;
    end;
end;

procedure TGitLogFrame.NextClick(Sender: TObject);
var
  First: string;
begin
  FCount := FCount + DefaultRange;
  First := FRevisionList[FRevisionList.Count - 1].FRevision;
  Next.Enabled := False;
  Refresh.Enabled := False;
  Range.Enabled := False;
  FLoadRevisionsCallBack(First, '', DefaultRange);
end;

procedure TGitLogFrame.NextCompleted;
begin
  Next.Enabled := True;
  Refresh.Enabled := True;
  Range.Enabled := True;
end;

function TGitLogFrame.PerformEditAction(AEditAction: TSvnEditAction): Boolean;
var
  I, J: Integer;
  S, ColumnContent: string;
  SL, FilesSL: TStringList;
  FirstItem: Boolean;
begin
  if Search.Focused then
    Result := PerformDefaultSvnEditAction(Search, AEditAction)
  else
  if Comment.Focused then
    Result := PerformDefaultSvnEditAction(Comment, AEditAction)
  else
  if Revisions.Focused then
  begin
    if AEditAction = seaCopy then
    begin
      SL := TStringList.Create;
      try
        FirstItem := True;
        for I := 0 to Revisions.Items.Count - 1 do
          if Revisions.Items[I].Selected then
          begin
            if not FirstItem then
              SL.Add('');
            FirstItem := False;
            for J := 0 to Revisions.Columns.Count - 1 do
            begin
              if J = 0 then
                ColumnContent := Revisions.Items[I].Caption
              else
                ColumnContent := Revisions.Items[I].SubItems[J - 1];
              //the logmessage should start on it's own line and needs a line break correction
              if J = GetCommentColumn then
                ColumnContent := #13#10 + AdjustLineBreaks(ColumnContent);
              SL.Add(Format('%s: %s', [Revisions.Columns[J].Caption, ColumnContent]));
            end;
            SL.Add('----');
            if Assigned(Revisions.Items[I].Data) then
            begin
              FilesSL := TStringList(Revisions.Items[I].Data);
              //TODO: Duplicate code
              for J := 0 to Pred(FilesSL.Count) do
              begin
                ColumnContent := System.Copy(FilesSL[J], 2, MaxInt);
                if FilesSL[J] <> '' then
                begin
                  case FilesSL[J][1] of
                    'M': S := sModified;
                    'A': S := sAdded;
                    'D': S := sDeleted;
                    'R': S := sReplaced;
                    else
                      S := '';
                  end;
                end;
                SL.Add(Format('%s: %s', [S, ColumnContent]));
              end;
            end;
          end;
        Clipboard.AsText := SL.Text;
      finally
        SL.Free;
      end;
      Result := True;
    end
    else
    if AEditAction = seaSelectAll then
    begin
      Files.Items.Clear;
      Comment.Clear;
      FExecutingSelectAll := True;
      try
        Revisions.SelectAll;
      finally
        FExecutingSelectAll := False;
      end;
      RevisionsSelectItem(nil, nil, False);
      Result := True;
    end
    else
      Result := False;
  end
  else
  if Files.Focused then
  begin
    if AEditAction = seaCopy then
    begin
      SL := TStringList.Create;
      try
        for I := 0 to Files.Items.Count - 1 do
          if Files.Items[I].Selected then
            SL.Add(Files.Items[I].SubItems[0]);
        Clipboard.AsText := SL.Text;
      finally
        SL.Free;
      end;
      Result := True;
    end
    else
    if AEditAction = seaSelectAll then
    begin
      Files.SelectAll;
      Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TGitLogFrame.RefreshClick(Sender: TObject);
var
  I: Integer;
begin
  Revisions.Clear;
  for I := 1 to Revisions.Columns.Count - 1 do
    Revisions.Columns[I].Width := -2;
  Application.ProcessMessages;
  Files.Clear;
  Comment.Lines.Text := '';
  FVisibleRevisions.Clear;
  FRevisionList.Clear;
  Next.Enabled := False;
  Refresh.Enabled := False;
  Range.Enabled := False;
  //TODO: take care of the selected range?
  FLoadRevisionsCallBack('', '', FCount);
end;

procedure TGitLogFrame.RestoreRevisions;
var
  I: Integer;
begin
  if FDoingSearch then
  begin
    FDoingSearch := False;
    Revisions.Items.BeginUpdate;
    try
      Revisions.Items.Clear;
      FVisibleRevisions.Clear;
      for I := 0 to FRevisionList.Count - 1 do
        AddRevisionToListView(FRevisionList[I]);
    finally
      Revisions.Items.EndUpdate;
    end;
    Search.RightButton.Visible := False;
  end;
end;

procedure TGitLogFrame.RevisionsData(Sender: TObject; Item: TListItem);
var
  Revision: TRevision;
begin
  Revision := FVisibleRevisions[Item.Index];
  Item.Caption := Copy(Revision.FRevision, 1, 7);
  Item.SubItems.Add(Revision.FAuthor);
  Item.SubItems.Add(Revision.FTime);
  Item.SubItems.Add(Revision.FComment);
  if FBugIDColumnNo <> -1 then
    Item.SubItems.Add(Revision.FBugID);
  Item.Data := Revision.FFiles;
end;

procedure TGitLogFrame.RevisionsDataStateChange(Sender: TObject; StartIndex,
  EndIndex: Integer; OldState, NewState: TItemStates);
begin
  RevisionsSelectItem(nil, nil, False);
end;

procedure TGitLogFrame.RevisionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  I, J, W, Idx, ActionValue: Integer;
  FilesSL: TStringList;
  S: string;
  NewAction: Char;
  ColumnWidths: array [0..1] of Integer;
begin
  if not FExecutingSelectAll then
  begin
    Files.Items.BeginUpdate;
    try
      Files.Items.Count := 0;
      if Revisions.Selected <> nil then
      begin
        if Revisions.SelCount < 2 then
          Comment.Lines.Text := Revisions.Selected.SubItems[2]
        else
          Comment.Clear;
        if Revisions.Selected.Data <> nil then
        begin
          if Revisions.SelCount < 2 then
            FilesSL := TStringList(Revisions.Selected.Data)
          else
          begin
            FRevisionFiles.Clear;
            FRevisionFiles.Sorted := True;
            for I := 0 to Revisions.Items.Count - 1 do
              if Revisions.Items[I].Selected and Assigned(FVisibleRevisions[I].FFiles) then
              begin
                FilesSL := FVisibleRevisions[I].FFiles;
                for J := 0 to FilesSL.Count - 1 do
                begin
                  S := ' ' + Copy(FilesSL[J], 2, MaxInt);
                  case FilesSL[J][1] of
                    'M': ActionValue := 1;
                    'A': ActionValue := 2;
                    'D': ActionValue := 4;
                    'R': ActionValue := 8;
                    else
                      ActionValue := 0;
                  end;
                  Idx := FRevisionFiles.IndexOf(S);
                  if Idx <> -1 then
                  begin
                    if ActionValue <> 0 then
                      FRevisionFiles.Objects[Idx] := TObject(Integer(FRevisionFiles.Objects[Idx]) or ActionValue);
                  end
                  else
                    FRevisionFiles.AddObject(S, TObject(ActionValue));
                end;
              end;
            FRevisionFiles.Sorted := False;
            for I := 0 to FRevisionFiles.Count - 1 do
            begin
              case Integer(FRevisionFiles.Objects[I]) of
                1:   NewAction := 'M';
                2,3: NewAction := 'A';
                4:   NewAction := 'D';
                8:   NewAction := 'R';
                else
                  NewAction := ' ';
              end;
              if NewAction <> ' ' then
                FRevisionFiles[I] := NewAction + Copy(FRevisionFiles[I], 2, MaxInt);
            end;
            FilesSL := FRevisionFiles;
          end;
          Files.Items.Count := FilesSL.Count;
          //get max column widths
          for I := Low(ColumnWidths) to High(ColumnWidths) do
            ColumnWidths[I] := 0;
          for I := 0 to FilesSL.Count - 1 do
          begin
            S := System.Copy(FilesSL[I], 2, MaxInt);
            W := Files.StringWidth(S);
            if W > ColumnWidths[1] then
              ColumnWidths[1] := W;
            case FilesSL[I][1] of
              'M': S := sModified;
              'A': S := sAdded;
              'D': S := sDeleted;
              'R': S := sReplaced;
              else
                S := Files.Columns[0].Caption;
            end;
            W := Files.StringWidth(S);
            if W > ColumnWidths[0] then
              ColumnWidths[0] := W;
          end;
          //set columns widths including a margin (using 14 as margin, because this is the observed
          // margin with themes (no themes: first column 8, other columns 12; themes: first 10, other 14)
          for I := Low(ColumnWidths) to High(ColumnWidths) do
            Files.Columns[I].Width := ColumnWidths[I] + 14;
        end;
      end;
    finally
      Files.Items.EndUpdate;
    end;
  end;
end;

initialization
  TGitLogFrame.FUseCount := 0;
end.
