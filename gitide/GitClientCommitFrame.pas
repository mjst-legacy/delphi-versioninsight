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
unit GitClientCommitFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, GitClient, Generics.Collections, ActnList, Menus,
  ExtCtrls, GitUITypes, GitUIUtils, Clipbrd, Generics.Defaults, GitImages, GitTree;

type
  PSvnListViewItem = ^TSvnListViewItem;
  TSvnListViewItem = class
  protected
    FCopied: Boolean;
    FDirectory: Boolean;
    FPathName: string;
    FTextStatus: TGitStatus;
    FChecked: Boolean;
    FVisible: Boolean;
    procedure SetTextStatus(Value: TGitStatus);
  public
    constructor Create(const APathName: string; ATextStatus: TGitStatus;
      ADirectory, ACopied: Boolean);
    procedure NewValues(const APathName: string; ATextStatus: TGitStatus;
      ADirectory, ACopied: Boolean);
    property Copied: Boolean read FCopied;
    property Directory: Boolean read FDirectory;
    property PathName: string read FPathName;
    property TextStatus: TGitStatus read FTextStatus write SetTextStatus;
    property Checked: Boolean read FChecked write FChecked;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TCommitCallBack = procedure(const CommitList: TStringList;
    const Comment: string; const RecentComments: TStringList) of object;
  TCloseCallBack = procedure of object;
  TDiffCallBack = procedure(const FileName: string) of object;
  TRevertCallBack = function(const FileName: string; ARecursive: Boolean; var ANewTextStatus: TGitStatus): Boolean of object;
  TAddCallBack = function(const FileName: string): Boolean of object;
  TResolveCallBack = procedure(const FileName: string) of object;
  TGetFileStatusCallBack = procedure(const FileName: string; var SvnListViewItem: TSvnListViewItem) of object;
  TFileColorCallBack = function(AItem: TSvnListViewItem): TColor of object;
  TRefreshCallBack = procedure of object;

  TGitCommitFrame = class(TFrame)
    Label1: TLabel;
    Location: TLabel;
    Comment: TMemo;
    Recent: TButton;
    Commit: TButton;
    Files: TListView;
    UnversionedFiles: TCheckBox;
    Externals: TCheckBox;
    CommitMenu: TPopupMenu;
    CommitActions: TActionList;
    DiffAction: TAction;
    Difference1: TMenuItem;
    RevertAction: TAction;
    Revert1: TMenuItem;
    CheckAll: TCheckBox;
    UpperPanel: TPanel;
    Splitter1: TSplitter;
    LowerPanel: TPanel;
    Label2: TLabel;
    AddAction: TAction;
    Add1: TMenuItem;
    ResolveAction: TAction;
    ResolveAction1: TMenuItem;
    SelCountTotalCount: TLabel;

    procedure CommitClick(Sender: TObject);
    procedure UnversionedFilesClick(Sender: TObject);
    procedure ExternalsClick(Sender: TObject);
    procedure FilesItemChecked(Sender: TObject; Item: TListItem);
    procedure FilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure DoDiff(Sender: TObject);
    procedure RevertActionExecute(Sender: TObject);
    procedure CheckAllClick(Sender: TObject);
    procedure FilesDblClick(Sender: TObject);
    procedure RecentClick(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure RevertActionUpdate(Sender: TObject);
    procedure AddActionExecute(Sender: TObject);
    procedure DiffActionUpdate(Sender: TObject);
    procedure ResolveActionUpdate(Sender: TObject);
    procedure ResolveActionExecute(Sender: TObject);
    procedure LowerPanelResize(Sender: TObject);
    procedure FilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FilesCustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure FilesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure CommentChange(Sender: TObject);
  protected
    FAlternativeLayoutEnabled: Boolean;
    FAllowEmptyComment: Boolean;
    FCommitCallBack: TCommitCallBack;
    FCloseCallBack: TCloseCallBack;
    FDiffCallBack: TDiffCallBack;
    FFileColorCallBack: TFileColorCallBack;
    FRevertCallBack: TRevertCallBack;
    FAddCallBack: TAddCallBack;
    FResolveCallBack: TResolveCallBack;
    FGetFileStatusCallBack: TGetFileStatusCallBack;
    FRefreshCallBack: TRefreshCallBack;
    FExecutingCheckAllClick: Boolean;
    FExecutingRefresh: Boolean;
    FExecutingSelectedCheck: Boolean;
    FExecutingUnversionedParentCheck: Boolean;
    FItemList: TList<TSvnListViewItem>;
    FIndexList: TList<Integer>;
    FRefreshItemList: TObjectList<TSvnListViewItem>;
    FSetAutoSizeColumnWidth: Boolean;
    FSortColumns: array of Integer;
    FSortOrder: Boolean;
    FRecentComments: TStringList;
    FSupportsExternals: Boolean;
    FURL: string;
    FNoFiles: Boolean;
    FUpdateCount: Integer;
    FKeepOpenAfterCommit: Boolean;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure DoRefresh;
    function GetSvnEditState: TSvnEditState;
    procedure SetRecentComments(Value: TStringList);
    procedure Notify(Sender: TObject; const Item: TSvnListViewItem;
      Action: TCollectionNotification);
    procedure RebuildList;
    function ItemShown(const SvnListItem: TSvnListViewItem): Boolean;
    procedure SetAllowEmptyComment(const Value: Boolean);
    procedure SetSupportsExternal(const Value: Boolean);
    procedure SetURL(const AValue: string);
    function StatusKindStrEx(Status: TGitStatus; ACopied: Boolean): string;
    procedure UpdateCommitButton;
    procedure UpdateCountLabel;
    procedure UpdateListView(const SvnListItem: TSvnListViewItem; ItemIndex: Integer);
    procedure ResizeStuff;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const SvnListItem: TSvnListViewItem);
    procedure BeginUpdate;
    procedure EnableAlternativeLayout;
    procedure EndUpdate;
    procedure CheckForNoFilesVisible;
    procedure RefreshAdd(const SvnListItem: TSvnListViewItem);
    function Found(const FileName: string): Boolean;
    function PerformEditAction(AEditAction: TSvnEditAction): Boolean;
    procedure HandleMissingFiles(ARefresh: Boolean = False);
    property AddCallBack: TAddCallBack read FAddCallBack write FAddCallBack;
    property AllowEmptyComment: Boolean read FAllowEmptyComment write SetAllowEmptyComment;
    property CloseCallBack: TCloseCallBack read FCloseCallBack write FCloseCallBack;
    property CommitCallBack: TCommitCallBack read FCommitCallBack write FCommitCallBack;
    property DiffCallBack: TDiffCallBack read FDiffCallBack write FDiffCallBack;
    property FileColorCallBack: TFileColorCallBack read FFileColorCallBack write FFileColorCallBack;
    property KeepOpenAfterCommit: Boolean read FKeepOpenAfterCommit write FKeepOpenAfterCommit;
    property RevertCallBack: TRevertCallBack read FRevertCallBack write FRevertCallBack;
    property ResolveCallBack: TResolveCallBack read FResolveCallBack write FResolveCallBack;
    property GetFileStatusCallBack: TGetFileStatusCallBack read FGetFileStatusCallBack write FGetFileStatusCallBack;
    property RefreshCallBack: TRefreshCallBack read FRefreshCallBack write FRefreshCallBack;
    property RecentComments: TStringList read FRecentComments write SetRecentComments;
    property SupportsExternals: Boolean read FSupportsExternals write SetSupportsExternal;
    property SvnEditState: TSvnEditState read GetSvnEditState;
    property URL: string read FURL write SetURL;
  end;

implementation

uses {HgClient, }GitUIConst, GitClientRecentComments;

{$R *.dfm}

type
  TSvnListViewItemPathComparer = class(TInterfacedObject, IComparer<TSvnListViewItem>)
    function Compare(const Left, Right: TSvnListViewItem): Integer;
  end;

function ColumnSort(Item1, Item2: TListItem; Param: LParam): Integer; stdcall;
var
  SvnCommitFrame: TGitCommitFrame;
  S1, S2: string;
  I, OrderColumn: Integer;
begin
  SvnCommitFrame := TGitCommitFrame(Param);
  Result := 0;
  for I := Low(SvnCommitFrame.FSortColumns) to High(SvnCommitFrame.FSortColumns) do
  begin
    OrderColumn := SvnCommitFrame.FSortColumns[I];
    if OrderColumn = 0 then
    begin
      S1 := AnsiLowerCase(Item1.Caption);
      S2 := AnsiLowerCase(Item2.Caption);
    end
    else
    begin
      S1 := AnsiLowerCase(Item1.SubItems[OrderColumn - 1]);
      S2 := AnsiLowerCase(Item2.SubItems[OrderColumn - 1]);
    end;
    if S1 = S2 then
      Result := 0
    else if (S1 < S2) xor SvnCommitFrame.FSortOrder then
      Result := 1
    else
      Result := -1;
    if Result <> 0 then
      Break;
  end;
end;

procedure TGitCommitFrame.Add(const SvnListItem: TSvnListViewItem);
var
  ItemIndex: Integer;
begin
  ItemIndex := FItemList.Add(SvnListItem);
  UpdateListView(SvnListItem, ItemIndex);
end;

procedure TGitCommitFrame.AddActionExecute(Sender: TObject);

  function CheckAddParents(ASvnListViewItem: TSvnListViewItem): Boolean;
  var
    I: Integer;
    S: string;
    UnversionedDirectories, UnversionedParents: TList<TSvnListViewItem>;
    FoundUnversionedParent: Boolean;
  begin
    Result := True;
    UnversionedParents := TList<TSvnListViewItem>.Create;
    UnversionedDirectories := TList<TSvnListViewItem>.Create;
    try
      for I := 0 to FItemList.Count - 1 do
        if FItemList[I].Directory and (FItemList[I].TextStatus = gsUnversioned) then
          UnversionedDirectories.Add(FItemList[I]);
      FoundUnversionedParent := UnversionedDirectories.Count > 0;
      S := ExcludeTrailingPathDelimiter(ExtractFilePath(ASvnListViewItem.PathName));
      while FoundUnversionedParent do
      begin
        FoundUnversionedParent := False;
        for I := 0 to UnversionedDirectories.Count - 1 do
          if UnversionedDirectories[I].PathName = S then
          begin
            FoundUnversionedParent := True;
            UnversionedParents.Add(UnversionedDirectories[I]);
            S := ExcludeTrailingPathDelimiter(ExtractFilePath(S));
            Break;
          end;
      end;
      if UnversionedParents.Count > 0 then
      begin
        //sort list to make sure the parents are added first
        if UnversionedParents.Count > 1 then
          UnversionedParents.Sort(TSvnListViewItemPathComparer.Create);
        for I := 0 to UnversionedParents.Count - 1 do
          if FAddCallBack(UnversionedParents[I].FPathName) then
          begin
            UnversionedParents[I].FTextStatus := gsAdded;
            UnversionedParents[I].Checked := True;
          end
          else
          begin
            Result := False;
            Break;
          end;
      end;
    finally
      UnversionedDirectories.Free;
      UnversionedParents.Free;
   end;
  end;

var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
begin
  if Files.SelCount > 0 then
  begin
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if (SvnListViewItem.FTextStatus = gsUnversioned) and CheckAddParents(SvnListViewItem) then
        begin
          if FAddCallBack(SvnListViewItem.FPathName) then
          begin
            SvnListViewItem.FTextStatus := gsAdded;
            SvnListViewItem.Checked := True;
          end;
        end;
      end;
    RebuildList;
  end;
end;

procedure TGitCommitFrame.AddActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  AddState: Boolean;
begin
  if Files.SelCount > 0 then
  begin
    AddState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
        if FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]].FTextStatus = gsUnversioned then
        begin
          AddState := True;
          Break;
        end;
    AddAction.Visible := AddState;
  end
  else
    AddAction.Visible := False;
end;

procedure TGitCommitFrame.BeginUpdate;
var
  I: Integer;
begin
  Files.Items.BeginUpdate;
  if FUpdateCount = 0 then
  begin
    FSetAutoSizeColumnWidth := False;
    for I := 0 to Files.Columns.Count - 1 do
      Files.Columns.Items[I].Width := Files.StringWidth(Files.Columns[I].Caption) + 14;
  end;
  Inc(FUpdateCount);
end;

procedure TGitCommitFrame.CheckAllClick(Sender: TObject);
var
  I: Integer;
  Checked: Boolean;
begin
  FExecutingCheckAllClick := True;
  try
  if CheckAll.State <> cbGrayed then
  begin
    Checked := CheckAll.State = cbChecked;
    for I := 0 to Files.Items.Count - 1 do
      Files.Items[I].Checked := Checked;
  end;
  UpdateCommitButton;
    UpdateCountLabel;
  finally
    FExecutingCheckAllClick := False;
  end;
end;

procedure TGitCommitFrame.CheckForNoFilesVisible;
begin
  FNoFiles := Files.Items.Count = 0;
  UpdateCountLabel;
end;

procedure TGitCommitFrame.CMRelease(var Message: TMessage);
begin
  CloseCallBack;
end;

procedure TGitCommitFrame.CommentChange(Sender: TObject);
begin
  if not FAllowEmptyComment then
    UpdateCommitButton;
end;

procedure TGitCommitFrame.CommitClick(Sender: TObject);
var
  CommitList, AddList: TStringList;
  I, Idx: Integer;
  SortedIndexList: TList<Integer>;
  KeepOpen: Boolean;
begin
  KeepOpen := FKeepOpenAfterCommit;
  if KeepOpen then
  begin
    KeepOpen := False;
    for I := 0 to FItemList.Count - 1 do
      if FItemList[I].Visible and not FItemList[I].Checked then
      begin
        KeepOpen := True;
        Break;
      end;
  end;
  if Comment.Text <> '' then
  begin
    Idx := FRecentComments.IndexOf(Comment.Text);
    if Idx <> -1 then
      FRecentComments.Move(Idx, 0)
    else
      FRecentComments.Insert(0, Comment.Text);
  end;
  CommitList := TStringList.Create;
  AddList := TStringList.Create;
  SortedIndexList := TList<Integer>.Create;
  try
    SortedIndexList.AddRange(FIndexList);
    SortedIndexList.Sort;
    for I := 0 to FItemList.Count - 1 do
      if FItemList[I].Checked and SortedIndexList.BinarySearch(I, Idx) then
      begin
        CommitList.Add(FItemList[I].PathName);
        if FItemList[I].TextStatus = gsUnversioned then
          AddList.Add(FItemList[I].PathName);
      end;
    if AddList.Count > 0 then
    begin
      //sort list to make sure the parents are added first
      AddList.Sort;
      for I := 0 to AddList.Count - 1 do
        FAddCallBack(AddList[I]);
    end;
    FCommitCallBack(CommitList, Comment.Text, FRecentComments);
  finally
    SortedIndexList.Free;
    AddList.Free;
    CommitList.Free;
  end;
  if KeepOpen then
  begin
    Comment.Clear;
    DoRefresh;
  end
  else
    PostMessage(Handle, CM_Release, 0, 0)
end;

constructor TGitCommitFrame.Create(AOwner: TComponent);
begin
  inherited;
  FAlternativeLayoutEnabled := False;
  FItemList := TList<TSvnListViewItem>.Create;
  FItemList.OnNotify := Notify;
  FIndexList := TList<Integer>.Create;
  FRefreshItemList := TObjectList<TSvnListViewItem>.Create;
  SetLength(FSortColumns, 1);
  FSortColumns[0] := 0;
  FSortOrder := True;
  FRecentComments := TStringList.Create;
  FExecutingCheckAllClick := False;
  FExecutingRefresh := False;
  FExecutingSelectedCheck := False;
  FExecutingUnversionedParentCheck := False;
  FNoFiles := False;
  FAllowEmptyComment := True;
  FSupportsExternals := True;
  FSetAutoSizeColumnWidth := False;
  FUpdateCount := 0;
end;

destructor TGitCommitFrame.Destroy;
begin
  FRefreshItemList.Free;
  FIndexList.Free;
  FItemList.Free;
  FRecentComments.Free;
  inherited;
end;

procedure TGitCommitFrame.DiffActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  DiffState: Boolean;
  SvnListViewItem: TSvnListViewItem;
begin
  if Files.SelCount > 0 then
  begin
    DiffState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if not (SvnListViewItem.FTextStatus in [gsUnversioned, gsAdded, gsDeleted])
          and not SvnListViewItem.Directory then
        begin
          DiffState := True;
          Break;
        end;
      end;
    DiffAction.Enabled := DiffState;
  end
  else
    DiffAction.Enabled := False;
end;

procedure TGitCommitFrame.DoDiff(Sender: TObject);
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
begin
  if Files.SelCount > 0 then
  begin
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if not (SvnListViewItem.FTextStatus in [gsUnversioned, gsAdded, gsDeleted])
          and not SvnListViewItem.Directory then
          DiffCallBack(SvnListViewItem.FPathName);
      end;
  end;
end;

procedure TGitCommitFrame.DoRefresh;
var
  I: Integer;
  SvnListViewItem: TSvnListViewItem;
  UncheckedItems: TStringList;
begin
  if not FExecutingRefresh then
  begin
    FExecutingRefresh := True;
    try
      Cursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      try
        FRefreshItemList.Clear;
        FRefreshCallBack;
        UncheckedItems := TStringList.Create;
        BeginUpdate;
        try
          UncheckedItems.Sorted := True;
          for I := 0 to FItemList.Count - 1 do
            if not FItemList[I].Checked then
              UncheckedItems.Add(FItemList[I].PathName);
          FIndexList.Clear;
          FItemList.Clear;
          Files.Items.Clear;
          for I := 0 to FRefreshItemList.Count - 1 do
          begin
            SvnListViewItem := FRefreshItemList[I];
            Add(TSvnListViewItem.Create(SvnListViewItem.PathName, SvnListViewItem.TextStatus,
              SvnListViewItem.Directory, SvnListViewItem.Copied));
          end;
          for I := 0 to Files.Items.Count - 1 do
          begin
            SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
            if UncheckedItems.IndexOf(SvnListViewItem.PathName) <> -1 then
              Files.Items[I].Checked := False;
          end;
          CheckForNoFilesVisible;
        finally
          EndUpdate;
          UncheckedItems.Free;
        end;
      finally
        Screen.Cursor := Cursor;
      end;
    finally
      FExecutingRefresh := False;
    end;
  end;
end;

procedure TGitCommitFrame.EnableAlternativeLayout;
var
  PixelsPerInch: Integer;
begin
  FAlternativeLayoutEnabled := True;
  //PixelsPerInch := Screen.PixelsPerInch;
  PixelsPerInch := 96;
  UpperPanel.Align := alTop;
  UpperPanel.Height := MulDiv(163, PixelsPerInch, 96);

  Label2.Parent := UpperPanel;
  Label2.Top := MulDiv(56, PixelsPerInch, 96);

  Comment.Parent := UpperPanel;
  Comment.Top := MulDiv(72, PixelsPerInch, 96);
  Comment.Height := UpperPanel.Height - Comment.Top;

  Recent.Parent := UpperPanel;
  Recent.Top := MulDiv(43, PixelsPerInch, 96);
  Recent.Anchors := [akTop, akRight];

  LowerPanel.Align := alClient;
  Splitter1.Top := LowerPanel.Top - 1;
  Splitter1.Align := alTop;

  Files.Parent := LowerPanel;
  Files.Top := MulDiv(6, PixelsPerInch, 96);
  Files.Height := MulDiv(UnversionedFiles.Top - 12, PixelsPerInch, 96);

  SelCountTotalCount.Top := UnversionedFiles.Top;
  SelCountTotalCount.Anchors := [akRight, akBottom];
end;

procedure TGitCommitFrame.EndUpdate;
var
  I: Integer;
begin
  Files.CustomSort(@ColumnSort, LPARAM(Self));
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    if FSetAutoSizeColumnWidth then
    begin
      for I := 0 to Files.Columns.Count - 1 do
        Files.Columns.Items[I].Width := -1;
      FSetAutoSizeColumnWidth := False;
    end;
  end;
  Files.Items.EndUpdate;
end;

procedure TGitCommitFrame.ExternalsClick(Sender: TObject);
begin
  RebuildList;
end;

procedure TGitCommitFrame.FilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if (Length(FSortColumns) > 0) and (FSortColumns[0] = Column.Index) then
    FSortOrder := not FSortOrder
  else
  begin
    case Column.Index of
      0: begin
           SetLength(FSortColumns, 1);
           FSortColumns[0] := 0;//Name
         end;
      1: begin
           SetLength(FSortColumns, 2);
           FSortColumns[0] := 1;//Path
           FSortColumns[1] := 0;//Name
         end;
      2: begin
           SetLength(FSortColumns, 3);
           FSortColumns[0] := 2;//Ext
           FSortColumns[1] := 1;//Path
           FSortColumns[2] := 0;//Name
         end;
      3: begin
           SetLength(FSortColumns, 3);
           FSortColumns[0] := 3;//Status
           FSortColumns[1] := 1;//Path
           FSortColumns[2] := 0;//Name
         end;
      else
      begin
        SetLength(FSortColumns, 1);
        FSortColumns[0] := 0;//Name
      end;
    end;
    FSortColumns[0] := Column.Index;
    FSortOrder := True;
  end;
  Files.CustomSort(@ColumnSort, LPARAM(Self));
end;

procedure TGitCommitFrame.FilesCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  ItemRect: TRect;
  S: string;
begin
  if FNoFiles then
  begin
    ItemRect := ARect;
    ItemRect.Top := ItemRect.Top + (ItemRect.Bottom div 3);
    S := sNoFiles;
    Files.Canvas.TextRect(ItemRect, S, [tfCenter, tfWordBreak]);
  end;
  DefaultDraw := not FNoFiles;
end;

procedure TGitCommitFrame.FilesCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  TextColor: TColor;
begin
  DefaultDraw := True;
  if Assigned(FFileColorCallBack) then
  begin
    TextColor := FFileColorCallBack(FItemList[FIndexList[Integer(Item.Data) - 1]]);
    if TextColor <> clNone then
      Files.Canvas.Font.Color := TextColor;
  end;
end;

procedure TGitCommitFrame.FilesDblClick(Sender: TObject);
begin
  DoDiff(Sender);
end;

procedure TGitCommitFrame.FilesItemChecked(Sender: TObject; Item: TListItem);

  procedure UnversionedParentCheck(ASvnListViewItem: TSvnListViewItem);
  var
    I: Integer;
    TestSvnListViewItem: TSvnListViewItem;
    S: string;
    UnversionedDirectories: TList<TListItem>;
    FoundUnversionedParent: Boolean;
  begin
    if Item.Checked then
    begin
      //check all unversioned parent directories
      UnversionedDirectories := TList<TListItem>.Create;
      try
        for I := 0 to Files.Items.Count - 1 do
          if not Files.Items[I].Checked and (Files.Items[I].Data <> nil) then
          begin
            TestSvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
            if TestSvnListViewItem.Directory and
              (TestSvnListViewItem.TextStatus = gsUnversioned) then
              UnversionedDirectories.Add(Files.Items[I]);
          end;
        FoundUnversionedParent := UnversionedDirectories.Count > 0;
        S := ExcludeTrailingPathDelimiter(ExtractFilePath(ASvnListViewItem.PathName));
        while FoundUnversionedParent do
        begin
          FoundUnversionedParent := False;
          for I := 0 to UnversionedDirectories.Count - 1 do
          begin
            TestSvnListViewItem := FItemList[FIndexList[Integer(UnversionedDirectories[I].Data) - 1]];
            if TestSvnListViewItem.PathName = S then
            begin
              FoundUnversionedParent := True;
              UnversionedDirectories[I].Checked := True;
              S := ExcludeTrailingPathDelimiter(ExtractFilePath(S));
              Break;
            end;
          end;
        end;
      finally
        UnversionedDirectories.Free;
      end;
    end
    else
    if not Item.Checked and ASvnListViewItem.Directory then
    begin
      //uncheck all unversioned child files and directories
      S := ASvnListViewItem.PathName;
      for I := 0 to Files.Items.Count - 1 do
        if Files.Items[I].Checked and (Files.Items[I].Data <> nil) then
        begin
          TestSvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
          if (TestSvnListViewItem.TextStatus = gsUnversioned) and
            (Pos(S, TestSvnListViewItem.PathName) = 1) then
            Files.Items[I].Checked := False;
        end;
    end;
  end;

var
  I, StartIdx: Integer;
  Checked: Boolean;
begin
  if Item.Data <> nil then
    FItemList[FIndexList[Integer(Item.Data) - 1]].Checked := Item.Checked;
  if not FExecutingCheckAllClick and not FExecutingUnversionedParentCheck then
  begin
    if (Item.Data <> nil) and
      (FItemList[FIndexList[Integer(Item.Data) - 1]].TextStatus = gsUnversioned) then
    begin
      FExecutingUnversionedParentCheck := True;
      try
        UnversionedParentCheck(FItemList[FIndexList[Integer(Item.Data) - 1]]);
      finally
        FExecutingUnversionedParentCheck := False;
      end;
    end;
    //when a selected item is checked/unchecked then check/uncheck all other selected items
    if not FExecutingSelectedCheck and (Files.SelCount > 1) and Item.Selected then
    begin
      FExecutingSelectedCheck := True;
      try
        Checked := Item.Checked;
        StartIdx := Files.Selected.Index;
        for I := StartIdx to Files.Items.Count - 1 do
          if Files.Items[I].Selected and (Files.Items[I] <> Item) then
            Files.Items[I].Checked := Checked;
      finally
        FExecutingSelectedCheck := False;
      end;
    end;
    Checked := Item.Checked;
    for I := 0 to Files.Items.Count - 1 do
      if Files.Items[I].Checked <> Checked then
      begin
        CheckAll.State := cbGrayed;
        UpdateCommitButton;
        UpdateCountLabel;
        Exit;
      end;
    if Item.Checked then
      CheckAll.State := cbChecked
    else
      CheckAll.State := cbUnChecked;
    UpdateCommitButton;
    UpdateCountLabel;
  end;
end;

procedure TGitCommitFrame.FilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DoDiff(Sender);
end;

function TGitCommitFrame.Found(const FileName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FItemList.Count - 1 do
    if SameText(FItemList[I].PathName, FileName) then
      Exit;
  Result := False;
end;

function TGitCommitFrame.GetSvnEditState: TSvnEditState;
begin
  if Comment.Focused then
    Result := ControlToSvnEditState(Comment)
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

procedure TGitCommitFrame.HandleMissingFiles(ARefresh: Boolean = False);
var
  UnversionedFiles: TDictionary<string, string>;
  MissingList: TStringList;
  I, J: Integer;
  SvnListViewItem: TSvnListViewItem;
  FileName: string;
  ItemList: TList<TSvnListViewItem>;
begin
  UnversionedFiles := TDictionary<string, string>.Create(0, TOrdinalStringComparer.Create);
  try
    MissingList := TStringList.Create;
    try
      if ARefresh then
        ItemList := FRefreshItemList
      else
        ItemList := FItemList;
      for I := 0 to ItemList.Count - 1 do
        if ItemList[I].TextStatus = gsUnversioned then
          UnversionedFiles.Add(ItemList[I].PathName, ItemList[I].PathName)
        {else if ItemList[I].TextStatus = gsMissing then
          MissingList.Add(ItemList[I].PathName)};//TODO: Remove - there seems to be no status Missing for Git
      for I := 0 to MissingList.Count - 1 do
        if UnversionedFiles.TryGetValue(MissingList[I], FileName) then
        begin
          try
            RenameFile(FileName, MissingList[I]);
          except
            // If an error occures renaming the file then it is ok to ignore it.
          end;
          SvnListViewItem := TSvnListViewItem.Create('', gsNormal, False, False);
          try
            GetFileStatusCallBack(MissingList[I], SvnListViewItem);
            //if SvnListViewItem.FTextStatus <> gsMissing then//TODO: Remove - there seems to be no status Missing for Git
            begin
              {//TODO: Remove - there seems to be no status Missing for Git
              for J := 0 to ItemList.Count - 1 do
                if (ItemList[J].FTextStatus = gsMissing)
                  and SameText(ItemList[J].PathName, MissingList[I]) then
                begin
                  if SvnListViewItem.TextStatus = gsNormal then
                    ItemList.Delete(J)
                  else
                    ItemList[J].TextStatus := SvnListViewItem.FTextStatus;
                  Break;
                end;
              }
              for J := 0 to ItemList.Count - 1 do
                if (ItemList[J].TextStatus = gsUnversioned)
                  and SameText(ItemList[J].PathName, MissingList[I]) then
                begin
                  ItemList.Delete(J);
                  Break;
                end;
            end;
          finally
            SvnListViewItem.Free;
          end;
        end;
        RebuildList;
    finally
      MissingList.Free;
    end;
  finally
    UnversionedFiles.Free;
  end;
end;

function TGitCommitFrame.ItemShown(const SvnListItem: TSvnListViewItem): Boolean;
begin
  Result := UnversionedFiles.Checked or (SvnListItem.TextStatus <> gsUnversioned);
  //Result := Result and (Externals.Checked or (SvnListItem.TextStatus <> svnWcStatusExternal));
  Result := Result and SvnListItem.Visible;
end;

procedure TGitCommitFrame.LowerPanelResize(Sender: TObject);
begin
  LowerPanel.OnResize := nil;
  try
    ResizeStuff;
  finally
    LowerPanel.OnResize := LowerPanelResize;
  end;
end;

procedure TGitCommitFrame.ResizeStuff;
var
  StackButtons: Boolean;
  LongestCheckBoxWidth: Integer;
  HeightDelta: Integer;
begin
  if FAlternativeLayoutEnabled then
    Exit;
  //find widest checkbox (they *should* all be the smae width)
  LongestCheckBoxWidth := UnversionedFiles.Width;
  if Externals.Width > LongestCheckBoxWidth then
    LongestCheckBoxWidth := Externals.Width;
  if CheckAll.Width > LongestCheckBoxWidth then
    LongestCheckBoxWidth := CheckAll.Width;
  //calculate the difference in height for the Comments memo (also used for adjusting the top of the checkboxes)
  HeightDelta := Recent.Height + Recent.Margins.Top + Recent.Margins.Bottom + Commit.Height + Commit.Margins.Top + Commit.Margins.Bottom;
  //should the two buttons get stacked under the checkboxes?
  StackButtons := Width < UnversionedFiles.Left + UnversionedFiles.Margins.Left + LongestCheckBoxWidth + Recent.Width + Recent.Margins.Left;
  DisableAlign;
  try
    if StackButtons then
    begin
      if Recent.Left <> UnversionedFiles.Left then //have we already adjusted for this situation?
      begin
        //make Comments memo shorter, move checkboxes up and position the buttons under the checkboxes
        Comment.Height := Comment.Height - HeightDelta;
        UnversionedFiles.Top := UnversionedFiles.Top - HeightDelta;
        Externals.Top := Externals.Top - HeightDelta;
        CheckAll.Top := CheckAll.Top - HeightDelta;
        LowerPanel.Height := LowerPanel.Height + Recent.Height + Recent.Margins.Top + Recent.Margins.Bottom + Commit.Height + Commit.Margins.Top + Commit.Margins.Bottom;
        Recent.Top := CheckAll.Top + CheckAll.Height + CheckAll.Margins.Bottom + Recent.Margins.Top;
        Recent.Left := UnversionedFiles.Left;
        Commit.Top := Recent.Top + Recent.Height + Recent.Margins.Bottom + Commit.Margins.Top;
        Commit.Left := Recent.Left;
        Recent.Anchors := [akLeft, akBottom];
        Commit.Anchors := [akLeft, akBottom];
      end;
    end else
    begin
      if Recent.Left = CheckAll.Left then //have we already adjusted for this situation?
      begin
        //make Comments memo taller, move checkboxes down and position the buttons right aligned in the LowerPanel
        Comment.Height := Comment.Height + HeightDelta;
        UnversionedFiles.Top := UnversionedFiles.Top + HeightDelta;
        Externals.Top := Externals.Top + HeightDelta;
        CheckAll.Top := CheckAll.Top + HeightDelta;
        LowerPanel.Height := LowerPanel.Height - (Recent.Height + Recent.Margins.Top + Recent.Margins.Bottom + Commit.Height + Commit.Margins.Top + Commit.Margins.Bottom);
        Recent.Left := Comment.Left + Comment.Width - Recent.Width;
        Recent.Top := Comment.Top + Comment.Height + Comment.Margins.Bottom + Recent.Margins.Top;
        Commit.Left := Comment.Left + Comment.Width - Commit.Width;
        Commit.Top := Recent.Top + Recent.Height + Recent.Margins.Bottom + Commit.Margins.Top;
        Recent.Anchors := [akRight, akBottom];
        Commit.Anchors := [akRight, akBottom];
      end;
    end;
  finally
    EnableAlign
  end;
end;

procedure TGitCommitFrame.Notify(Sender: TObject; const Item: TSvnListViewItem;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TGitCommitFrame.PerformEditAction(AEditAction: TSvnEditAction): Boolean;
var
  I, StartIdx: Integer;
  SL: TStringList;
begin
  if Comment.Focused then
  begin
    Result := PerformDefaultSvnEditAction(Comment, AEditAction);
    UpdateCommitButton;
  end
  else
  if Files.Focused then
  begin
    if AEditAction = seaCopy then
    begin
      SL := TStringList.Create;
      try
        StartIdx := Files.Selected.Index;
        for I := StartIdx to Files.Items.Count - 1 do
          if Files.Items[I].Selected then //path + filename
            SL.Add(Files.Items[I].SubItems[0] + Files.Items[I].Caption);
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

procedure TGitCommitFrame.RebuildList;
var
  I: Integer;
  Cursor: TCursor;
begin
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    BeginUpdate;
    try
      FNoFiles := False;
      Files.Clear;
      FIndexList.Clear;
      for I := 0 to FItemList.Count - 1 do
        UpdateListView(FItemList[I], I);
      Files.CustomSort(@ColumnSort, LPARAM(Self));
      if Files.Items.Count = 0 then
      begin
        FNoFiles := True;
        for I := 0 to Files.Columns.Count - 1 do
          Files.Columns.Items[I].Width := -2;
      end;
    finally
      EndUpdate;
    end;
    UpdateCountLabel;
  finally
    Screen.Cursor := Cursor;
  end;
end;

procedure TGitCommitFrame.RecentClick(Sender: TObject);
var
  S: string;
begin
  S := SelectRecentComments(Self, FRecentComments);
  if Comment.Text = '' then
    Comment.Text := S
  else
    Comment.Text := Comment.Text + ' ' + S;
end;

procedure TGitCommitFrame.RefreshAdd(const SvnListItem: TSvnListViewItem);
begin
  FRefreshItemList.Add(SvnListItem);
end;

procedure TGitCommitFrame.ResolveActionExecute(Sender: TObject);
{
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
}
begin
  {
  if Files.SelCount > 0 then
  begin
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if SvnListViewItem.TextStatus = svnWcStatusConflicted then
        begin
          FResolveCallBack(SvnListViewItem.FPathName);
          SvnListViewItem.FTextStatus := svnWcStatusModified;
          SvnListViewItem.Checked := True;
        end;
      end;
    RebuildList;
  end;
  }
end;

procedure TGitCommitFrame.ResolveActionUpdate(Sender: TObject);
{
var
  I, StartIdx: Integer;
  ResolveState: Boolean;
}
begin
  {
  if Files.SelCount > 0 then
  begin
    ResolveState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        if FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]].FTextStatus = svnWcStatusConflicted then
        begin
          ResolveState := True;
          Break;
        end;
      end;
    ResolveAction.Visible := ResolveState;
  end
  else
    ResolveAction.Visible := False;
  }
  ResolveAction.Visible := False;
end;

type
  TSvnTreeCommitData = class(TSvnTreeData)
    ListItem: TSvnListViewItem;
    Selected: Boolean;
  end;

  TRevertData = class(TObject)
  private
    FListItem: TSvnListViewItem;
    FListItems: TList<TSvnListViewItem>;
    FRecursive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ListItem: TSvnListViewItem read FListItem write FListItem;
    property ListItems: TList<TSvnListViewItem> read FListItems;
    property Recursive: Boolean read FRecursive write FRecursive;
  end;

{ TRevertData }

constructor TRevertData.Create;
begin
  inherited Create;
  FListItems := TList<TSvnListViewItem>.Create;
end;

destructor TRevertData.Destroy;
begin
  FListItems.Free;
  inherited Destroy;
end;

procedure TGitCommitFrame.RevertActionExecute(Sender: TObject);
var
  SvnListViewItem: TSvnListViewItem;
  S: string;
  I, J, AdditionalDirCount, AdditionalFileCount: Integer;
  FilesToRevertText: TList<TSvnListViewItem>;
  NewState: TGitStatus;
  UpdateList: Boolean;
  SvnRoot: TSvnRootNode<TSvnTreeCommitData>;
  ItemsToRevert: TObjectList<TRevertData>;
  RevertData: TRevertData;
  SvnCommitData: TSvnTreeCommitData;
begin
  if Files.SelCount > 0 then
  begin
    FilesToRevertText := TList<TSvnListViewItem>.Create;
    ItemsToRevert := TObjectList<TRevertData>.Create;
    SvnRoot := TSvnRootNode<TSvnTreeCommitData>.Create;
    try
      for I := 0 to Files.Items.Count - 1 do
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if SvnListViewItem.Visible and (SvnListViewItem.TextStatus <> gsUnversioned) then
        begin
          if SvnListViewItem.Directory then
            SvnCommitData := SvnRoot.AddDir(SvnListViewItem.PathName).Data
          else
            SvnCommitData := SvnRoot.AddFile(SvnListViewItem.PathName).Data;
          SvnCommitData.ListItem := SvnListViewItem;
          SvnCommitData.Selected := Files.Items[I].Selected;
        end;
      end;
      SvnRoot.WalkThrough(procedure(ANode: TObject; AData: TSvnTreeCommitData; var AWalkModes: TSvnRootNodeWalkModes)
        begin
          if wmInfo in AWalkModes then
            FilesToRevertText.Add(AData.ListItem)
          else
          if AData.Selected then
          begin
            if (AData.Kind = dkFolder) and (AData.ListItem.TextStatus = gsAdded) then
            begin
              Include(AWalkModes, wmInfo);
              ItemsToRevert.Add(TRevertData.Create);
              RevertData := ItemsToRevert.Last;
              RevertData.ListItem := AData.ListItem;
              RevertData.ListItems.Add(AData.ListItem);
              RevertData.Recursive := True;
              FilesToRevertText.Add(AData.ListItem);
              SvnRoot.WalkThrough(procedure(AChildNode: TObject; AChildData: TSvnTreeCommitData;
                  var AChildWalkModes: TSvnRootNodeWalkModes)
                begin
                  RevertData.ListItems.Add(AChildData.ListItem);
                end, ANode);
            end
            else
            begin
              ItemsToRevert.Add(TRevertData.Create);
              RevertData := ItemsToRevert.Last;
              RevertData.ListItem := AData.ListItem;
              RevertData.Recursive := False;
              FilesToRevertText.Add(AData.ListItem);
            end;
          end;
        end);

      FilesToRevertText.Sort(TSvnListViewItemPathComparer.Create);
      S := sRevertCheck;
      if FilesToRevertText.Count <= 7 then
        for I := 0 to FilesToRevertText.Count - 1 do
          S := S + sLineBreak + Format('[%s] %s', [StatusKindStrEx(FilesToRevertText[I].TextStatus,
            FilesToRevertText[I].Copied), FilesToRevertText[I].PathName])
      else
      begin
        AdditionalDirCount := 0;
        AdditionalFileCount := 0;
        for I := 0 to FilesToRevertText.Count - 1 do
        begin
          if I <= 4 then
            S := S + sLineBreak + Format('[%s] %s', [StatusKindStrEx(FilesToRevertText[I].TextStatus,
              FilesToRevertText[I].Copied), FilesToRevertText[I].PathName])
          else
          begin
            if FilesToRevertText[I].Directory then
              Inc(AdditionalDirCount)
            else
              Inc(AdditionalFileCount);
          end;
        end;
        if AdditionalDirCount > 0 then
          S := S + sLineBreak + Format(sRevertDirMoreDirectories, [AdditionalDirCount]);
        if AdditionalFileCount > 0 then
          S := S + sLineBreak + Format(sRevertDirMoreFiles, [AdditionalFileCount]);
      end;

      if MessageDlg(S, mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        UpdateList := False;
        for I := 0 to ItemsToRevert.Count - 1 do
          if ItemsToRevert[I].Recursive then
          begin
            if RevertCallBack(ItemsToRevert[I].ListItem.PathName, True, NewState) and
              (NewState = gsUnversioned) then
            begin
              UpdateList := True;
              for J := 0 to ItemsToRevert[I].ListItems.Count - 1 do
              begin
                SvnListViewItem := ItemsToRevert[I].ListItems[J];
                SvnListViewItem.FTextStatus := gsUnversioned;
                SvnListViewItem.Checked := False;
              end;
            end
          end
          else
          if RevertCallBack(ItemsToRevert[I].ListItem.PathName, False, NewState) then
          begin
            UpdateList := True;
            SvnListViewItem := ItemsToRevert[I].ListItem;
            SvnListViewItem.FTextStatus := NewState;
            SvnListViewItem.Visible := not (NewState in [{svnWcStatusNone, }gsNormal]);
            SvnListViewItem.Checked := False;
          end;
        if UpdateList then
        begin
          RebuildList;
          UpdateCommitButton;
        end;
      end;
    finally
      SvnRoot.Free;
      ItemsToRevert.Free;
      FilesToRevertText.Free;
    end;
  end;
end;

procedure TGitCommitFrame.RevertActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  RevertState: Boolean;
  SvnListViewItem: TSvnListViewItem;
begin
  if Files.SelCount > 0 then
  begin
    RevertState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if (SvnListViewItem.FTextStatus <> gsUnversioned)
          and ((not SvnListViewItem.Directory) or (SvnListViewItem.FTextStatus = gsAdded)) then
        begin
          RevertState := True;
          Break;
        end;
      end;
    RevertAction.Visible := RevertState;
  end
  else
    RevertAction.Visible := False;
end;

procedure TGitCommitFrame.SetAllowEmptyComment(const Value: Boolean);
begin
  if FAllowEmptyComment <> Value then
  begin
    FAllowEmptyComment := Value;
    if True then

    UpdateCommitButton;
  end;
end;

procedure TGitCommitFrame.SetRecentComments(Value: TStringList);
begin
  FRecentComments.Assign(Value);
  Recent.Enabled := FRecentComments.Count <> 0;
end;

procedure TGitCommitFrame.SetSupportsExternal(const Value: Boolean);
begin
  if FSupportsExternals <> Value then
  begin
    FSupportsExternals := Value;
    Externals.Visible := Value;
  end;
end;

procedure TGitCommitFrame.SetURL(const AValue: string);
begin
  if FURL <> AValue then
  begin
    FURL := AValue;
    Location.Caption := FURL;
  end;
end;


function TGitCommitFrame.StatusKindStrEx(Status: TGitStatus; ACopied: Boolean): string;
begin
  case Status of
    gsAdded: Result := 'Added';//str
    gsModified: Result := 'Modified';
    gsNormal: Result := 'Normal';
    gsUnversioned: Result := 'Unversioned';
    //gsMissing: Result := 'Missing';//TODO: Remove - there seems to be no status Missing for Git
    gsDeleted: Result := 'Deleted';
    else
      Result := '?';
  end;
end;

procedure TGitCommitFrame.UnversionedFilesClick(Sender: TObject);
begin
  RebuildList;
  UpdateCommitButton;
end;

procedure TGitCommitFrame.UpdateCommitButton;
var
  I: Integer;
begin
  for I := 0 to Files.Items.Count - 1 do
    if Files.Items[I].Checked then
    begin
      Commit.Enabled := FAllowEmptyComment or (Comment.Text <> '');
      Exit;
    end;
    Commit.Enabled := False;
end;

procedure TGitCommitFrame.UpdateCountLabel;
var
  I, CheckedCount: Integer;
begin
  CheckedCount := 0;
  for I := 0 to FIndexList.Count - 1 do
    if FItemList[FIndexList[I]].Checked then
      Inc(CheckedCount);
  SelCountTotalCount.Caption := Format(sSelCountTotalCount, [CheckedCount, Files.Items.Count]);
end;

procedure TGitCommitFrame.UpdateListView(const SvnListItem: TSvnListViewItem;
  ItemIndex: Integer);
var
  ListItem: TListItem;
  CheckedState: Boolean;
  IndexIndex: Integer;
  I: Integer;
  FirstAdded: Boolean;
begin
  if ItemShown(SvnListItem) then
  begin
    FirstAdded := Files.Items.Count = 0;
    IndexIndex := FIndexList.Add(ItemIndex);
    CheckedState := SvnListItem.Checked;
    ListItem := Files.Items.Add;
    ListItem.Data := TCustomData(IndexIndex + 1);
    ListItem.Caption := ExtractFileName(SvnListItem.PathName);
    ListItem.SubItems.Add(ExtractFilePath(SvnListItem.PathName));
    ListItem.SubItems.Add(ExtractFileExt(SvnListItem.PathName));
    ListItem.SubItems.Add(StatusKindStrEx(SvnListItem.TextStatus, SvnListItem.Copied));
    ListItem.Checked := CheckedState;
    ListItem.ImageIndex := GitImageModule.GetShellImageIndex(SvnListItem.PathName);
    if FirstAdded then
    begin
      if FUpdateCount > 0 then
        FSetAutoSizeColumnWidth := True
      else
        for I := 0 to Files.Columns.Count - 1 do
          Files.Columns.Items[I].Width := -1;
      FNoFiles := False;
    end;
  end;
end;

procedure TGitCommitFrame.WndProc(var Message: TMessage);
begin
  if (Message.Msg = CM_CHILDKEY) and(TCMChildKey(Message).CharCode = VK_F5) then
    DoRefresh
  else
    inherited WndProc(Message);
end;

{ TSvnListViewItem }

constructor TSvnListViewItem.Create(const APathName: string;
  ATextStatus: TGitStatus; ADirectory, ACopied: Boolean);
begin
  inherited Create;
  NewValues(APathName, ATextStatus, ADirectory, ACopied);
end;

procedure TSvnListViewItem.NewValues(const APathName: string;
  ATextStatus: TGitStatus; ADirectory, ACopied: Boolean);
begin
  FCopied := ACopied;
  FDirectory := ADirectory;
  FPathName := APathName;
  FTextStatus := ATextStatus;
  FVisible := True;
  FChecked := (FTextStatus <> gsUnversioned) {and
    (FTextStatus <> svnWcStatusExternal) and (FTextStatus <> gsMissing) and
    (FTextStatus <> svnWcStatusConflicted) and (FTextStatus <> svnWcStatusIgnored)};
end;

procedure TSvnListViewItem.SetTextStatus(Value: TGitStatus);
begin
  FTextStatus := Value;
  FChecked := (FTextStatus <> gsUnversioned) {and
    (FTextStatus <> svnWcStatusExternal) and (FTextStatus <> gsMissing) {and
    (FTextStatus <> svnWcStatusConflicted) and (FTextStatus <> svnWcStatusIgnored)};
end;

{ TSvnListViewItemPathComparer }

function TSvnListViewItemPathComparer.Compare(const Left, Right: TSvnListViewItem): Integer;
begin
  Result := CompareStr(Left.PathName, Right.PathName);
end;

end.
