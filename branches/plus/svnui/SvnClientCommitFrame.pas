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
unit SvnClientCommitFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, svn_client, Generics.Collections, ActnList, Menus,
  ExtCtrls, SvnUITypes, SvnUIUtils, Clipbrd, Generics.Defaults, SvnImages, SvnTree;

type
  PSvnListViewItem = ^TSvnListViewItem;
  TSvnListViewItem = class
  protected
    FChangeList: string;
    FCopied: Boolean;
    FDirectory: Boolean;
    FPathName: string;
    FTextStatus: TSvnWCStatusKind;
    FChecked: Boolean;
    FVisible: Boolean;
    procedure SetTextStatus(Value: TSvnWCStatusKind);
  public
    constructor Create(const APathName: string; ATextStatus: TSvnWCStatusKind;
      ADirectory, ACopied: Boolean; AChangeList: string);
    procedure NewValues(const APathName: string; ATextStatus: TSvnWCStatusKind;
      ADirectory, ACopied: Boolean; AChangeList: string);
    property ChangeList: string read FChangeList;
    property Copied: Boolean read FCopied;
    property Directory: Boolean read FDirectory;
    property PathName: string read FPathName;
    property TextStatus: TSvnWCStatusKind read FTextStatus write SetTextStatus;
    property Checked: Boolean read FChecked write FChecked;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TCommitCallBack = procedure(const CommitList: TStringList;
    const Comment: string; const RecentComments: TStringList) of object;
  TCloseCallBack = procedure of object;
  TDiffCallBack = procedure(const FileName: string) of object;
  TRevertCallBack = function(const FileName: string; ARecursive: Boolean; var ANewTextStatus: TSvnWCStatusKind): Boolean of object;
  TAddCallBack = function(const FileName: string): Boolean of object;
  TResolveCallBack = procedure(const FileName: string) of object;
  TGetFileStatusCallBack = procedure(const FileName: string; var SvnListViewItem: TSvnListViewItem) of object;
  TFileColorCallBack = function(AItem: TSvnListViewItem): TColor of object;
  TRefreshCallBack = procedure of object;
  TAddToChangeListCallBack = function(const FileName, AChangeList: string): Boolean of object;
  TRemoveFromChangeListCallBack = function(const FileName: string): Boolean of object;

  TSvnCommitFrame = class(TFrame)
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
    RemoveChangeListAction: TAction;
    RemoveFromChangeList1: TMenuItem;
    MoveToChangeListAction: TAction;
    MoveToChangeList1: TMenuItem;
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
    procedure RemoveChangeListActionExecute(Sender: TObject);
    procedure RemoveChangeListActionUpdate(Sender: TObject);
    procedure MoveToChangeListActionUpdate(Sender: TObject);
    procedure CommitMenuPopup(Sender: TObject);
    procedure AddToNewChangeListClick(Sender: TObject);
    procedure MoveToChangeListActionExecute(Sender: TObject);
  protected
    FAlternativeLayoutEnabled: Boolean;
    FAddToChangeListCallBack: TAddToChangeListCallBack;
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
    FRemoveFromChangeListCallBack: TRemoveFromChangeListCallBack;
    FSetAutoSizeColumnWidth: Boolean;
    FSortColumns: array of Integer;
    FSortOrder: Boolean;
    FRecentComments: TStringList;
    FURL: string;
    FNoFiles: Boolean;
    FChangesLists: TStringList;
    FChangesListIgnoreOnCommitGroupID: Integer;
    FUpdateCount: Integer;
    FKeepOpenAfterCommit: Boolean;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure DoRefresh;
    function GetGroupID(const AChangeList: string): Integer;
    function GetSvnEditState: TSvnEditState;
    procedure SetRecentComments(Value: TStringList);
    procedure Notify(Sender: TObject; const Item: TSvnListViewItem;
      Action: TCollectionNotification);
    procedure RebuildList;
    function ItemShown(const SvnListItem: TSvnListViewItem): Boolean;
    procedure SetURL(const AValue: string);
    function StatusKindStrEx(Status: TSvnWCStatusKind; ACopied: Boolean): string;
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
    property AddToChangeListCallBack: TAddToChangeListCallBack read FAddToChangeListCallBack write FAddToChangeListCallBack;
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
    property RemoveFromChangeListCallBack: TRemoveFromChangeListCallBack read FRemoveFromChangeListCallBack write FRemoveFromChangeListCallBack;
    property SvnEditState: TSvnEditState read GetSvnEditState;
    property URL: string read FURL write SetURL;
  end;

implementation

uses SvnClient, SvnUIConst, SvnClientRecentComments;

{$R *.dfm}

const
  cIgnoreOnCommitChangeList = 'ignore-on-commit';
  cTagNewChangeList = -1;
  cTagIgnoreChangeList = -2;

type
  TSvnListViewItemPathComparer = class(TInterfacedObject, IComparer<TSvnListViewItem>)
    function Compare(const Left, Right: TSvnListViewItem): Integer;
  end;

function ColumnSort(Item1, Item2: TListItem; Param: LParam): Integer; stdcall;
var
  SvnCommitFrame: TSvnCommitFrame;
  S1, S2: string;
  I, OrderColumn: Integer;
begin
  SvnCommitFrame := TSvnCommitFrame(Param);
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

procedure TSvnCommitFrame.Add(const SvnListItem: TSvnListViewItem);
var
  ItemIndex: Integer;
begin
  ItemIndex := FItemList.Add(SvnListItem);
  UpdateListView(SvnListItem, ItemIndex);
end;

procedure TSvnCommitFrame.AddActionExecute(Sender: TObject);

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
        if FItemList[I].Directory and (FItemList[I].TextStatus = svnWcStatusUnversioned) then
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
            UnversionedParents[I].FTextStatus := svnWcStatusAdded;
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
        if (SvnListViewItem.FTextStatus = svnWcStatusUnversioned) and CheckAddParents(SvnListViewItem) then
        begin
          if FAddCallBack(SvnListViewItem.FPathName) then
          begin
            SvnListViewItem.FTextStatus := svnWcStatusAdded;
            SvnListViewItem.Checked := True;
          end;
        end;
      end;
    RebuildList;
  end;
end;

procedure TSvnCommitFrame.AddActionUpdate(Sender: TObject);
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
        if FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]].FTextStatus = svnWcStatusUnversioned then
        begin
          AddState := True;
          Break;
        end;
    AddAction.Visible := AddState;
  end
  else
    AddAction.Visible := False;
end;

procedure TSvnCommitFrame.AddToNewChangeListClick(Sender: TObject);
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
  ChangeList: string;
  ModifiedChangeLists: Boolean;
begin
  if (Files.SelCount > 0) and (Sender is TMenuItem) then
  begin
    case TMenuItem(Sender).Tag of
      cTagNewChangeList: if not InputQuery(sCreateChangeListCaption, sCreateChangeListPrompt, ChangeList) then
            ChangeList := '';
      cTagIgnoreChangeList: ChangeList := cIgnoreOnCommitChangeList;
      else
        ChangeList := FChangesLists[TMenuItem(Sender).Tag];
    end;
    if ChangeList <> '' then
    begin
      ModifiedChangeLists := False;
      StartIdx := Files.Selected.Index;
      for I := StartIdx to Files.Items.Count - 1 do
        if Files.Items[I].Selected then
        begin
          SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
          if not (SvnListViewItem.Directory or (SvnListViewItem.TextStatus in [svnWcStatusNone, svnWcStatusUnversioned])) then
            if FAddToChangeListCallBack(SvnListViewItem.PathName, ChangeList) then
            begin
              SvnListViewItem.FChangeList := ChangeList;
              if ChangeList = cIgnoreOnCommitChangeList then
                SvnListViewItem.Checked := False;
              ModifiedChangeLists := True;
            end;
        end;
      if ModifiedChangeLists then
        RebuildList;
    end;
  end;
end;

procedure TSvnCommitFrame.BeginUpdate;
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

procedure TSvnCommitFrame.CheckAllClick(Sender: TObject);
var
  I: Integer;
  Checked: Boolean;
begin
  FExecutingCheckAllClick := True;
  try
    if CheckAll.State <> cbGrayed then
    begin
      Checked := CheckAll.State = cbChecked;
      if (FChangesListIgnoreOnCommitGroupID <> -1) and Checked then
        for I := 0 to Files.Items.Count - 1 do
          Files.Items[I].Checked := Files.Items[I].GroupID <> FChangesListIgnoreOnCommitGroupID
      else
        for I := 0 to Files.Items.Count - 1 do
          Files.Items[I].Checked := Checked;
    end;
    UpdateCommitButton;
    UpdateCountLabel;
  finally
    FExecutingCheckAllClick := False;
  end;
end;

procedure TSvnCommitFrame.CheckForNoFilesVisible;
begin
  FNoFiles := Files.Items.Count = 0;
  UpdateCountLabel;
end;

procedure TSvnCommitFrame.CMRelease(var Message: TMessage);
begin
  CloseCallBack;
end;

procedure TSvnCommitFrame.CommitClick(Sender: TObject);
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
      if FItemList[I].Visible and (FItemList[I].ChangeList <> cIgnoreOnCommitChangeList) and
        not FItemList[I].Checked then
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
        if FItemList[I].TextStatus = svnWcStatusUnversioned then
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

procedure TSvnCommitFrame.CommitMenuPopup(Sender: TObject);
var
  I, StartIdx: Integer;
  MenuItem: TMenuItem;
  SvnListViewItem: TSvnListViewItem;
  SelectedChangeList: string;
  SelectedChangeLists: TStringList;
begin
  if MoveToChangeList1.Visible then
  begin
    while MoveToChangeList1.Count > 0 do
      MoveToChangeList1.Delete(0);

    MenuItem := TMenuItem.Create(MoveToChangeList1);
    MenuItem.Caption := sAddNewChangeList;
    MenuItem.OnClick := AddToNewChangeListClick;
    MenuItem.Tag := cTagNewChangeList;
    MoveToChangeList1.Add(MenuItem);

    //check if all selected items have the same changelist to hide it's menu item
    if Files.SelCount > 0 then
    begin
      SelectedChangeLists := TStringList.Create;
      try
        SelectedChangeLists.Sorted := True;
        SelectedChangeLists.Duplicates := dupIgnore;
        StartIdx := Files.Selected.Index;
        for I := StartIdx to Files.Items.Count - 1 do
          if Files.Items[I].Selected then
          begin
            SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
            if not (SvnListViewItem.Directory or (SvnListViewItem.TextStatus in [svnWcStatusNone, svnWcStatusUnversioned])) then
              SelectedChangeLists.Add(SvnListViewItem.ChangeList);
          end;
        if SelectedChangeLists.Count = 1 then
          SelectedChangeList := SelectedChangeLists[0]
        else
          SelectedChangeList := '';
      finally
        SelectedChangeLists.Free;
      end;
    end
    else
      SelectedChangeList := '';

    if SelectedChangeList <> cIgnoreOnCommitChangeList then
    begin
      MenuItem := TMenuItem.Create(MoveToChangeList1);
      MenuItem.Caption := '-';
      MoveToChangeList1.Add(MenuItem);

      MenuItem := TMenuItem.Create(MoveToChangeList1);
      MenuItem.Caption := cIgnoreOnCommitChangeList;
      MenuItem.OnClick := AddToNewChangeListClick;
      MenuItem.Tag := cTagIgnoreChangeList;
      MoveToChangeList1.Add(MenuItem);
    end;

    if (FChangesLists.Count > 1) or
      ((FChangesLists.Count = 1) and (FChangesLists[0] <> SelectedChangeList)) then
    begin
      MenuItem := TMenuItem.Create(MoveToChangeList1);
      MenuItem.Caption := '-';
      MoveToChangeList1.Add(MenuItem);
      for I := 0 to FChangesLists.Count - 1 do
        if FChangesLists[I] <> SelectedChangeList then
        begin
          MenuItem := TMenuItem.Create(MoveToChangeList1);
          MenuItem.Caption := FChangesLists[I];
          MenuItem.OnClick := AddToNewChangeListClick;
          MenuItem.Tag := I;
          MoveToChangeList1.Add(MenuItem);
        end;
    end;
  end;
end;

constructor TSvnCommitFrame.Create(AOwner: TComponent);
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
  FChangesLists := TStringList.Create;
  FChangesLists.Sorted := True;
  FChangesListIgnoreOnCommitGroupID := -1;
  FNoFiles := False;
  FSetAutoSizeColumnWidth := False;
  FUpdateCount := 0;
end;

destructor TSvnCommitFrame.Destroy;
begin
  FChangesLists.Free;
  FRefreshItemList.Free;
  FIndexList.Free;
  FItemList.Free;
  FRecentComments.Free;
  inherited;
end;

procedure TSvnCommitFrame.DiffActionUpdate(Sender: TObject);
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
        if not (SvnListViewItem.FTextStatus in [svnWcStatusUnversioned, svnWcStatusAdded, svnWcStatusNormal, svnWcStatusDeleted])
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

procedure TSvnCommitFrame.DoDiff(Sender: TObject);
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
        if not (SvnListViewItem.FTextStatus in [svnWcStatusUnversioned, svnWcStatusAdded, svnWcStatusNormal, svnWcStatusDeleted])
          and not SvnListViewItem.Directory then
          DiffCallBack(SvnListViewItem.FPathName);
      end;
  end;
end;

procedure TSvnCommitFrame.DoRefresh;
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
          Files.Groups.Clear;
          Files.GroupView := False;
          FChangesLists.Clear;
          FChangesListIgnoreOnCommitGroupID := -1;
          for I := 0 to FRefreshItemList.Count - 1 do
          begin
            SvnListViewItem := FRefreshItemList[I];
            Add(TSvnListViewItem.Create(SvnListViewItem.PathName, SvnListViewItem.TextStatus,
              SvnListViewItem.Directory, SvnListViewItem.Copied, SvnListViewItem.ChangeList));
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

procedure TSvnCommitFrame.EnableAlternativeLayout;
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

procedure TSvnCommitFrame.EndUpdate;
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

procedure TSvnCommitFrame.ExternalsClick(Sender: TObject);
begin
  RebuildList;
end;

procedure TSvnCommitFrame.FilesColumnClick(Sender: TObject;
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

procedure TSvnCommitFrame.FilesCustomDraw(Sender: TCustomListView;
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

procedure TSvnCommitFrame.FilesCustomDrawItem(Sender: TCustomListView;
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

procedure TSvnCommitFrame.FilesDblClick(Sender: TObject);
begin
  DoDiff(Sender);
end;

procedure TSvnCommitFrame.FilesItemChecked(Sender: TObject; Item: TListItem);

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
              (TestSvnListViewItem.TextStatus = svnWcStatusUnversioned) then
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
          if (TestSvnListViewItem.TextStatus = svnWcStatusUnversioned) and
            (Pos(S, TestSvnListViewItem.PathName) = 1) then
            Files.Items[I].Checked := False;
        end;
    end;
  end;

  function GetNormalizedCheckState(AItem: TListItem): Boolean;
  begin
    Result := AItem.Checked;
    if (FChangesListIgnoreOnCommitGroupID <> -1) and (AItem.GroupID = FChangesListIgnoreOnCommitGroupID) then
      Result := not Result;
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
      (FItemList[FIndexList[Integer(Item.Data) - 1]].TextStatus = svnWcStatusUnversioned) then
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
    Checked := GetNormalizedCheckState(Item);
    for I := 0 to Files.Items.Count - 1 do
      if GetNormalizedCheckState(Files.Items[I]) <> Checked then
      begin
        CheckAll.State := cbGrayed;
        UpdateCommitButton;
        UpdateCountLabel;
        Exit;
      end;
    if GetNormalizedCheckState(Item) then
      CheckAll.State := cbChecked
    else
      CheckAll.State := cbUnChecked;
    UpdateCommitButton;
    UpdateCountLabel;
  end;
end;

procedure TSvnCommitFrame.FilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DoDiff(Sender);
end;

function TSvnCommitFrame.Found(const FileName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FItemList.Count - 1 do
    if SameText(FItemList[I].PathName, FileName) then
      Exit;
  Result := False;
end;

function TSvnCommitFrame.GetGroupID(const AChangeList: string): Integer;

  function AddGroup(const AChangeList: string): Integer;
  var
    I, J, StartIndex, EndIndex: Integer;
    ListGroup, ListI, ListJ: TListGroup;
    IndexI, IndexJ: Integer;
  begin
    ListGroup := Files.Groups.Add;
    ListGroup.GroupID := Files.Groups.NextGroupID;
    ListGroup.Header := AChangeList;
    //sort groups - order is sNoChangeList, items sorted by name, cIgnoreOnCommitChangeList
    if Files.Groups.Count > 1 then
    begin
      if AChangeList = sNoChangeList then
        ListGroup.Index := 0
      else
      if AChangeList = cIgnoreOnCommitChangeList then
        ListGroup.Index := Files.Groups.Count - 1
      else
      begin
        StartIndex := 0;
        if Files.Groups[StartIndex].Header = sNoChangeList then
          Inc(StartIndex);
        EndIndex := Files.Groups.Count - 1;
        if Files.Groups[EndIndex].Header = cIgnoreOnCommitChangeList then
          Dec(EndIndex);
        if EndIndex > StartIndex then
        begin
          //bubble sort (very likely the amount of groups is very low)
          for I := StartIndex to EndIndex - 1 do
            for J := I + 1 to EndIndex do
              if CompareStr(Files.Groups[I].Header, Files.Groups[J].Header) > 0 then
              begin
                ListI := Files.Groups[I];
                ListJ := Files.Groups[J];
                IndexI := ListI.Index;
                IndexJ := ListJ.Index;
                ListI.Index := IndexJ;
                ListJ.Index := IndexI;
              end;
        end;
      end;
    end;
    Result := ListGroup.GroupID;
    if (AChangeList <> sNoChangeList) and (AChangeList <> cIgnoreOnCommitChangeList) then
      FChangesLists.Add(AChangeList);
    if AChangeList = cIgnoreOnCommitChangeList then
      FChangesListIgnoreOnCommitGroupID := Result;
  end;

var
  I, GroupID: Integer;
  ChangeList: string;
  FoundNoGroup: Boolean;
begin
  Result := -1;
  ChangeList := AChangeList;
  if Files.GroupView and (ChangeList = '') then
    ChangeList := sNoChangeList;
  if ChangeList <> '' then
  begin
    for I := 0 to Files.Groups.Count - 1 do
      if Files.Groups[I].Header = ChangeList then
      begin
        Result := Files.Groups[I].GroupID;
        Break;
      end;
    if Result = -1 then
      Result := AddGroup(ChangeList);
    if not Files.GroupView then
    begin
      Files.GroupView := True;
      FoundNoGroup := False;
      for I := 0 to Files.Items.Count - 2 do
        if Files.Items[I].GroupID = -1 then
        begin
          FoundNoGroup := True;
          Break;
        end;
      if FoundNoGroup then
      begin
        GroupID := AddGroup(sNoChangeList);
        for I := 0 to Files.Items.Count - 2 do
          if Files.Items[I].GroupID = -1 then
            Files.Items[I].GroupID := GroupID;
      end;
    end;
  end;
end;

function TSvnCommitFrame.GetSvnEditState: TSvnEditState;
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

procedure TSvnCommitFrame.HandleMissingFiles(ARefresh: Boolean = False);
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
        if ItemList[I].TextStatus = svnWcStatusUnversioned then
          UnversionedFiles.Add(ItemList[I].PathName, ItemList[I].PathName)
        else if ItemList[I].TextStatus = svnWcStatusMissing then
          MissingList.Add(ItemList[I].PathName);
      for I := 0 to MissingList.Count - 1 do
        if UnversionedFiles.TryGetValue(MissingList[I], FileName) then
        begin
          try
            RenameFile(FileName, MissingList[I]);
          except
            // If an error occures renaming the file then it is ok to ignore it.
          end;
          SvnListViewItem := TSvnListViewItem.Create('', svnWcStatusNormal, False, False, '');
          try
            GetFileStatusCallBack(MissingList[I], SvnListViewItem);
            if SvnListViewItem.FTextStatus <> svnWcStatusMissing then
            begin
              for J := 0 to ItemList.Count - 1 do
                if (ItemList[J].FTextStatus = svnWcStatusMissing)
                  and SameText(ItemList[J].PathName, MissingList[I]) then
                begin
                  if SvnListViewItem.TextStatus = svnWcStatusNormal then
                    ItemList.Delete(J)
                  else
                    ItemList[J].TextStatus := SvnListViewItem.FTextStatus;
                  Break;
                end;
              for J := 0 to ItemList.Count - 1 do
                if (ItemList[J].TextStatus = svnWcStatusUnversioned)
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

function TSvnCommitFrame.ItemShown(const SvnListItem: TSvnListViewItem): Boolean;
begin
  Result := UnversionedFiles.Checked or (SvnListItem.TextStatus <> svnWcStatusUnversioned);
  Result := Result and (Externals.Checked or (SvnListItem.TextStatus <> svnWcStatusExternal));
  Result := Result and SvnListItem.Visible;
end;

procedure TSvnCommitFrame.LowerPanelResize(Sender: TObject);
begin
  LowerPanel.OnResize := nil;
  try
    ResizeStuff;
  finally
    LowerPanel.OnResize := LowerPanelResize;
  end;
end;

procedure TSvnCommitFrame.MoveToChangeListActionExecute(Sender: TObject);
begin
//dummy Execute implementation to make sure that the action is enabled
end;

procedure TSvnCommitFrame.MoveToChangeListActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
  VisibleState: Boolean;
begin
  VisibleState := Assigned(FAddToChangeListCallBack) and (Files.SelCount > 0);
  if VisibleState then
  begin
    VisibleState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if not (SvnListViewItem.Directory or (SvnListViewItem.TextStatus in [svnWcStatusNone, svnWcStatusUnversioned])) then
        begin
          VisibleState := True;
          Break;
        end;
      end;
  end;
  MoveToChangeListAction.Visible := VisibleState;
end;

procedure TSvnCommitFrame.ResizeStuff;
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

procedure TSvnCommitFrame.Notify(Sender: TObject; const Item: TSvnListViewItem;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TSvnCommitFrame.PerformEditAction(AEditAction: TSvnEditAction): Boolean;
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

procedure TSvnCommitFrame.RebuildList;
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
      Files.Groups.Clear;
      Files.GroupView := False;
      FChangesLists.Clear;
      FChangesListIgnoreOnCommitGroupID := -1;
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

procedure TSvnCommitFrame.RecentClick(Sender: TObject);
var
  S: string;
begin
  S := SelectRecentComments(Self, FRecentComments);
  if Comment.Text = '' then
    Comment.Text := S
  else
    Comment.Text := Comment.Text + ' ' + S;
end;

procedure TSvnCommitFrame.RefreshAdd(const SvnListItem: TSvnListViewItem);
begin
  FRefreshItemList.Add(SvnListItem);
end;

procedure TSvnCommitFrame.RemoveChangeListActionExecute(Sender: TObject);
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
  ModifiedChangeLists: Boolean;
begin
  if Files.SelCount > 0 then
  begin
    ModifiedChangeLists := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if SvnListViewItem.ChangeList <> '' then
          if FRemoveFromChangeListCallBack(SvnListViewItem.PathName) then
          begin
            SvnListViewItem.FChangeList := '';
            SvnListViewItem.Visible := not (SvnListViewItem.TextStatus in [svnWcStatusNone, svnWcStatusUnversioned]);
            ModifiedChangeLists := True;
          end;
      end;
    if ModifiedChangeLists then
      RebuildList;
  end;
end;

procedure TSvnCommitFrame.RemoveChangeListActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  SvnListViewItem: TSvnListViewItem;
  VisibleState: Boolean;
begin
  VisibleState := Assigned(FRemoveFromChangeListCallBack) and (Files.SelCount > 0);
  if VisibleState then
  begin
    VisibleState := False;
    StartIdx := Files.Selected.Index;
    for I := StartIdx to Files.Items.Count - 1 do
      if Files.Items[I].Selected then
      begin
        SvnListViewItem := FItemList[FIndexList[Integer(Files.Items[I].Data) - 1]];
        if SvnListViewItem.ChangeList <> '' then
        begin
          VisibleState := True;
          Break;
        end;
      end;
  end;
  RemoveChangeListAction.Visible := VisibleState;
end;

procedure TSvnCommitFrame.ResolveActionExecute(Sender: TObject);
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
        if SvnListViewItem.TextStatus = svnWcStatusConflicted then
        begin
          FResolveCallBack(SvnListViewItem.FPathName);
          SvnListViewItem.FTextStatus := svnWcStatusModified;
          SvnListViewItem.Checked := True;
        end;
      end;
    RebuildList;
  end;
end;

procedure TSvnCommitFrame.ResolveActionUpdate(Sender: TObject);
var
  I, StartIdx: Integer;
  ResolveState: Boolean;
begin
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

procedure TSvnCommitFrame.RevertActionExecute(Sender: TObject);
var
  SvnListViewItem: TSvnListViewItem;
  S: string;
  I, J, AdditionalDirCount, AdditionalFileCount: Integer;
  FilesToRevertText: TList<TSvnListViewItem>;
  NewState: TSvnWCStatusKind;
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
        if SvnListViewItem.Visible and (not (SvnListViewItem.TextStatus in [svnWcStatusUnversioned, svnWcStatusNormal])) then
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
            if (AData.Kind = dkFolder) and (AData.ListItem.TextStatus = svnWcStatusAdded) then
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
              (NewState = svnWcStatusUnversioned) then
            begin
              UpdateList := True;
              for J := 0 to ItemsToRevert[I].ListItems.Count - 1 do
              begin
                SvnListViewItem := ItemsToRevert[I].ListItems[J];
                SvnListViewItem.FTextStatus := svnWcStatusUnversioned;
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
            SvnListViewItem.Visible := (not (NewState in [svnWcStatusNone, svnWcStatusNormal]))
              or (SvnListViewItem.ChangeList <> '');
            if SvnListViewItem.ChangeList = '' then
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

procedure TSvnCommitFrame.RevertActionUpdate(Sender: TObject);
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
        if (not (SvnListViewItem.FTextStatus in [svnWcStatusUnversioned, svnWcStatusNormal]))
          and ((not SvnListViewItem.Directory) or (SvnListViewItem.FTextStatus = svnWcStatusAdded)) then
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

procedure TSvnCommitFrame.SetRecentComments(Value: TStringList);
begin
  FRecentComments.Assign(Value);
  Recent.Enabled := FRecentComments.Count <> 0;
end;

procedure TSvnCommitFrame.SetURL(const AValue: string);
begin
  if FURL <> AValue then
  begin
    FURL := AValue;
    Location.Caption := FURL;
  end;
end;


function TSvnCommitFrame.StatusKindStrEx(Status: TSvnWCStatusKind; ACopied: Boolean): string;
begin
  Result := StatusKindStr(Status);
  if (Status = svnWcStatusAdded) and ACopied then
    Result := Result + ' (+)';
end;

procedure TSvnCommitFrame.UnversionedFilesClick(Sender: TObject);
begin
  RebuildList;
  UpdateCommitButton;
end;

procedure TSvnCommitFrame.UpdateCommitButton;
var
  I: Integer;
begin
  for I := 0 to Files.Items.Count - 1 do
    if Files.Items[I].Checked then
    begin
      Commit.Enabled := True;
      Exit;
    end;
    Commit.Enabled := False;
end;

procedure TSvnCommitFrame.UpdateCountLabel;
var
  I, CheckedCount: Integer;
begin
  CheckedCount := 0;
  for I := 0 to FIndexList.Count - 1 do
    if FItemList[FIndexList[I]].Checked then
      Inc(CheckedCount);
  SelCountTotalCount.Caption := Format(sSelCountTotalCount, [CheckedCount, Files.Items.Count]);
end;

procedure TSvnCommitFrame.UpdateListView(const SvnListItem: TSvnListViewItem;
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
    ListItem.GroupID := GetGroupID(SvnListItem.ChangeList);
    if not CheckedState then //workaround for correct CheckAll state
      ListItem.Checked := True;
    ListItem.Checked := CheckedState;
    ListItem.ImageIndex := SvnImageModule.GetShellImageIndex(SvnListItem.PathName);
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

procedure TSvnCommitFrame.WndProc(var Message: TMessage);
begin
  if (Message.Msg = CM_CHILDKEY) and(TCMChildKey(Message).CharCode = VK_F5) then
    DoRefresh
  else
    inherited WndProc(Message);
end;

{ TSvnListViewItem }

constructor TSvnListViewItem.Create(const APathName: string;
  ATextStatus: TSvnWCStatusKind; ADirectory, ACopied: Boolean; AChangeList: string);
begin
  inherited Create;
  NewValues(APathName, ATextStatus, ADirectory, ACopied, AChangeList);
end;

procedure TSvnListViewItem.NewValues(const APathName: string;
  ATextStatus: TSvnWCStatusKind; ADirectory, ACopied: Boolean; AChangeList: string);
begin
  FChangeList := AChangeList;
  FCopied := ACopied;
  FDirectory := ADirectory;
  FPathName := APathName;
  FTextStatus := ATextStatus;
  FVisible := True;
  FChecked := (FTextStatus <> svnWcStatusUnversioned) and
    (FTextStatus <> svnWcStatusExternal) and (FTextStatus <> svnWcStatusMissing) and
    (FTextStatus <> svnWcStatusConflicted) and (FTextStatus <> svnWcStatusIgnored) and
    (AChangeList <> cIgnoreOnCommitChangeList);
end;

procedure TSvnListViewItem.SetTextStatus(Value: TSvnWCStatusKind);
begin
  FTextStatus := Value;
  FChecked := (FTextStatus <> svnWcStatusUnversioned) and
    (FTextStatus <> svnWcStatusExternal) and (FTextStatus <> svnWcStatusMissing) and
    (FTextStatus <> svnWcStatusConflicted) and (FTextStatus <> svnWcStatusIgnored) and
    (FChangeList <> cIgnoreOnCommitChangeList);
end;

{ TSvnListViewItemPathComparer }

function TSvnListViewItemPathComparer.Compare(const Left, Right: TSvnListViewItem): Integer;
begin
  Result := CompareStr(Left.PathName, Right.PathName);
end;

end.
