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
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{                                                                              }
{ Portions created or modified by Embarcadero Technologies are                 }
{ Copyright © 2010 Embarcadero Technologies, Inc. All Rights Reserved          }
{ Modifications include a major re-write of delphisvn. New functionality for   }
{ diffing, international character support, asynchronous gathering of data,    }
{ check-out and import, usability, tighter integration into RAD Studio, and    }
{ other new features.  Most original source files not used or re-written.      }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnClientRepoBrowserFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SvnClient, apr, svn_client,
  Generics.Defaults, Generics.Collections, ImgList, ShellAPI, ComCtrls,
  SvnUITypes;

type
  TSvnTreeNodeChildState = (tncsNotLoaded, tncsLoading, tncsLoaded, tncsPartiallyLoaded);

  TCustomSvnTreeNode = class(TObject)
  private
    FURL: string;
    FListItem: TSvnListItem;
  public
    constructor Create;
    destructor Destroy; override;
    property ListItem: TSvnListItem read FListItem;
    property URL: string read FURL write FURL;
  end;

  TSvnTreeFile = class(TCustomSvnTreeNode);
  TSvnTreeDir = class;

  TSvnTreeNodeList = class(TObjectList<TCustomSvnTreeNode>);
  TSvnTreeFileList = class(TList<TSvnTreeFile>);
  TSvnTreeDirList = class(TList<TSvnTreeDir>);

  TSvnTreeDir = class(TCustomSvnTreeNode)
  private
    FItems: TSvnTreeNodeList;
    FDirectories: TSvnTreeDirList;
    FExpanded: Boolean;
    FFiles: TSvnTreeFileList;
    FChildState: TSvnTreeNodeChildState;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TCustomSvnTreeNode;
    function GetFileCount: Integer;
    function GetFileItems(AIndex: Integer): TSvnTreeFile;
    function GetDirCount: Integer;
    function GetDirItems(AIndex: Integer): TSvnTreeDir;
  public
    constructor Create;
    destructor Destroy; override;
    function AddDir: TSvnTreeDir;
    function AddFile: TSvnTreeFile;
    procedure Clear;
    procedure SortItems;
    property ChildState: TSvnTreeNodeChildState read FChildState write FChildState;
    property Count: Integer read GetCount;
    property DirCount: Integer read GetDirCount;
    property DirItems[AIndex: Integer]: TSvnTreeDir read GetDirItems;
    property Expanded: Boolean read FExpanded write FExpanded;
    property FileCount: Integer read GetFileCount;
    property FileItems[AIndex: Integer]: TSvnTreeFile read GetFileItems;
    property Items[AIndex: Integer]: TCustomSvnTreeNode read GetItems; default;
  end;

  TFrmRepoBrowser = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    btnLoad: TButton;
    edURL: TEdit;
    ImageList: TImageList;
    Splitter1: TSplitter;
    edRevision: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SysImageList: TImageList;
    Panel3: TPanel;
    tvFolders: TTreeView;
    Panel4: TPanel;
    lvItems: TListView;
    procedure btnLoadClick(Sender: TObject);
    procedure tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure lvItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvItemsCustomDraw(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure lvItemsResize(Sender: TObject);
    procedure edURLChange(Sender: TObject);
    procedure edURLKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    type
      TRepoBrowserFileListViewMode = (lvmFiles, lvmText);
    var
    FSvnClient: TSvnClient;
    FRootNode: TSvnTreeDir;
    FActiveDir: TSvnTreeDir;
    FFolderIndex: Integer;
    FRevision: TSvnRevNum;
    FImageDictionary: TDictionary<string, Integer>;
    FLoading: Boolean;
    FFileListViewMode: TRepoBrowserFileListViewMode;
    FFileListViewText: string;
    class var FUseCount: Integer;
    procedure AddDirToListView(ATreeDir: TSvnTreeDir);
    procedure AddNodeToTree(ATreeDir: TSvnTreeDir; ANode: TTreeNode);
    procedure EnableLoad;
    function GetImageIndexByExt(const AExtension: string): Integer;
    function GetSvnEditState: TSvnEditState;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    procedure SetFileListViewMode(const AValue: TRepoBrowserFileListViewMode);
    procedure SetFileListViewText(const AValue: string);
    procedure UpdateSelectedURL;
    property FileListViewMode: TRepoBrowserFileListViewMode read FFileListViewMode write SetFileListViewMode;
    property FileListViewText: string read FFileListViewText write SetFileListViewText;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PerformEditAction(AEditAction: TSvnEditAction): Boolean;
    property SvnClient: TSvnClient read FSvnClient write FSvnClient;
    property SvnEditState: TSvnEditState read GetSvnEditState;
    property URL: string read GetURL write SetURL;
  end;

implementation

uses SvnUIUtils, SvnUIConst;

{$R *.dfm}

{ TCustomSvnTreeNode }

constructor TCustomSvnTreeNode.Create;
begin
  inherited Create;
  FListItem := TSvnListItem.Create;
end;

destructor TCustomSvnTreeNode.Destroy;
begin
  FListItem.Free;
  inherited Destroy;
end;

{ TSvnTreeNode }

constructor TSvnTreeDir.Create;
begin
  inherited Create;
  FItems := TSvnTreeNodeList.Create;
  FFiles := TSvnTreeFileList.Create;
  FDirectories := TSvnTreeDirList.Create;
  FChildState := tncsNotLoaded;
  FExpanded := False;
end;

destructor TSvnTreeDir.Destroy;
begin
  FItems.Free;
  FFiles.Free;
  FDirectories.Free;
  inherited Destroy;
end;

function TSvnTreeDir.AddDir: TSvnTreeDir;
begin
  FItems.Add(TSvnTreeDir.Create);
  Result := TSvnTreeDir(FItems.Last);
  FDirectories.Add(Result);
end;

function TSvnTreeDir.AddFile: TSvnTreeFile;
begin
  FItems.Add(TSvnTreeFile.Create);
  Result := TSvnTreeFile(FItems.Last);
  FFiles.Add(Result);
end;

procedure TSvnTreeDir.Clear;
begin
  FItems.Clear;
  FDirectories.Clear;
  FFiles.Clear;
  FChildState := tncsNotLoaded;
end;

function TSvnTreeDir.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSvnTreeDir.GetDirCount: Integer;
begin
  Result := FDirectories.Count;
end;

function TSvnTreeDir.GetDirItems(AIndex: Integer): TSvnTreeDir;
begin
  Result := FDirectories[AIndex];
end;

function TSvnTreeDir.GetFileCount: Integer;
begin
  Result := FFiles.Count;
end;

function TSvnTreeDir.GetFileItems(AIndex: Integer): TSvnTreeFile;
begin
  Result := FFiles[AIndex];
end;

function TSvnTreeDir.GetItems(AIndex: Integer): TCustomSvnTreeNode;
begin
  Result := FItems[AIndex];
end;

type
  TSvnTreeComparer<T: TCustomSvnTreeNode> = class(TObject, IInterface, IComparer<T>)
  private
    function GetTypePriority(AObj: TObject): Integer; inline;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function Compare(const Left, Right: T): Integer;
  end;

function TSvnTreeComparer<T>.GetTypePriority(AObj: TObject): Integer;
begin
  if AObj is TSvnTreeDir then
    Result := 0
  else
  if AObj is TSvnTreeFile then
    Result := 1
  else
    Result := 2;
end;

function TSvnTreeComparer<T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TSvnTreeComparer<T>._AddRef: Integer;
begin
  Result := -1;
end;

function TSvnTreeComparer<T>._Release: Integer;
begin
  Result := -1;
end;

function TSvnTreeComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := GetTypePriority(Left) - GetTypePriority(Right);
  if Result = 0 then
    Result := CompareStr(Left.ListItem.Path, Right.ListItem.Path);
end;

procedure TSvnTreeDir.SortItems;
var
  CustomSvnTreeNodeComparer: TSvnTreeComparer<TCustomSvnTreeNode>;
  SvnTreeDirComparer: TSvnTreeComparer<TSvnTreeDir>;
begin
  CustomSvnTreeNodeComparer := TSvnTreeComparer<TCustomSvnTreeNode>.Create;
  try
    FItems.Sort(CustomSvnTreeNodeComparer);
  finally
    CustomSvnTreeNodeComparer.Free;
  end;
  SvnTreeDirComparer := TSvnTreeComparer<TSvnTreeDir>.Create;
  try
    FDirectories.Sort(SvnTreeDirComparer);
  finally
    SvnTreeDirComparer.Free;
  end;
end;

procedure AddSvnToNode(ANode: TSvnTreeDir; ASvnClient: TSvnClient; AURL: string; ARevision: TSvnRevNum; ALevel: Integer = 0; AMaxDeep: Integer = 1);
var
  SvnList: TSvnList;
  I, J, Idx: Integer;
  SvnNode: TSvnTreeDir;
  SvnFileNode: TSvnTreeFile;
  NewItems: TSvnTreeDirList;
  Cursor: TCursor;
begin
  Cursor := Screen.Cursor;
  if ALevel = 0 then
    Screen.Cursor := crHourGlass;
  try
  SvnList := TSvnList.Create(ASvnClient);
  try
    ANode.ChildState := tncsLoading;
    SvnList.LoadList(AURL, svnDepthImmediates, False, SVN_DIRENT_ALL, ARevision);
    NewItems := TSvnTreeDirList.Create;
    try
      for I := 0 to Pred(SvnList.Count) do
        if (SvnList[I].Kind = svnNodeDir) and (Trim(SvnList[I].Path) <> '') then
        begin
          Idx := -1;
          //TODO: do check only in partialloadmode
          for J := 0 to Pred(ANode.DirCount) do
            if ANode.DirItems[J].URL = SvnIncludeTrailingPathDelimiter(AURL) + SvnList[I].Path then
            begin
              Idx := J;
              Break;
            end;
          if Idx = -1 then
          begin
            SvnNode := ANode.AddDir;
            SvnNode.ListItem.Assign(SvnList[I]);
            SvnNode.ListItem.AbsolutePath := SvnList[I].AbsolutePath;
            SvnNode.URL := SvnIncludeTrailingPathDelimiter(AURL) + SvnNode.ListItem.Path;
            NewItems.Add(SvnNode);
          end
          else
            ANode.DirItems[Idx].ListItem.Assign(SvnList[I]);
        end
        else
        if (SvnList[I].Kind = svnNodeFile) and (Trim(SvnList[I].Path) <> '') then
        begin
          SvnFileNode := ANode.AddFile;
          SvnFileNode.ListItem.Assign(SvnList[I]);
          SvnFileNode.URL := SvnIncludeTrailingPathDelimiter(AURL) + SvnFileNode.ListItem.Path;
        end;
      ANode.SortItems;
      ANode.ChildState := tncsLoaded;
      if ALevel < AMaxDeep then
        for I := 0 to NewItems.Count - 1 do
          AddSvnToNode(NewItems[I], ASvnClient, NewItems[I].URL, ARevision, ALevel + 1);
    finally
      NewItems.Free;
    end;
  finally
    SvnList.Free;
  end;
  finally
    if ALevel = 0 then
      Screen.Cursor := Cursor;
  end;
end;

procedure TFrmRepoBrowser.btnLoadClick(Sender: TObject);

  function GetRootNode(const APath: string): TSvnTreeDir;
  var
    SubPool: PAprPool;
    URL: PAnsiChar;
    I, P: Integer;
    S, Root: string;
    PathComponents: PAprArrayHeader;
    PathComponent: PPAnsiChar;
    PathList: TStringList;
    RootURL: string;
    Cursor: TCursor;
  begin
    Cursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
    Result := FRootNode;
    PathList := TStringList.Create;
    try
      AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
      try
        SvnCheck(svn_client_root_url_from_path(URL, svn_path_uri_encode(PAnsiChar(UTF8Encode(APath)), SubPool), SvnClient.Ctx, SubPool));
        Root := string(URL);
        PathList.Add(Root);
        S := APath;
        P := Pos(AnsiUpperCase(Root), AnsiUpperCase(S));
        if P > 0 then
          Delete(S, 1, P + Length(Root) - 1);

        PathComponents := svn_path_decompose(PAnsiChar(UTF8Encode(S)), SubPool);
        if Assigned(PathComponents) then
        begin
          repeat
            PathComponent := apr_array_pop(PathComponents);
            if Assigned(PathComponent) then
              if PathComponent^ <> '/' then
                PathList.Insert(1, UTF8ToString(PathComponent^));
          until PathComponent = nil;
        end;
      finally
        apr_pool_destroy(SubPool);
      end;
      RootURL := '';
      for I := 0 to PathList.Count - 1 do
      begin
        Result := Result.AddDir;
        Result.ListItem.Path := PathList[I];
        Result.URL := SvnIncludeTrailingPathDelimiter(RootURL) + PathList[I];
        RootURL := Result.URL;
        if I < PathList.Count - 1 then
          Result.ChildState := tncsPartiallyLoaded
        else
          Result.ChildState := tncsLoaded;
        Result.Expanded := True;
      end;
    finally
      PathList.Free;
    end;
    finally
      Screen.Cursor := Cursor;
    end;
  end;

  function FindURLNode(ANodes: TTreeNodes; AURL: string): TTreeNode;
  var
    Node: TTreeNode;
  begin
    Result := nil;
    for Node in ANodes do
      if Assigned(Node.Data) and (TObject(Node.Data) is TCustomSvnTreeNode) and
        (TCustomSvnTreeNode(Node.Data).URL = AURL) then
      begin
        Result := Node;
        Break;
      end
  end;

var
  SvnNode: TSvnTreeDir;
  URLNode: TTreeNode;
begin
  FileListViewText := '';//TODO: fill this with some text like 'Loading...'
  FileListViewMode := lvmText;
  try
  FRevision := StrToIntDef(edRevision.Text, -1);
  FRootNode.Clear;
  SvnNode := GetRootNode(edURL.Text);
  AddSvnToNode(SvnNode, FSvnClient, edURL.Text, FRevision, 0, 0);
  tvFolders.Items.BeginUpdate;
  FLoading := True;
  try
    tvFolders.Items.Clear;
    AddNodeToTree(FRootNode, nil);
    URLNode := FindURLNode(tvFolders.Items, edURL.Text);
    if Assigned(URLNode) then
      tvFolders.Selected := URLNode;
  finally
    FLoading := False;
    tvFolders.Items.EndUpdate;
  end;
  except
    on E: Exception do
    begin
      if E is ESvnError then
      begin
        tvFolders.Items.Clear;
        FileListViewText := E.Message;
        FileListViewMode := lvmText;
      end
      else
        raise;
  end;
end;
end;

function GetWindowsDir: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  SetString(Result, Buffer, GetWindowsDirectory(Buffer, SizeOf(Buffer)));
end;

type
  TImageListAccess = class(TImageList);

function Intensity(const R, G, B: Single): Single;
const
  RFactor =  61 / 256;
  GFactor = 174 / 256;
  BFactor =  21 / 256;
begin
  Result := RFactor * R + GFactor * G + BFactor * B;
end;

procedure BitmapGray(ABitmap: TBitmap);
var
  LX, LY: Integer;
  TC: TColor;
  R, G, B, CS: Single;
  C, CR, CG, CB: Byte;
begin
  for LX := 0 to Pred(ABitmap.Width) do
    for LY := 0 to Pred(ABitmap.Height) do
    begin
      TC := ABitmap.Canvas.Pixels[LX, LY];
      R := (TC shr 16) and 255;
      G := (TC shr 8) and 255;
      B := TC and 255;
      CS := Intensity(R, G, B);
      CS := 128 + CS * 2/4;
      if CS > 255 then
        C := 255
      else
        C := Round(CS);
      CB := C;
      CG := C;
      CR := C;
      ABitmap.Canvas.Pixels[LX, LY] := RGB(CB, CG, CR);
    end;
end;

constructor TFrmRepoBrowser.Create(AOwner: TComponent);
var
  FileInfo: SHFILEINFO;
  Image, Mask: TBitmap;
begin
  inherited Create(AOwner);
  Name := Format('%s_%d', [Name, FUseCount]);
  Inc(FUseCount);
  FRootNode := TSvnTreeDir.Create;
  FActiveDir := FRootNode;
  FFolderIndex := -1;
  FRevision := -1;
  FImageDictionary := TDictionary<string, Integer>.Create;
  FLoading := False;

  SysImageList.Handle := SHGetFileInfo('', 0, FileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SHGetFileInfo(PChar(GetWindowsDir), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);

  Image := TBitmap.Create;
  Mask := TBitmap.Create;
  try
    Image.Width := ImageList.Width;
    Image.Height := ImageList.Height;
    Mask.Width := ImageList.Width;
    Mask.Height := ImageList.Height;
    TImageListAccess(SysImageList).GetImages(FileInfo.iIcon, Image, Mask);
    BitmapGray(Image);
    ImageList.Add(Image, Mask);
  finally
    Image.Free;
    Mask.Free;
  end;
  FFolderIndex := ImageList.AddImage(SysImageList, FileInfo.iIcon);
end;

destructor TFrmRepoBrowser.Destroy;
begin
  FImageDictionary.Free;
  FRootNode.Free;
  inherited Destroy;
end;

procedure TFrmRepoBrowser.edURLChange(Sender: TObject);
begin
  EnableLoad;
end;

procedure TFrmRepoBrowser.edURLKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnLoad.Click;
end;

procedure TFrmRepoBrowser.AddDirToListView(ATreeDir: TSvnTreeDir);
var
  I: Integer;
  LvItem: TListItem;
  ListItem: TSvnListItem;
begin
  lvItems.Items.BeginUpdate;
  try
    FileListViewMode := lvmFiles;
    lvItems.Items.Clear;
    if Assigned(ATreeDir) then
      for I := 0 to ATreeDir.Count - 1 do
      begin
        ListItem := ATreeDir[I].ListItem;
        LvItem := lvItems.Items.Add;
        LvItem.Data := ATreeDir[I];
        if ListItem.Kind = svnNodeDir then
          LvItem.ImageIndex := FFolderIndex
        else
          LvItem.ImageIndex := GetImageIndexByExt(ExtractFileExt(ListItem.Path));
        LvItem.Caption := ListItem.Path;
        LvItem.SubItems.Add(ListItem.LastAuthor);
        LvItem.SubItems.Add(DateTimeToStr(ListItem.Time));
        if ListItem.Kind = svnNodeFile then
          LvItem.SubItems.Add(FormatFloat('#,', ListItem.Size))
        else
          LvItem.SubItems.Add('');
        LvItem.SubItems.Add(IntToStr(ListItem.CreatedRevision));
      end;
  finally
    lvItems.Items.EndUpdate;
  end;
  UpdateSelectedURL;
end;

procedure TFrmRepoBrowser.AddNodeToTree(ATreeDir: TSvnTreeDir; ANode: TTreeNode);
var
  I, ImageIndex: Integer;
  Node, Node2: TTreeNode;
begin
  for I := 0 to ATreeDir.DirCount - 1 do
  begin
    if Assigned(ANode) then
      Node := tvFolders.Items.AddChild(ANode, ATreeDir.DirItems[I].ListItem.Path)
    else
      Node := tvFolders.Items.Add(nil, ATreeDir.DirItems[I].ListItem.Path);
    Node.Data := ATreeDir.DirItems[I];
    ImageIndex := FFolderIndex;
    if ATreeDir.DirItems[I].ChildState = tncsPartiallyLoaded then
      ImageIndex := 0;
    Node.ImageIndex := ImageIndex;
    Node.SelectedIndex := ImageIndex;
    if ATreeDir.DirItems[I].DirCount > 0 then
      AddNodeToTree(ATreeDir.DirItems[I], Node)
    else
    if (ATreeDir.DirItems[I].DirCount = 0) and (ATreeDir.DirItems[I].ChildState = tncsNotLoaded) then
    begin
      Node2 := tvFolders.Items.AddChild(Node, ATreeDir.DirItems[I].ListItem.Path);
      Node2.Data := nil;
    end;
    if ATreeDir.DirItems[I].Expanded then
      Node.Expanded := True;
  end;
end;

procedure TFrmRepoBrowser.EnableLoad;
begin
  btnLoad.Enabled := Trim(edURL.Text) <> '';
end;

function TFrmRepoBrowser.GetImageIndexByExt(const AExtension: string): Integer;
var
  FileInfo: SHFILEINFO;
begin
  if not FImageDictionary.TryGetValue(AExtension, Result) then
  begin
    SHGetFileInfo(PChar('1' + AExtension), FILE_ATTRIBUTE_NORMAL, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
    Result := TImageListAccess(ImageList).AddImage(SysImageList, FileInfo.iIcon);
    FImageDictionary.Add(AExtension, Result);
  end;
end;

function TFrmRepoBrowser.GetSvnEditState: TSvnEditState;
begin
  if edURL.Focused then
    Result := ControlToSvnEditState(edURL)
  else
    Result := [];
end;

function TFrmRepoBrowser.GetURL: string;
begin
  Result := edURL.Text;
end;

procedure TFrmRepoBrowser.lvItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
 if FFileListViewMode = lvmFiles then
  UpdateSelectedURL;
end;

procedure TFrmRepoBrowser.lvItemsCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  ItemRect, TextRect: TRect;
begin
  DefaultDraw := FFileListViewMode = lvmFiles;
  if FFileListViewMode = lvmText then
  begin
    Assert(lvItems.Items.Count > 0);
    TextRect := lvItems.Items[0].DisplayRect(drBounds);
    ItemRect := ARect;
    ItemRect.Top := (TextRect.Top + TextRect.Bottom) div 2;
    lvItems.Canvas.TextRect(ItemRect, FFileListViewText, [tfCenter, tfWordBreak]);
  end;
end;

procedure TFrmRepoBrowser.lvItemsResize(Sender: TObject);
begin
  if FFileListViewMode = lvmText then
    lvItems.Repaint;
end;

function TFrmRepoBrowser.PerformEditAction(
  AEditAction: TSvnEditAction): Boolean;
begin
  if edURL.Focused then
    Result := PerformDefaultSvnEditAction(edURL, AEditAction)
  else
    Result := False;
end;

procedure TFrmRepoBrowser.SetFileListViewMode(
  const AValue: TRepoBrowserFileListViewMode);
begin
  if FFileListViewMode <> AValue then
  begin
    FFileListViewMode := AValue;
    lvItems.Clear;
    //Dummy item for the text position
    if FFileListViewMode = lvmText then
      lvItems.Items.Add;
    lvItems.Repaint;
  end;
end;

procedure TFrmRepoBrowser.SetFileListViewText(const AValue: string);
begin
  FFileListViewText := AValue;
  if FFileListViewMode = lvmText then
    lvItems.Repaint;
end;

procedure TFrmRepoBrowser.SetURL(const AValue: string);
begin
  edURL.Text := AValue;
  EnableLoad;
end;

procedure TFrmRepoBrowser.tvFoldersChange(Sender: TObject; Node: TTreeNode);
var
  TreeDir: TSvnTreeDir;
  WasExpanded: Boolean;
begin
  if Assigned(Node) and Assigned(Node.Data) then
    TreeDir := TSvnTreeDir(Node.Data)
  else
    TreeDir := nil;
  FActiveDir := TreeDir;
  if Assigned(TreeDir) and (TreeDir.ChildState in [tncsPartiallyLoaded, tncsNotLoaded]) then
  begin
    AddSvnToNode(TreeDir, FSvnClient, TreeDir.URL, FRevision, 0, 0);
    WasExpanded := Node.Expanded;
    tvFolders.Items.BeginUpdate;
    try
      Node.DeleteChildren;
      AddNodeToTree(TreeDir, Node);
      Node.ImageIndex := FFolderIndex;
      Node.SelectedIndex := FFolderIndex;
      Node.Expanded := WasExpanded;
    finally
      tvFolders.Items.EndUpdate;
    end;
  end;
  AddDirToListView(TreeDir);
end;

procedure TFrmRepoBrowser.tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  TreeDir: TSvnTreeDir;
begin
  if not FLoading then
  begin
    if Assigned(Node) and Assigned(Node.Data) then
      TreeDir := TSvnTreeDir(Node.Data)
    else
      TreeDir := nil;
    if Assigned(TreeDir) and (TreeDir.ChildState in [tncsPartiallyLoaded, tncsNotLoaded]) then
    begin
      AddSvnToNode(TreeDir, FSvnClient, TreeDir.URL, FRevision, 0, 0);
      tvFolders.Items.BeginUpdate;
      try
        Node.DeleteChildren;
        AddNodeToTree(TreeDir, Node);
      finally
        tvFolders.Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TFrmRepoBrowser.UpdateSelectedURL;
begin
  if Assigned(lvItems.Selected) and Assigned(lvItems.Selected.Data) and
    (TObject(lvItems.Selected.Data) is TCustomSvnTreeNode) then
    URL := TCustomSvnTreeNode(lvItems.Selected.Data).URL
  else
    URL := FActiveDir.URL;
end;

initialization
  TFrmRepoBrowser.FUseCount := 0;
end.
