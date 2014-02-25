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
{ The Original Code is SvnTree.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2014 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnTree;

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  TCustomSvnTreeItem<T: class, constructor> = class(TObject)
  private
    FParent: TCustomSvnTreeItem<T>;
    FData: T;
  public
    constructor Create(AParent: TCustomSvnTreeItem<T>);
    property Data: T read FData;
    property Parent: TCustomSvnTreeItem<T> read FParent;
  end;

  TSvnLeaf<T: class, constructor> = class(TCustomSvnTreeItem<T>);

  TSvnNode<T: class, constructor> = class(TCustomSvnTreeItem<T>)
  private
    FNodes: TObjectList<TSvnNode<T>>;
    FLeaves: TObjectList<TSvnLeaf<T>>;
    function GetNodeCount: Integer;
    function GetNodes(AIndex: Integer): TSvnNode<T>;
    function GetLeafCount: Integer;
    function GetLeaves(AIndex: Integer): TSvnLeaf<T>;
  public
    constructor Create(AParent: TCustomSvnTreeItem<T>);
    destructor Destroy; override;
    function AddLeaf: TSvnLeaf<T>;
    function AddNode: TSvnNode<T>;
    property LeafCount: Integer read GetLeafCount;
    property Leaves[AIndex: Integer]: TSvnLeaf<T> read GetLeaves;
    property NodeCount: Integer read GetNodeCount;
    property Nodes[AIndex: Integer]: TSvnNode<T> read GetNodes;
  end;

  {$IFNDEF ICE273479Fixed}
  TDataKind = (dkRoot, dkFile, dkFolder);
  {$ENDIF}

  TSvnTreeData = class(TObject)
  private
    {$IFDEF ICE273479Fixed}
    type
      TDataKind = (dkRoot, dkFile, dkFolder);
    {$ENDIF}
    var
      FFileName: string;
      FKind: TDataKind;
      FInternal: Boolean;
      FName: string;
  public
    constructor Create;
    property FileName: string read FFileName;
    property Kind: TDataKind read FKind;
    property Internal: Boolean read FInternal;
    property Name: string read FName;
  end;

  TSvnRootNodeWalkModes = set of (wmEnumChilds, wmInfo);

  TSvnRootNode<T: TSvnTreeData, constructor> = class(TSvnNode<T>)
  private
    FDirMapping: TDictionary<string, TSvnNode<T>>;
    function IndexOfNode(ANode: TSvnNode<T>; const AName: string): Integer;
    function InternalAddDir(AStr: string; AInternal: Boolean): TSvnNode<T>;

    type
      TEnumerator = class(TEnumerator<TCustomSvnTreeItem<T>>)
      private
        FIndex: Integer;
        FList: TList<TCustomSvnTreeItem<T>>;
        procedure EnumFolders(ANode: TSvnNode<T>);
      protected
        function DoGetCurrent: TCustomSvnTreeItem<T>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ARootNode: TSvnRootNode<T>);
        destructor Destroy; override;
      end;

      TWalkProc = reference to procedure(ANode: TObject; AData: T; var AWalkModes: TSvnRootNodeWalkModes);

    procedure InternalWalk(ANode: TSvnNode<T>; AWalkProc: TWalkProc; AWalkModes: TSvnRootNodeWalkModes);
  public
    constructor Create;
    destructor Destroy; override;
    function AddDir(const AStr: string): TSvnNode<T>;
    function AddFile(const AStr: string): TSvnLeaf<T>;
    function GetEnumerator: TEnumerator; reintroduce;
    procedure WalkThrough(AWalkProc: TWalkProc; ANode: TObject = nil);
  end;

implementation

{ TCustomSvnTreeItem<T> }

constructor TCustomSvnTreeItem<T>.Create(AParent: TCustomSvnTreeItem<T>);
begin
  inherited Create;
  FParent := AParent;
  FData := T.Create;
end;

{ TSvnNode<T> }

constructor TSvnNode<T>.Create(AParent: TCustomSvnTreeItem<T>);
begin
  inherited Create(AParent);
  FNodes := nil;
  FLeaves := nil;
end;

destructor TSvnNode<T>.Destroy;
begin
  FLeaves.Free;
  FNodes.Free;
  inherited Destroy;
end;

function TSvnNode<T>.AddLeaf: TSvnLeaf<T>;
begin
  if not Assigned(FLeaves) then
    FLeaves := TObjectList<TSvnLeaf<T>>.Create;
  FLeaves.Add(TSvnLeaf<T>.Create(Self));
  Result := FLeaves.Last;
end;

function TSvnNode<T>.AddNode: TSvnNode<T>;
begin
  if not Assigned(FNodes) then
    FNodes := TObjectList<TSvnNode<T>>.Create;
  FNodes.Add(TSvnNode<T>.Create(Self));
  Result := FNodes.Last;
end;

function TSvnNode<T>.GetLeafCount: Integer;
begin
  if Assigned(FLeaves) then
    Result := FLeaves.Count
  else
    Result := 0;
end;

function TSvnNode<T>.GetLeaves(AIndex: Integer): TSvnLeaf<T>;
begin
  Result := FLeaves[AIndex];
end;

function TSvnNode<T>.GetNodeCount: Integer;
begin
  if Assigned(FNodes) then
    Result := FNodes.Count
  else
    Result := 0;
end;

function TSvnNode<T>.GetNodes(AIndex: Integer): TSvnNode<T>;
begin
  Result := FNodes[AIndex];
end;

{ TSvnTreeData }

constructor TSvnTreeData.Create;
begin
  inherited Create;
  FFileName := '';
  FKind := dkRoot;
  FInternal := True;
  FName := '';
end;

{ TSvnRootNode<T> }

constructor TSvnRootNode<T>.Create;
begin
  inherited Create(nil);
  FDirMapping := TDictionary<string, TSvnNode<T>>.Create;
end;

destructor TSvnRootNode<T>.Destroy;
begin
  FDirMapping.Free;
  inherited Destroy;
end;

function TSvnRootNode<T>.AddDir(const AStr: string): TSvnNode<T>;
begin
  Result := InternalAddDir(AStr, False);
end;

function TSvnRootNode<T>.AddFile(const AStr: string): TSvnLeaf<T>;
var
  Dir: TSvnNode<T>;
  Data: T;
begin
  Dir := InternalAddDir(ExtractFilePath(AStr), True);
  Result := Dir.AddLeaf;
  Data := Result.Data;
  Data.FFileName := AStr;
  Data.FKind := dkFile;
  Data.FName := ExtractFileName(AStr);
end;

function TSvnRootNode<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TSvnRootNode<T>.IndexOfNode(ANode: TSvnNode<T>; const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ANode.NodeCount - 1 do
    if ANode.Nodes[I].Data.Name = AName then
    begin
      Result := I;
      Break;
    end;
end;

function TSvnRootNode<T>.InternalAddDir(AStr: string;
  AInternal: Boolean): TSvnNode<T>;
var
  S: string;
  I, I2, Idx, P, L: Integer;
  DirectoryParts: TStringList;
  Node: TSvnNode<T>;
begin
  Result := nil;
  AStr := ExcludeTrailingPathDelimiter(AStr);
  if FDirMapping.TryGetValue(AnsiLowerCase(AStr), Result) then
  begin
    if Result.Data.Internal and (not AInternal) then
    begin
      Result.Data.FInternal := AInternal;
      Result.Data.FFileName := AStr;
    end;
  end
  else
  begin
    DirectoryParts := TStringList.Create;
    try
      I := 1;
      P := 1;
      L := Length(AStr);
      I2 := 0;
      while I <= L do
      begin
        if CharInSet(AStr[I], ['/', '\']) then
        begin
          if I2 > 0 then
            DirectoryParts.Add(Copy(AStr, P, I2));
          I2 := 0;
          P := I + 1;
        end
        else
        begin
          Inc(I2);
          if I = L then
            DirectoryParts.Add(Copy(AStr, P, I2));
        end;
        Inc(I);
      end;

      Node := Self;
      Idx := -1;
      for I := 0 to DirectoryParts.Count - 1 do
      begin
        I2 := IndexOfNode(Node, DirectoryParts[I]);
        if I2 <> -1 then
          Node := Node.Nodes[I2]
        else
        begin
          Idx := I;
          Break;
        end;
      end;
      if Idx <> -1 then
      begin
        S := '';
        if Node <> Self then
          S := Node.Data.FFileName;
        for I := Idx to DirectoryParts.Count - 1 do
        begin
          Node := Node.AddNode;
          if S <> '' then
            S := S + '\';
          S := S + DirectoryParts[I];
          Node.Data.FFileName := S;
          Node.Data.FKind := dkFolder;
          Node.Data.FName := DirectoryParts[I];
          FDirMapping.Add(AnsiLowerCase(S), Node);
        end;
      end;
      Result := Node;
      if not AInternal then
      begin
        Result.Data.FInternal := AInternal;
        Result.Data.FFileName := AStr;
      end;
    finally
      DirectoryParts.Free;
    end;
  end;
end;

procedure TSvnRootNode<T>.InternalWalk(ANode: TSvnNode<T>;
  AWalkProc: TWalkProc; AWalkModes: TSvnRootNodeWalkModes);
var
  I: Integer;
  OldWalkMode: TSvnRootNodeWalkModes;
begin
  for I := 0 to ANode.NodeCount - 1 do
  begin
    OldWalkMode := AWalkModes;
    try
      if not ANode.Nodes[I].Data.Internal then
        AWalkProc(ANode.Nodes[I], ANode.Nodes[I].Data, AWalkModes);
      if wmEnumChilds in AWalkModes then
        InternalWalk(ANode.Nodes[I], AWalkProc, AWalkModes);
    finally
      AWalkModes := OldWalkMode;
    end;
  end;
  for I := 0 to ANode.LeafCount - 1 do
    AWalkProc(ANode.Leaves[I], ANode.Leaves[I].Data, AWalkModes);
end;

procedure TSvnRootNode<T>.WalkThrough(AWalkProc: TWalkProc; ANode: TObject = nil);
var
  Node: TSvnNode<T>;
begin
  if Assigned(ANode) then
    Node := ANode as TSvnNode<T>
  else
    Node := Self;
  InternalWalk(Node, AWalkProc, [wmEnumChilds]);
end;

{ TSvnRootNode<T>.TEnumerator }

constructor TSvnRootNode<T>.TEnumerator.Create(ARootNode: TSvnRootNode<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := TList<TCustomSvnTreeItem<T>>.Create;
  EnumFolders(ARootNode);
end;

destructor TSvnRootNode<T>.TEnumerator.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TSvnRootNode<T>.TEnumerator.DoGetCurrent: TCustomSvnTreeItem<T>;
begin
  Result := FList[FIndex];
end;

function TSvnRootNode<T>.TEnumerator.DoMoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

procedure TSvnRootNode<T>.TEnumerator.EnumFolders(ANode: TSvnNode<T>);
var
  I: Integer;
begin
  if not ANode.Data.Internal then
    FList.Add(ANode);
  for I := 0 to ANode.LeafCount - 1 do
    FList.Add(ANode.Leaves[I]);
  for I := 0 to ANode.NodeCount - 1 do
    EnumFolders(ANode.Nodes[I]);
end;

end.
