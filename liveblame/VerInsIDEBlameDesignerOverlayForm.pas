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
{ The Original Code is VerInsIDEBlameDesignerOverlayForm.pas.                  }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2015 - 2017 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEBlameDesignerOverlayForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Generics.Defaults, Generics.Collections, ToolsAPI,
  VerInsLiveBlameTypes;

type
  TLiveBlameComponentInfo = class(TObject)
    Component: TComponent;
    Color: TColor;
    Revision: TJVCSLineHistoryRevision;
  end;

  TLiveBlameComponentComparer = class(TInterfacedObject, IComparer<TLiveBlameComponentInfo>)
    function Compare(const Left, Right: TLiveBlameComponentInfo): Integer;
  end;

  TLiveBlameComponentInfoList = class(TObject)
  private
    FItems: TObjectList<TLiveBlameComponentInfo>;
    FSortRequired: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AComponent: TComponent): TLiveBlameComponentInfo;
    procedure Clear;
    function GetInfo(AComponent: TComponent): TLiveBlameComponentInfo;
    function GetInfoByName(const AComponentName: string): TLiveBlameComponentInfo;
  end;

  TfmLiveBlameDesignerOverlay = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FInfoList: TLiveBlameComponentInfoList;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    { Public declarations }
    property InfoList: TLiveBlameComponentInfoList read FInfoList write FInfoList;
  end;

var
  fmLiveBlameDesignerOverlay: TfmLiveBlameDesignerOverlay;

implementation

{$R *.dfm}

{ TLiveBlameComponentComparer }

function TLiveBlameComponentComparer.Compare(const Left, Right: TLiveBlameComponentInfo): Integer;
begin
  Result := NativeInt(Left.Component) - NativeInt(Right.Component);
end;

{ TLiveBlameComponentInfoList }

constructor TLiveBlameComponentInfoList.Create;
begin
  inherited Create;
  FItems := TObjectList<TLiveBlameComponentInfo>.Create;
  FSortRequired := False;
end;

destructor TLiveBlameComponentInfoList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TLiveBlameComponentInfoList.Add(AComponent: TComponent): TLiveBlameComponentInfo;
begin
  FItems.Add(TLiveBlameComponentInfo.Create);
  Result := FItems.Last;
  Result.Component := AComponent;
  FSortRequired := True;
end;

procedure TLiveBlameComponentInfoList.Clear;
begin
  FItems.Clear;
  FSortRequired := False;
end;

function TLiveBlameComponentInfoList.GetInfo(AComponent: TComponent): TLiveBlameComponentInfo;
var
  Idx: Integer;
  SearchInfo: TLiveBlameComponentInfo;
  Comparer: TLiveBlameComponentComparer;
begin
  Comparer := TLiveBlameComponentComparer.Create;
  if FSortRequired then
  begin
    FItems.Sort(Comparer);
    FSortRequired := False;
  end;
  SearchInfo := TLiveBlameComponentInfo.Create;
  try
    SearchInfo.Component := AComponent;
    if FItems.BinarySearch(SearchInfo, Idx, Comparer) then
      Result := FItems[Idx]
    else
      Result := nil;
  finally
    SearchInfo.Free;
  end;
end;

function TLiveBlameComponentInfoList.GetInfoByName(const AComponentName: string): TLiveBlameComponentInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if Assigned(FItems[I].Component) and (FItems[I].Component.Name = AComponentName) then
    begin
      Result := FItems[I];
      Break;
    end;
end;

procedure TfmLiveBlameDesignerOverlay.FormPaint(Sender: TObject);

  procedure PaintCompo(R: TWinControl; C: TControl);
  var
    P, PM: TPoint;
    I: Integer;
    CI: TLiveBlameComponentInfo;
    CW: TWinControl absolute C;
  begin
    PM := ClientToScreen(Point(0, 0));
    P := C.ClientToScreen(Point(0, 0));
    P.X := P.X - PM.X;
    P.Y := P.Y - PM.Y;
    CI := nil;
    if Assigned(FInfoList) then
      CI := FInfoList.GetInfo(C);
    if Assigned(CI) then
      Canvas.Brush.Color := CI.Color
    else
      Canvas.Brush.Color := clSilver;
    Canvas.FillRect(Rect(P.X, P.Y, P.X + C.Width, P.Y + C.Height));
    if C is TWinControl then
      for I := 0 to CW.ControlCount - 1 do
        PaintCompo(R, CW.Controls[I]);
  end;

var
  Module: IOTAModule;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
  I: Integer;
  C: TWinControl;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  FormEditor := nil;
  for I := 0 to Pred(Module.GetModuleFileCount) do
  begin
    Editor := Module.GetModuleFileEditor(I);
    if Assigned(Editor) and (Editor.QueryInterface(IOTAFormEditor, FormEditor) = S_OK) then
      Break;
  end;
  if Assigned(FormEditor) and (TObject(FormEditor.GetRootComponent.GetComponentHandle) is TWinControl) then
  begin
    C := TWinControl(TObject(FormEditor.GetRootComponent.GetComponentHandle));
    for I := 0 to Pred(C.ControlCount) do
      PaintCompo(C, C.Controls[I]);
  end
end;

procedure TfmLiveBlameDesignerOverlay.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

end.
