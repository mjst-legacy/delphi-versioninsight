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
{ The Original Code is SvnIDEColors.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 - 2015 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit SvnIDEColors;

interface

uses
  Graphics, svn_client;

type
  TSvnColorKind = (ssckConflicted, ssckAdded, ssckDeleted, ssckMerged, ssckModified);

  TSvnColorArray = array[TSvnColorKind] of TColor;

  TSvnColors = class(TObject)
  private
    FColors: TSvnColorArray;
    FDefaultColors: TSvnColorArray;
    FEnabled: Boolean;
  public
    constructor Create;
    function GetLogActionColor(ActionChar: Char): TColor;
    function GetNotifyActionColor(Action: TSvnWcNotifyAction; AContentState: TSvnWCNotifyState): TColor;
    function GetStatusColor(Status: TSvnWCStatusKind): TColor;
    procedure Load;
    procedure Save;
    property Colors: TSvnColorArray read FColors write FColors;
    property DefaultColors: TSvnColorArray read FDefaultColors;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

uses
  TypInfo, Registry, SvnIDEClient;

const
  cColors = 'Colors';
  cColor = 'Color';
  cEnabled = 'Enabled';

{ TSvnColors }

constructor TSvnColors.Create;
begin
  FDefaultColors[ssckConflicted] := $000000FF;
  FDefaultColors[ssckAdded] := $00640064;
  FDefaultColors[ssckDeleted] := $00000064;
  FDefaultColors[ssckMerged] := $00006400;
  FDefaultColors[ssckModified] := $00A03200;
  FColors := FDefaultColors;
  FEnabled := True;
  Load;
end;

function TSvnColors.GetLogActionColor(ActionChar: Char): TColor;
begin
  if FEnabled then
  begin
    case ActionChar of
      'A': Result := FColors[ssckAdded];
      'M': Result := FColors[ssckModified];
      'D', 'R': Result := FColors[ssckDeleted];
      '-': Result := clGrayText;
      else
        Result := clNone;
    end;
  end
  else
    Result := clNone;
end;

function TSvnColors.GetNotifyActionColor(Action: TSvnWcNotifyAction;
  AContentState: TSvnWCNotifyState): TColor;
begin
  if FEnabled then
  begin
    case AContentState of
      svnWcNotifyStateConflicted: Result := FColors[ssckConflicted];
      svnWcNotifyStateMerged: Result := FColors[ssckMerged];
      else
        case Action of
          svnWcNotifyUpdateAdd: Result := FColors[ssckAdded];
          svnWcNotifyUpdateDelete: Result := FColors[ssckDeleted];
          svnWcNotifyTreeConflict: Result := FColors[ssckConflicted];
          else
            Result := clNone;
        end;
    end;
  end
  else
    Result := clNone;
end;

function TSvnColors.GetStatusColor(Status: TSvnWCStatusKind): TColor;
begin
  if FEnabled then
  begin
    case Status of
      svnWcStatusAdded: Result := FColors[ssckAdded];
      svnWcStatusMissing, svnWcStatusDeleted, svnWcStatusReplaced: Result := FColors[ssckDeleted];
      svnWcStatusMerged: Result := FColors[ssckMerged];
      svnWcStatusModified: Result := FColors[ssckModified];
      svnWcStatusConflicted: Result := FColors[ssckConflicted];
      else
        Result := clNone;
    end;
  end
  else
    Result := clNone;
end;

procedure TSvnColors.Load;
var
  Reg: TRegistry;
  BaseKey, Key: string;
  ColorKind: TSvnColorKind;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cColors;
    if not Reg.KeyExists(BaseKey) then
      Exit;
    Reg.OpenKeyReadOnly(BaseKey);
    Key := cEnabled;
    if Reg.ValueExists(Key) then
      FEnabled := Reg.ReadBool(Key);
    for ColorKind := Low(TSvnColorKind) to High(TSvnColorKind) do
    begin
      Key := GetEnumName(TypeInfo(TSvnColorKind), Ord(ColorKind));
      Key := cColor + Copy(Key, 5, MaxInt);
      if Reg.ValueExists(Key) then
        FColors[ColorKind] := Reg.ReadInteger(Key);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TSvnColors.Save;
var
  Reg: TRegistry;
  BaseKey, Key: string;
  ColorKind: TSvnColorKind;
begin
  Reg := TRegistry.Create;
  try
    BaseKey := BaseRegKey + cColors;
    Reg.OpenKey(BaseKey, True);
    Reg.WriteBool(cEnabled, FEnabled);
    for ColorKind := Low(TSvnColorKind) to High(TSvnColorKind) do
    begin
      Key := GetEnumName(TypeInfo(TSvnColorKind), Ord(ColorKind));
      Key := cColor + Copy(Key, 5, MaxInt);
      Reg.WriteInteger(Key, FColors[ColorKind]);
    end;
  finally
    Reg.Free;
  end;
end;

end.
