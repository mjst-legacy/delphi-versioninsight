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
unit HgUIUtils;

interface

uses
  Windows, Messages, Controls, HgUITypes, Generics.Defaults;

type
  TOrdinalStringComparer = class(TStringComparer)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean;
      reintroduce; overload; override;
    function GetHashCode(const Value: string): Integer;
      reintroduce; overload; override;
  end;

function ControlToSvnEditState(AControl: TControl): TSvnEditState; overload;
function ControlToSvnEditState(AControl: TControl; var AHandled: Boolean): TSvnEditState; overload;
function PerformDefaultSvnEditAction(AControl: TControl; AEditAction: TSvnEditAction): Boolean;

implementation

uses
  Clipbrd, StdCtrls, SysUtils;

{ TOrdinalStringComparer }

function TOrdinalStringComparer.Compare(const Left, Right: string): Integer;
var
  L, R: string;
  len, lenDiff: Integer;
begin
  L := AnsiLowerCase(Left);
  R := AnsiLowerCase(Right);
  len := Length(L);
  lenDiff := len - Length(R);
  if lenDiff < 0 then
    Inc(len, lenDiff);
  Result := BinaryCompare(PChar(L), PChar(R), len * SizeOf(Char));
  if Result = 0 then
    Exit(lenDiff);
end;

function TOrdinalStringComparer.Equals(const Left, Right: string): Boolean;
var
  len: Integer;
  L, R: string;
begin
  L := AnsiLowerCase(Left);
  R := AnsiLowerCase(Right);
  len := Length(L);
  Result := (len - Length(R) = 0) and CompareMem(PChar(L), PChar(R), len * SizeOf(Char));
end;

function TOrdinalStringComparer.GetHashCode(const Value: string): Integer;
var
  S: string;
begin
  S := AnsiLowerCase(Value);
  Result := BobJenkinsHash(PChar(S)^, SizeOf(Char) * Length(S), 0);
end;

function ControlToSvnEditState(AControl: TControl): TSvnEditState;
var
  DummyHandled: Boolean;
begin
  Result := ControlToSvnEditState(AControl, DummyHandled);
end;

type
  TControlAccess = class(TControl);
  TCustomComboAccess = class(TCustomCombo);

function ControlToSvnEditState(AControl: TControl; var AHandled: Boolean): TSvnEditState;
var
  CustomEdit: TCustomEdit;
  CustomCombo: TCustomCombo;
begin
  Result := [];
  if AControl.Enabled and (AControl is TCustomEdit) then
  begin
    CustomEdit := TCustomEdit(AControl);
    if CustomEdit.CanUndo then
      Include(Result, sesCanUndo);
    if CustomEdit.SelLength > 0 then
    begin
      Include(Result, sesCanCopy);
      if not CustomEdit.ReadOnly then
      begin
        Include(Result, sesCanCut);
        Include(Result, sesCanDelete);
      end;
    end;
    if not CustomEdit.ReadOnly and (Clipboard.AsText <> '') then
      Include(Result, sesCanPaste);
    if (CustomEdit.Text <> '') and (CustomEdit.SelText <> CustomEdit.Text) then
      Include(Result, sesCanSelectAll);
    AHandled := True;
  end
  else
  if AControl.Enabled and (AControl is TCustomCombo) and
    (TCustomComboAccess(AControl).EditHandle <> 0) then
  begin
    CustomCombo := TCustomCombo(AControl);
    if SendMessage(TCustomComboAccess(AControl).EditHandle, EM_CANUNDO, 0, 0) <> 0 then
      Include(Result, sesCanUndo);
    if CustomCombo.SelLength > 0 then
    begin
      Include(Result, sesCanCopy);
      Include(Result, sesCanCut);
      Include(Result, sesCanDelete);
    end;
    if Clipboard.AsText <> '' then
      Include(Result, sesCanPaste);
    if (TControlAccess(CustomCombo).Text <> '') and
      (CustomCombo.SelLength <> Length(TControlAccess(CustomCombo).Text)) then
      Include(Result, sesCanSelectAll);
    AHandled := True;
  end
  else
    AHandled := False;
end;

function PerformDefaultSvnEditAction(AControl: TControl; AEditAction: TSvnEditAction): Boolean;
var
  CustomEdit: TCustomEdit;
begin
  if AControl.Enabled and (AControl is TCustomEdit) then
  begin
    CustomEdit := TCustomEdit(AControl);
    if CustomEdit.ReadOnly and not (AEditAction in [seaCopy, seaSelectAll]) then
      Result := False
    else
    begin
      Result := True;
      case AEditAction of
        seaUndo     : CustomEdit.Undo;
        seaCut      : CustomEdit.CutToClipboard;
        seaCopy     : CustomEdit.CopyToClipboard;
        seaPaste    : CustomEdit.PasteFromClipboard;
        seaDelete   : CustomEdit.ClearSelection;
        seaSelectAll: CustomEdit.SelectAll;
        else
          Result := False;
      end;
    end;
  end
  else
  if AControl.Enabled and (AControl is TCustomCombo) and
    (TCustomComboAccess(AControl).EditHandle <> 0) then
  begin
    Result := True;
    case AEditAction of
      seaUndo     : SendMessage(TCustomComboAccess(AControl).EditHandle, WM_UNDO, 0, 0);
      seaCut      : SendMessage(TCustomComboAccess(AControl).EditHandle, WM_CUT, 0, 0);
      seaCopy     : SendMessage(TCustomComboAccess(AControl).EditHandle, WM_COPY, 0, 0);
      seaPaste    : SendMessage(TCustomComboAccess(AControl).EditHandle, WM_PASTE, 0, 0);
      seaDelete   : SendMessage(TCustomComboAccess(AControl).EditHandle, WM_CLEAR, 0, 0);
      seaSelectAll: SendMessage(TCustomComboAccess(AControl).EditHandle, EM_SETSEL, 0, -1);
      else
        Result := False;
    end;
  end
  else
    Result := False;
end;

end.