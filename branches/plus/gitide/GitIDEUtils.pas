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
unit GitIDEUtils;

interface

uses
  GitUITypes, DesignIntf;

function EditActionToSvnEditAction(AEditAction: TEditAction): TSvnEditAction;
function SvnEditStateToEditState(ASvnEditState: TSvnEditState): TEditState;
function SaveAll: Boolean;

implementation

uses SysUtils, ToolsApi;

function EditActionToSvnEditAction(AEditAction: TEditAction): TSvnEditAction;
begin
  case AEditAction of
    eaUndo     : Result := seaUndo;
    eaRedo     : Result := seaRedo;
    eaCut      : Result := seaCut;
    eaCopy     : Result := seaCopy;
    eaPaste    : Result := seaPaste;
    eaDelete   : Result := seaDelete;
    eaSelectAll: Result := seaSelectAll;
    else
      Result := seaUnknown;
  end;
end;

function SvnEditStateToEditState(ASvnEditState: TSvnEditState): TEditState;
begin
  Result := [];
  if sesCanUndo in ASvnEditState then
    Include(Result, esCanUndo);
  if sesCanRedo in ASvnEditState then
    Include(Result, esCanRedo);
  if sesCanCut in ASvnEditState then
    Include(Result, esCanCut);
  if sesCanCopy in ASvnEditState then
    Include(Result, esCanCopy);
  if sesCanPaste in ASvnEditState then
    Include(Result, esCanPaste);
  if sesCanDelete in ASvnEditState then
    Include(Result, esCanDelete);
  if sesCanSelectAll in ASvnEditState then
    Include(Result, esCanSelectAll);
end;

function SaveAll: Boolean;
begin
  Result := (BorlandIDEServices as IOTAModuleServices).SaveAll;
end;

end.