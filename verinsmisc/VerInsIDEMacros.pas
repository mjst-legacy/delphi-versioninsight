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
{ The Original Code is VerInsIDEMacros.pas.                                    }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2012 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsIDEMacros;

interface

{$IFDEF TOOLSPROAPI}
uses
  Classes, ToolsProAPI;

type
  TOTAProMacroParameter = class(TInterfacedObject, IOTAProMacroParameter)
  private
    FDisplayName: string;
    FName: string;
  public
    constructor Create(const AName, ADisplayName: string);
    function GetDisplayName: string;
    function GetName: string;
  end;

  TOTAMacro = class(TInterfacedObject, IOTAProMacro)
  private
    FDisplayName: string;
    FName: string;
    FParameters: TInterfaceList;
  public
    constructor Create(const AName, ADisplayName: string);
    destructor Destroy; override;
    procedure AddParameter(const AName, ADisplayName: string);
    function GetDisplayName: string;
    function GetName: string;
    function GetParameterCount: Integer;
    function GetParameters(AIndex: Integer): IOTAProMacroParameter;
  end;
{$ENDIF TOOLSPROAPI}

implementation

{$IFDEF TOOLSPROAPI}
{ TOTAProMacroParameter }

constructor TOTAProMacroParameter.Create(const AName, ADisplayName: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := ADisplayName;
end;

function TOTAProMacroParameter.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TOTAProMacroParameter.GetName: string;
begin
  Result := FName;
end;

{ TOTAMacro }

constructor TOTAMacro.Create(const AName, ADisplayName: string);
begin
  inherited Create;
  FName := AName;
  FDisplayName := ADisplayName;
  FParameters := TInterfaceList.Create;
end;

destructor TOTAMacro.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TOTAMacro.AddParameter(const AName, ADisplayName: string);
begin
  FParameters.Add(TOTAProMacroParameter.Create(AName, ADisplayName));
end;

function TOTAMacro.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TOTAMacro.GetName: string;
begin
  Result := FName;
end;

function TOTAMacro.GetParameterCount: Integer;
begin
  Result := FParameters.Count;
end;

function TOTAMacro.GetParameters(AIndex: Integer): IOTAProMacroParameter;
begin
  Result := FParameters[AIndex] as IOTAProMacroParameter;
end;
{$ENDIF TOOLSPROAPI}

end.
