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
{ The Initial Developer of the Original Code is Ondrej Kelle.                  }
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
{ Christian Wimmer                                                             }
{ Embarcadero Technologies                                                     }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains helper routines and classes encapsulating Subversion API  }
{ calls.                                                                       }
{                                                                              }
{******************************************************************************}

unit SvnClient;

interface

uses
  Windows, Classes, SysUtils, Contnrs, SyncObjs, apr, svn_client, svnconst;

const
  DefaultPropValDelimiter = ';';

type
  TSvnClient = class;
  TSvnItem = class;

  IAnnotationLineProvider = interface
    ['{3568F26C-40E4-436A-A129-5B7D5E6E47AF}']
    function GetCount: Integer;
    function GetGutterInfo(Index: Integer): string;
    function GetIntensity(Index: Integer): Integer;
    function GetMaxGutterWidth: Integer;
    function GetHintStr(Index: Integer): string;

    property Count: Integer read GetCount;
    property GutterInfo[Index: Integer]: string read GetGutterInfo;
    property MaxGutterWidth: Integer read GetMaxGutterWidth;
    property HintStr[Index: Integer]: string read GetHintStr;
    property Intensity[Index: Integer]: Integer read GetIntensity;
  end;

  IAnnotationCompletion = interface
    ['{AA7CE09D-ECCA-42FA-896A-2EFE65FDDCE3}']
    procedure AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
  end;

  IAsyncUpdate = interface
    ['{80185CE5-DE85-4FAB-BFD0-836DF6C1C1EC}']
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;
  end;

  TSvnBlameItem = class
  private
    FAuthor: string;
    FLine: string;
    FLineNo: Int64;
    FRevision: Integer;
    FTime: TDateTime;
  public
    property Author: string read FAuthor;
    property Line: string read FLine;
    property LineNo: Int64 read FLineNo;
    property Revision: Integer read FRevision;
    property Time: TDateTime read FTime;
  end;

  TSvnHistoryItem = class
  private
    FAuthor: string;
    FBlame: TList;
    FBlameError: string;
    FBlameNotify: IAnnotationCompletion;
    FBlameThread: TThread;
    FCancelBlame: Boolean;
    FChangeFiles: TStringList;
    FDestroying: Boolean;
    FFile: TBytes;
    FLogMessage: string;
    FOwner: TSvnItem;
    FRevision: Integer;
    FTime: TDateTime;
    FMinTime: Integer;
    FMaxTime: Integer;
    FMinRevision: TSvnRevNum;
    FMaxRevision: TSvnRevNum;

    procedure BlameCallback(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string; Time: TDateTime;
      const Line: string; var Cancel: Boolean);
    procedure BlameThreadTerminate(Sender: TObject);
    procedure ClearBlame;
    function GetBlameCount: Integer;
    function GetBlameItems(Index: Integer): TSvnBlameItem;
    function GetChangeFiles: TStringList;
    function GetHintString: string;
    procedure ReloadBlame; overload;
    procedure ReloadBlame(ASvnClient: TSvnClient; const APathName: string; ABaseRevision: Integer); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CancelBlame;
    function GetFile: TBytes;
    function HasBlameLoaded: Boolean;
    function IsLoadingBlame: Boolean;
    procedure StartLoadingBlame(ANotify: IAnnotationCompletion = nil);

    property Author: string read FAuthor;
    property BlameCount: Integer read GetBlameCount;
    property BlameError: string read FBlameError;
    property BlameItems[Index: Integer]: TSvnBlameItem read GetBlameItems;
    property ChangeFiles: TStringList read GetChangeFiles;
    property HintString: string read GetHintString;
    property LogMessage: string read FLogMessage;
    property MinTime: Integer read FMinTime;
    property MaxTime: Integer read FMaxTime;
    property MinRevision: TSvnRevNum read FMinRevision;
    property MaxRevision: TSvnRevNum read FMaxRevision;
    property Owner: TSvnItem read FOwner;
    property Revision: Integer read FRevision;
    property Time: TDateTime read FTime;
  end;

  THistoryUpdateThread = class(TThread)
  private
    FNewItemIndex: Integer;
    FLastAdded: Integer;
    FSvnItem: TSvnItem;
    FAsyncUpdate: IAsyncUpdate;
    FRevision: TSvnRevNum;
    FAuthor: PAnsiChar;
    FDate: PAnsiChar;
    FMessage: PAnsiChar;
    FPool: PAprPool;
  protected
    procedure Completed(Sender: TObject);
    procedure Execute; override;
    procedure InternalUpdate(ForceUpdate: Boolean);
  public
    constructor Create(SvnItem: TSvnItem; const AsyncUpdate: IAsyncUpdate);
    procedure NewUpdate;
    procedure ResetUpdate;
    procedure Update;
    property NewItemIndex: Integer read FNewItemIndex;
    property Revision: TSvnRevNum read FRevision write FRevision;
    property Author: PAnsiChar read FAuthor write FAuthor;
    property Date: PAnsiChar read FDate write FDate;
    property Message: PAnsiChar read FMessage write FMessage;
    property Pool: PAprPool read FPool write FPool;

  end;

  TSvnItem = class
  private
    FAbsent: Boolean;
    FAsyncUpdate: IAsyncUpdate;
    FBaseRevision: Integer;
    FChangeList: string;
    FCheckSum: string;
    FCommitAuthor: string;
    FCommittedRevision: Integer;
    FCommitTime: TDateTime;
    FConflictNewFile: string;
    FConflictOldFile: string;
    FConflictWorkingFile: string;
    FCopied: Boolean;
    FCopiedFromRevision: Integer;
    FCopiedFromURL: string;
    FDeleted: Boolean;
    FDestroyNotifications: TList;
    FFileAttr: Cardinal;
    FHistory: TList;
    FHistoryUpdateThread: THistoryUpdateThread;
    FIncludeChangeFiles: Boolean;
    FIncomplete: Boolean;
    FItems: TList;
    FKind: TSvnNodeKind;
    FLargeImageIndex: Integer;
    FLastCommitAuthor: string;
    FLastCommitRevision: Integer;
    FLastCommitTime: TDateTime;
    FLockComment: string;
    FLockDAVComment: Boolean;
    FLocked: Boolean;
    FLockExpirationTime: TDateTime;
    FLockOwner: string;
    FLockPath: string;
    FLockTime: TDateTime;
    FLockToken: string;
    FLogLimit: Integer;
    FLogFirstRev: TSvnRevNum;
    FLogLastRev: TSvnRevNum;
    FParent: TSvnItem;
    FPathName: string;
    FPropRejectFile: string;
    FProps: TStringList;
    FPropStatus: TSvnWCStatusKind;
    FPropTime: TDateTime;
    FPropValDelimiter: Char;
    FNextAsyncUpdate: IAsyncUpdate;
    FReloadExternals: TList;
    FReloadGlobalExternals: TList;
    FReloadRecursive: Boolean;
    FReloadStack: TStack;
    FReloadUnversioned: TList;
    FRemotePropStatus: TSvnWCStatusKind;
    FRemoteTextStatus: TSvnWCStatusKind;
    FRepository: string;
    FSchedule: TSvnWCSchedule;
    FSmallImageIndex: Integer;
    FSvnClient: TSvnClient;
    FSvnPathName: string;
    FSwitched: Boolean;
    FTag: Integer;
    FTextStatus: TSvnWCStatusKind;
    FTextTime: TDateTime;
    FURL: string;
    FUUID: string;

    procedure ClearHistory;
    procedure ClearItems;
    function GetCount: Integer;
    function GetExternal: Boolean;
    function GetFileAttr: Cardinal;
    function GetHintStrings(Index: Integer): string;
    function GetHistoryCount: Integer;
    function GetHistoryItems(Index: Integer): TSvnHistoryItem;
    function GetIsDirectory: Boolean;
    function GetItems(Index: Integer): TSvnItem;
    function GetPropCount: Integer;
    function GetPropNames(Index: Integer): string;
    function GetPropValueFromIndex(Index: Integer): string;
    function GetPropValues(const Name: string): string;
    procedure LoadStatus(const Status: TSvnWcStatus2);
    procedure LoadUnversionedPaths;
    procedure ReloadHistory;
    procedure ReloadProps;
    procedure SetPropValues(const Name, Value: string);
    procedure SortItems(Recurse: Boolean);
  protected
    procedure DoDestroyNotifications; virtual;
    procedure DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2);
  public
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string; Recurse: Boolean = False;
      Update: Boolean = False; DoReload:Boolean = False); overload;
    constructor Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
      const Status: TSvnWcStatus2); overload;
    { TSvnItem for URLs is some kind of a hack and right now don't expect
      anything else to work than asynchronous history loading }
    constructor Create(ASvnClient: TSvnClient; const AURL: string); overload;
    destructor Destroy; override;

    function Add(Item: TSvnItem): Integer;
    procedure AddDestroyNotification(Notification: TNotifyEvent);
    procedure AsyncReloadHistory;
    procedure AsyncUpdateUI(NewItemIndex: Integer);
    procedure Clear;
    function GetBaseFile: TBytes;
    function GetCommittedFile: TBytes;
    procedure HistoryThreadTerminated;
    function IndexOf(Item: TSvnItem): Integer; overload;
    function IndexOf(const PathName: string; SvnPath: Boolean = False): Integer; overload;
    procedure PauseAsynchronousUpdate;
    procedure Reload(Recurse: Boolean = False; Update: Boolean = False);
    procedure ReloadStatus;
    procedure Remove(Item: TSvnItem);
    procedure RemoveDestroyNotification(Notification: TNotifyEvent);
    procedure Resolved(Recurse: Boolean = False);
    procedure ResumeAsynchronousUpdate;
    procedure ScheduleAsyncReload(NextAsyncUpdate: IAsyncUpdate);
    procedure TerminateAsynchronousUpdate(WaitForTerminate: Boolean);

    property Absent: Boolean read FAbsent;
    property AsyncUpdate: IAsyncUpdate read FAsyncUpdate write FAsyncUpdate;
    property BaseRevision: Integer read FBaseRevision;
    property ChangeList: string read FChangeList;
    property CheckSum: string read FCheckSum;
    property CommitAuthor: string read FCommitAuthor;
    property CommittedRevision: Integer read FCommittedRevision;
    property CommitTime: TDateTime read FCommitTime;
    property ConflictNewFile: string read FConflictNewFile;
    property ConflictOldFile: string read FConflictOldFile;
    property ConflictWorkingFile: string read FConflictWorkingFile;
    property Copied: Boolean read FCopied;
    property CopiedFromRevision: Integer read FCopiedFromRevision;
    property CopiedFromURL: string read FCopiedFromURL;
    property Count: Integer read GetCount;
    property Deleted: Boolean read FDeleted;
    property External: Boolean read GetExternal;
    property FileAttr: Cardinal read GetFileAttr;
    property HintStrings[Index: Integer]: string read GetHintStrings;
    property HistoryCount: Integer read GetHistoryCount;
    property HistoryItems[Index: Integer]: TSvnHistoryItem read GetHistoryItems;
    property Incomplete: Boolean read FIncomplete;
    property IncludeChangeFiles: Boolean read FIncludeChangeFiles write FIncludeChangeFiles;
    property Items[Index: Integer]: TSvnItem read GetItems; default;
    property IsDirectory: Boolean read GetIsDirectory;
    property Kind: TSvnNodeKind read FKind;
    property LargeImageIndex: Integer read FLargeImageIndex write FLargeImageIndex;
    property LastCommitAuthor: string read FLastCommitAuthor;
    property LastCommitRevision: Integer read FLastCommitRevision;
    property LastCommitTime: TDateTime read FLastCommitTime;
    property LockComment: string read FLockComment;
    property LockDAVComment: Boolean read FLockDAVComment;
    property LockExpirationTime: TDateTime read FLockExpirationTime;
    property Locked: Boolean read FLocked;
    property LockOwner: string read FLockOwner;
    property LockPath: string read FLockPath;
    property LockTime: TDateTime read FLockTime;
    property LockToken: string read FLockToken;
    property LogLimit: Integer read FLogLimit write FLogLimit;
    property LogFirstRev: TSvnRevNum read FLogFirstRev write FLogFirstRev;
    property LogLastRev: TSvnRevNum read FLogLastRev write FLogLastRev;
    property Parent: TSvnItem read FParent;
    property PathName: string read FPathName;
    property PropCount: Integer read GetPropCount;
    property PropNames[Index: Integer]: string read GetPropNames;
    property PropRejectFile: string read FPropRejectFile;
    property PropStatus: TSvnWCStatusKind read FPropStatus;
    property PropTime: TDateTime read FPropTime;
    property PropValDelimiter: Char read FPropValDelimiter write FPropValDelimiter default ';';
    property PropValueFromIndex[Index: Integer]: string read GetPropValueFromIndex;
    property PropValues[const Name: string]: string read GetPropValues write SetPropValues;
    property RemotePropStatus: TSvnWCStatusKind read FRemotePropStatus;
    property RemoteTextStatus: TSvnWCStatusKind read FRemoteTextStatus;
    property Repository: string read FRepository;
    property Schedule: TSvnWCSchedule read FSchedule;
    property SmallImageIndex: Integer read FSmallImageIndex write FSmallImageIndex;
    property SvnClient: TSvnClient read FSvnClient;
    property SvnPathName: string read FSvnPathName;
    property Switched: Boolean read FSwitched;
    property TextTime: TDateTime read FTextTime;
    property Tag: Integer read FTag write FTag;
    property TextStatus: TSvnWCStatusKind read FTextStatus;
    property URL: string read FURL;
    property UUID: string read FUUID;
  end;

  TSvnListItem = class(TPersistent)
  private
    FAbsolutePath: string;
    FCreatedRevision: TSvnRevNum;
    FHasProps: Boolean;
    FPath: string;
    FKind: TSvnNodeKind;
    FLastAuthor: string;
    FLockComment: string;
    FLockDAVComment: Boolean;
    FLockExpirationTime: TDateTime;
    FLocked: Boolean;
    FLockOwner: string;
    FLockPath: string;
    FLockTime: TDateTime;
    FLockToken: string;
    FSize: TSvnFileSize;
    FTime: TDateTime;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AbsolutePath: string read FAbsolutePath write FAbsolutePath;
    property CreatedRevision: TSvnRevNum read FCreatedRevision write FCreatedRevision;
    property HasProps: Boolean read FHasProps write FHasProps;
    property Path: string read FPath write FPath;
    property Kind: TSvnNodeKind read FKind write FKind;
    property LastAuthor: string read FLastAuthor write FLastAuthor;
    property LockComment: string read FLockComment write FLockComment;
    property LockDAVComment: Boolean read FLockDAVComment write FLockDAVComment;
    property Locked: Boolean read FLocked write FLocked;
    property LockExpirationTime: TDateTime read FLockExpirationTime write FLockExpirationTime;
    property LockOwner: string read FLockOwner write FLockOwner;
    property LockPath: string read FLockPath write FLockPath;
    property LockTime: TDateTime read FLockTime write FLockTime;
    property LockToken: string read FLockToken write FLockToken;
    property Size: TSvnFileSize read FSize write FSize;
    property Time: TDateTime read FTime write FTime;
  end;

  TSvnList = class(TObject)
  private
    FDepth: TSvnDepth;
    FDirEntryFields: DWORD;
    FFetchLocks: Boolean;
    FItems: TObjectList;
    FPathName: string;
    FSvnClient: TSvnClient;
    procedure Clear;
    procedure ListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
      AbsPath: string; var Cancel: Boolean);
    function GetCount: Integer;
    function GetItems(Index: Integer): TSvnListItem;
  public
    constructor Create(ASvnClient: TSvnClient);
    destructor Destroy; override;
    procedure LoadList(const APathName: string; ADepth: TSvnDepth; AFetchLocks: Boolean; ADirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1);
    property Count: Integer read GetCount;
    property Depth: TSvnDepth read FDepth;
    property DirEntryFields: DWORD read FDirEntryFields;
    property FetchLocks: Boolean read FFetchLocks;
    property Items[Index: Integer]: TSvnListItem read GetItems; default;
    property PathName: string read FPathName;
  end;

  TSvnMergeRevisionRange = class(TObject)
  private
    FEndRevision: TSvnRevNum;
    FStartRevision: TSvnRevNum;
  public
    constructor Create(AStartRevision: TSvnRevNum; AEndRevision: TSvnRevNum);
    property EndRevision: TSvnRevNum read FEndRevision;
    property StartRevision: TSvnRevNum read FStartRevision;
  end;

  TSvnMergeRevisionList = class(TPersistent)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TSvnMergeRevisionRange;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRevision(ARevision: TSvnRevNum);
    procedure AddRevisionRange(AStartRevision: TSvnRevNum; AEndRevision: TSvnRevNum);
    procedure Clear;
    function IsValidRangeStr(const AValue: string): Boolean;
    procedure PrepareForMerge(AHeadRevision: TSvnRevNum = -1; AReverseMerge: Boolean = False);
    function ToAprArray(ASvnClient: TSvnClient; SubPool: PAprPool = nil): PAprArrayHeader;
    property AsString: string read GetAsString write SetAsString;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TSvnMergeRevisionRange read GetItems; default;
  end;

  TSvnConfigItemState = (scisDefault, scisSet, scisRemove);

  TSvnConfigString = class(TObject)
  private
    FState: TSvnConfigItemState;
    FValue: string;
    procedure SetValue(AValue: string);
  public
    constructor Create;
    property State: TSvnConfigItemState read FState write FState;
    property Value: string read FValue write SetValue;
  end;

  TSvnCustomConfig = class(TObject)
  protected
    procedure GetStringValue(AConfig: PSvnConfig; ASection, AOption: PAnsiChar; AConfigString: TSvnConfigString);
    procedure SetStringValue(AConfig: PSvnConfig; ASection, AOption: PAnsiChar; AConfigString: TSvnConfigString);
  public
    procedure GetConfig(AConfig: PSvnConfig); virtual; abstract;
    procedure SetConfig(AConfig: PSvnConfig); virtual; abstract;
  end;

  TSvnServerConfig = class(TSvnCustomConfig)
  private
    FHttpProxyExceptions: TSvnConfigString;
    FHttpProxyHost: TSvnConfigString;
    FHttpProxyPassword: TSvnConfigString;
    FHttpProxyPort: TSvnConfigString;
    FHttpProxyUsername: TSvnConfigString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetConfig(AConfig: PSvnConfig); override;
    procedure SetConfig(AConfig: PSvnConfig); override;
    property HttpProxyExceptions: TSvnConfigString read FHttpProxyExceptions;
    property HttpProxyHost: TSvnConfigString read FHttpProxyHost;
    property HttpProxyPassword: TSvnConfigString read FHttpProxyPassword;
    property HttpProxyPort: TSvnConfigString read FHttpProxyPort;
    property HttpProxyUsername: TSvnConfigString read FHttpProxyUsername;
  end;

  TSvnBlameOptions = class(TPersistent)
  private
    FIgnoreEOL: Boolean;
    FIgnoreSpace: Boolean;
    FIgnoreSpaceAll: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property IgnoreEOL: Boolean read FIgnoreEOL write FIgnoreEOL;
    property IgnoreSpace: Boolean read FIgnoreSpace write FIgnoreSpace;
    property IgnoreSpaceAll: Boolean read FIgnoreSpaceAll write FIgnoreSpaceAll;
  end;

  TSvnItemArray = array of TSvnItem;

  TSSLServerTrustFailures = set of (sslCertNotYetValid, sslCertExpired, sslCertHostNameMismatch,
    sslCertAuthorityUnknown, sslCertOther);

  TLoginPromptEvent = procedure(Sender: TObject; const Realm: string; var UserName, Password: string;
    var Cancel, Save: Boolean) of object;
  TUserNamePromptEvent = procedure(Sender: TObject; const Realm: string; var UserName: string;
    var Cancel, Save: Boolean) of object;
  TSSLServerTrustPrompt = procedure(Sender: TObject; const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
    Failures: TSSLServerTrustFailures; var Cancel, Save: Boolean) of object;
  TSSLClientCertPrompt = procedure(Sender: TObject; const Realm: string; var CertFileName: string;
    var Cancel, Save: Boolean) of object;
  TSSLClientPasswordPrompt = procedure(Sender: TObject; const Realm: string; var Password: string;
    var Cancel, Save: Boolean) of object;

  TSvnBlameCallback = procedure(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string;
    Time: TDateTime; const Line: string; var Cancel: Boolean) of object;
  TSvnListCallback = procedure(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
    AbsPath: string; var Cancel: Boolean) of object;
  TSvnNotifyCallback = procedure(Sender: TObject; const Path, MimeType: string; Action: TSvnWcNotifyAction;
    Kind: TSvnNodeKind; ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum; var Cancel: Boolean)
    of object;
  TSvnStatusCallback = procedure(Sender: TObject; Item: TSvnItem; var Cancel: Boolean) of object;

  TSvnCancelCallback = procedure(Sender: TObject; var Cancel: Boolean) of object;
  TSvnProgressCallback = procedure(Sender: TObject; Progress, Total: TAprOff) of object;
  TSvnConflictCallback = function(Sender: TObject; var ResultFileName: string;
    var Choice: TSvnWcConflictChoice; description: PSvnWcConflictDescription): Boolean of object;

  PStatusEntry = ^TStatusEntry;
  TStatusEntry = record
    Name: string;
    Revision: TSvnRevNum;
    URL: string;
    Repos: string;
    Uuid: string;
    Kind: TSvnNodeKind;
    Schedule: TSvnWcSchedule;
    Copied: TSvnBoolean;
    Deleted: TSvnBoolean;
    Absent: TSvnBoolean;
    Incomplete: TSvnBoolean;
    Copyfrom_url: string;
    Copyfrom_rev: TSvnRevNum;
    Conflict_old: string;
    Conflict_new: string;
    Conflict_wrk: string;
    Prejfile: string;
    Text_time: TAprTime;
    Prop_time: TAprTime;
    Checksum: string;
    Cmt_rev: TSvnRevNum;
    Cmt_date: TAprTime;
    Cmt_author: string;
    Lock_token: string;
    Lock_owner: string;
    Lock_comment: string;
    Lock_creation_date: TAprTime;
    Has_props: TSvnBoolean;
    Has_prop_mods: TSvnBoolean;
    Cachable_props: string;
    Present_props: string;
    Changelist: string;
    Working_size: TAprOff;
    Keep_local: TSvnBoolean;
    Depth: TSvnDepth;
    Valid: Boolean;
  end;

  TSvnClient = class
  private
    FAllocator: PAprAllocator;
    FAprLibLoaded: Boolean;
    FBlameOptions: TSvnBlameOptions;
    FCancelled: Boolean;
    FChangeLists: TStrings;
    FCommitLogMessage: string;
    FConfigDir: string;
    FCtx: PSvnClientCtx;
    FExternals: TStrings;
    FLastCommitInfoRevision: TSvnRevNum;
    FListStrings: TStrings;
    FPassword: string;
    FPool: PAprPool; // main pool
    FPoolUtf8: PAprPool; // pool for UTF-8 routines
    FRecurseUnversioned: Boolean;
    FServerConfig: TSvnServerConfig;
    FSvnClientLibLoaded: Boolean;
    FUserName: string;

    FBlameCallback: TSvnBlameCallback;
    FBlameSubPool: PAprPool;
    FListCallback: TSvnListCallback;
    FNotifyCallback: TSvnNotifyCallback;
    FStatusCallback: TSvnStatusCallback;
    FConflictCallback: TSvnConflictCallback;

    FOnLoginPrompt: TLoginPromptEvent;
    FOnSSLClientCertPrompt: TSSLClientCertPrompt;
    FOnSSLClientPasswordPrompt: TSSLClientPasswordPrompt;
    FOnSSLServerTrustPrompt: TSSLServerTrustPrompt;
    FOnUserNamePrompt: TUserNamePromptEvent;

    FOnCancel: TSvnCancelCallback;
    FOnProgress: TSvnProgressCallback;

    procedure CheckDllLoadError(const DLLName: string);
    procedure GetExternalsCallback(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
    function GetInitialized: Boolean;
    procedure GetListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
      AbsPath: string; var Cancel: Boolean);
  protected
    function DoBlame(LineNo: Int64; Revision: TSvnRevNum; const Author, Date, Line: string): Boolean;
    function DoCancel: Boolean;
    function DoList(Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock; AbsPath: string): Boolean;
    function DoLoginPrompt(const Realm: string; var UserName, Password: string; var Save: Boolean): Boolean; virtual;
    function DoNotify(const Path, MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
      ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean; virtual;
    procedure DoProgress(Progress, Total: TAprOff);
    function DoSSLClientCertPrompt(const Realm: string; var CertFileName: string; var Save: Boolean): Boolean; virtual;
    function DoSSLClientPasswordPrompt(const Realm: string; var Password: string; var Save: Boolean): Boolean; virtual;
    function DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
      Failures: TSSLServerTrustFailures; var Save: Boolean): Boolean; virtual;
    function DoUserNamePrompt(const Realm: string; var UserName: string; var Save: Boolean): Boolean; virtual;
    function DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2): Boolean;

    property Cancelled: Boolean read FCancelled;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const PathName: string; Recurse: Boolean = False; Force: Boolean = False; NoIgnore: Boolean = False;
      SubPool: PAprPool = nil);
    procedure AddToChangeList(PathNames: TStrings; const AChangeList: string; SvnDepth: TSvnDepth = svnDepthFiles; SubPool: PAprPool = nil);
    procedure Blame(const PathName: string; Callback: TSvnBlameCallback; StartRevision: TSvnRevNum = 1;
      EndRevision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
    procedure Checkout(const PathName, TargetDir: string; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      IgnoreExternals: Boolean = False; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1;
      SvnCancelCallback: TSvnCancelCallback = nil; SubPool: PAprPool = nil);
    procedure Cleanup(const PathName: string; SubPool: PAprPool = nil);
    function Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
      Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;
    procedure Export(const PathName, TargetDir: string; Callback: TSvnNotifyCallback = nil; Overwrite: Boolean = False;
      Recurse: Boolean = True; IgnoreExternals: Boolean = False; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
    procedure Finalize;
    function FindRepositoryRoot(const Path: string; SubPool: PAprPool = nil): string;
    function FindRepository(const Path: string; SubPool: PAprPool = nil): string;
    function GetBaseURL(AFilesAndDirectories: TStringList; var ABasePath: string): string;
    procedure GetChangeLists(const PathName: string; ChangeLists: TStrings; SvnDepth: TSvnDepth = svnDepthInfinity; SubPool: PAprPool = nil);
    procedure GetExternals(const PathName: string; Externals: TStrings; Recurse: Boolean = True);
    function GetHeadRevision(const URL: string; SubPool: PAprPool = nil): TSvnRevNum;
    function GetMaxRevision(const PathName: string; SubPool: PAprPool = nil): Integer;
    function GetModifications(const PathName: string; Callback: TSvnStatusCallback = nil;
      Recurse: Boolean = True; Update: Boolean = False; IgnoreExternals: Boolean = False;
      RecurseUnversioned: Boolean = False; SubPool: PAprPool = nil): TSvnRevNum;
    procedure GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
      Delimiter: Char = DefaultPropValDelimiter);
    procedure GetRevisionProperties(const URL: string; Revision: TSvnRevNum; Properties: TStrings;
      SubPool: PAprPool = nil; Delimiter: Char = DefaultPropValDelimiter);
    procedure GetFirstStatus(const PathName: string; var Status: TStatusEntry; SubPool: PAprPool = nil);
    procedure Import(const PathName, Url: string; SvnDepth: TSvnDepth = svnDepthInfinity);
    procedure Initialize(const AConfigDir: string = ''; Auth: PSvnAuthBaton = nil);
    function IsPathVersioned(const PathName: string): Boolean;
    procedure List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; DirEntryFields: DWORD = SVN_DIRENT_ALL;
      Callback: TSvnListCallback = nil; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil); overload;
    procedure List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; ListStrings: TStrings;
      DirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil); overload;
    procedure Merge(const Source1: string; Revision1: TSvnRevNum; const Source2: string; Revision2: TSvnRevNum;
      const TargetWcpath: string; Callback: TSvnNotifyCallback = nil; SvnCancelCallback: TSvnCancelCallback = nil;
      Depth: TSvnDepth = svnDepthInfinity; IgnoreAncestry: TSvnBoolean = False; Force: TSvnBoolean = False;
      RecordOnly: TSvnBoolean = False; DryRun: TSvnBoolean = False; SubPool: PAprPool = nil);
    procedure MergePeg(const Source: string; RangesToMerge: TSvnMergeRevisionList; const TargetWcpath: string;
      PegRevision: TSvnRevNum = -1; Callback: TSvnNotifyCallback = nil; SvnCancelCallback: TSvnCancelCallback = nil;
      Depth: TSvnDepth = svnDepthInfinity; IgnoreAncestry: TSvnBoolean = False; Force: TSvnBoolean = False;
      RecordOnly: TSvnBoolean = False; DryRun: TSvnBoolean = False; IgnoreEOL: Boolean = False; IgnoreSpace: Boolean = False;
      IgnoreSpaceAll: Boolean = False; SubPool: PAprPool = nil);
    function MkDir(const Paths: TStringList; const Comment: string; MakeParents: Boolean = False; SubPool: PAprPool = nil): Boolean;
    function MatchGlobalIgnores(const PathName: string; SubPool: PAprPool = nil): Boolean;
    procedure Move(SrcPathNames: TStrings; const DstPath: string; Force: Boolean = True; MoveAsChild: Boolean = False;
      MakeParents: Boolean = False; SubPool: PAprPool = nil);
    function NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;
    function PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool = nil): PAprArrayHeader; overload;
    function PathNamesToAprArray(const PathNames: array of string; SubPool: PAprPool = nil): PAprArrayHeader; overload;
    procedure RemoveFromChangeList(PathNames: TStrings; SvnDepth: TSvnDepth = svnDepthFiles; SubPool: PAprPool = nil);
    procedure Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      SubPool: PAprPool = nil);
    procedure Resolved(const SvnPath: string; ConflictChoice: TSvnWcConflictChoice = SvnWcConflictChooseMerged;
      Recurse: Boolean = False; SubPool: PAprPool = nil);
    procedure SaveFileContentToStream(const PathName: string; Revision: TSvnRevNum; OutputStream: TStream; SubPool: PAprPool = nil);
    procedure SetRevisionProperty(const URL: string; Revision: TSvnRevNum; PropName, PropValue: string; Force: Boolean = True; SubPool: PAprPool = nil);
    function StringListToAprArray(List: TStrings; SubPool: PAprPool = nil): PAprArrayHeader;
    function SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;
    procedure Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
      IgnoreExternals: Boolean = False; ConflictCallBack: TSvnConflictCallback = nil;
      SvnCancelCallback: TSvnCancelCallback = nil; SubPool: PAprPool = nil);

    property Allocator: PAprAllocator read FAllocator;
    property BlameOptions: TSvnBlameOptions read FBlameOptions;
    property ConfigDir: string read FConfigDir;
    property Ctx: PSvnClientCtx read FCtx;
    property Initialized: Boolean read GetInitialized;
    property LastCommitInfoRevision: TSvnRevNum read FLastCommitInfoRevision;
    property Password: string read FPassword write FPassword;
    property Pool: PAprPool read FPool;
    property PoolUtf8: PAprPool read FPoolUtf8;
    property ServerConfig: TSvnServerConfig read FServerConfig;
    property RecurseUnversioned: Boolean read FRecurseUnversioned;
    property UserName: string read FUserName write FUserName;

    property OnLoginPrompt: TLoginPromptEvent read FOnLoginPrompt write FOnLoginPrompt;
    property OnSSLClientCertPrompt: TSSLClientCertPrompt read FOnSSLClientCertPrompt write FOnSSLClientCertPrompt;
    property OnSSLClientPasswordPrompt: TSSLClientPasswordPrompt read FOnSSLClientPasswordPrompt
      write FOnSSLClientPasswordPrompt;
    property OnSSLServerTrustPrompt: TSSLServerTrustPrompt read FOnSSLServerTrustPrompt write FOnSSLServerTrustPrompt;
    property OnUserNamePrompt: TUserNamePromptEvent read FOnUserNamePrompt write FOnUserNamePrompt;

    property OnCancel: TSvnCancelCallback read FOnCancel write FOnCancel;
    property OnProgress: TSvnProgressCallback read FOnProgress write FOnProgress;
  end;

  DllLoadException = class(Exception)
  private
    FFileName: string;
    FLastError: Integer;
    function GetFileNotFoundError: Boolean;
  public
    constructor Create(const AFileName: string);
    property FileName: string read FFileName;
    property FileNotFoundError: Boolean read GetFileNotFoundError;
  end;

  TSvnClientAnnotationLineProvider = class(TInterfacedObject, IAnnotationLineProvider)
  protected
    FSvnHistoryItem: TSvnHistoryItem;
    FMaxGutterWidth: Integer;
  { IAnnotationLineProvider }
    function GetCount: Integer;
    function GetGutterInfo(Index: Integer): string;
    function GetIntensity(Index: Integer): Integer;
    function GetMaxGutterWidth: Integer;
    function GetHintStr(Index: Integer): string;
  public
    constructor Create(SvnHistoryItem: TSvnHistoryItem);
  end;

const
  SvnLineBreak = #10;
  SvnPathDelim = '/';
  NodeKindStrings: array[TSvnNodeKind] of string = (SNodeKindNone, SNodeKindFile, SNodeKindDir, SNodeKindUnknown);

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;
function DateTimeToAprTime(Value: TDateTime): TAprTime;
function SvnStrToDateTime(const S: string; Pool: PAprPool): TDateTime;
function TzToUTCDateTime(Value: TDateTime): TDateTime;
function UTCToTzDateTime(Value: TDateTime): TDateTime;

function FileAttrStr(Attr: Cardinal): string;
function NotifyActionStr(Action: TSvnWcNotifyAction): string;
function NotifyStateStr(State: TSvnWcNotifyState): string;
function StatusKindStr(Status: TSvnWCStatusKind): string;
function SvnExcludeTrailingPathDelimiter(const S: string): string;
function SvnExtractFileDrive(const SvnFileName: string): string;
function SvnExtractFileName(const SvnFileName: string): string;
function SvnExtractFilePath(const SvnFileName: string): string;
function SvnIncludeTrailingPathDelimiter(const S: string): string;

var
  BaseDllDir: string;
  Unloading: Boolean = False;

implementation

uses
  RTLConsts, ActiveX, ComObj, ShlObj, TypInfo, WinSock;

type
  PMethod = ^TMethod;

{ helper routines }

function TzToUTCDateTime(Value: TDateTime): TDateTime;
var
  TZ: TTimeZoneInformation;

begin
  Result := Value;
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result + (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result + (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result + TZ.Bias / MinsPerDay;
  end;
end;

function UTCToTzDateTime(Value: TDateTime): TDateTime;
var
  TZ: TTimeZoneInformation;
begin
  Result := Value;
  case GetTimeZoneInformation(TZ) of
    TIME_ZONE_ID_DAYLIGHT:
      Result := Result - (TZ.Bias + TZ.DaylightBias) / MinsPerDay;
    TIME_ZONE_ID_STANDARD:
      Result := Result - (TZ.Bias + TZ.StandardBias) / MinsPerDay;
    TIME_ZONE_ID_UNKNOWN:
      Result := Result - TZ.Bias / MinsPerDay;
  end;
end;

function AprTimeToDateTime(AprTime: TAprTime): TDateTime;
begin
  if AprTime = 0 then
    Result := 0
  else
    Result := UTCToTzDateTime(UnixDateDelta + AprTime / SecsPerDay / 1000000);
end;

function DateTimeToAprTime(Value: TDateTime): TAprTime;
begin
  if Value = 0 then
    Result := 0
  else
    Result := Round(TzToUTCDateTime(Value - UnixDateDelta) * SecsPerDay * 1000000);
end;

function CompareSvnPathNames(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TSvnItem(Item1).SvnPathName, TSvnItem(Item2).SvnPathName);
end;

function CompareNativePathNames(P1, P2: Pointer): Integer;
var
  Item1: TSvnItem absolute P1;
  Item2: TSvnItem absolute P2;
begin
  Result := 0;
  if Assigned(P1) and Assigned(P2) then
  begin
    Result := Ord(Item2.IsDirectory) - Ord(Item1.IsDirectory);
    if Result = 0 then
      Result := AnsiCompareText(Item1.PathName, Item2.PathName);
  end;
end;

function GetAppDataDir: string;
var
  Malloc: IMalloc;
  P: PItemIDList;
begin
  Result := '';

  OleCheck(SHGetMalloc(Malloc));

  if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_APPDATA, P)) then
  begin
    SetLength(Result, MAX_PATH);
    if SHGetPathFromIDList(P, PChar(Result)) then
      SetLength(Result, StrLen(PChar(Result)));
    Malloc.Free(P);
  end;
end;

function SvnStrToDateTime(const S: string; Pool: PAprPool): TDateTime;
var
  AprTime: TAprTime;
  Error: PSvnError;
begin
  Result := 0;
  AprTime := 0;
  Error := svn_time_from_cstring(AprTime, PAnsiChar(AnsiString(S)), Pool);
  if Assigned(Error) then
    svn_error_clear(Error)
  else
    Result := AprTimeToDateTime(AprTime);
end;

function BlameReceiver(baton: Pointer; line_no: Int64; revision: TSvnRevNum; author, date, line: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;
begin
  Result := nil;
  if revision <> SVN_INVALID_REVNUM then
    TSvnClient(baton).DoBlame(line_no, revision, UTF8ToString(author), string(date), string(line));
end;

function ChangeListReceiver(baton: Pointer; path, changelist: PAnsiChar; pool: PAprPool): PSvnError; cdecl;
begin
  Result := nil;
  TSvnClient(baton).FChangeLists.Add(UTF8ToString(changelist));
end;

function ConflictReceiver(out ConflictResult: PSvnWcConflictResult; description: PSvnWcConflictDescription;
    baton: Pointer; pool: PAprPool): PSvnError; cdecl;
var
  ResultFileName: string;
  Choice: TSvnWcConflictChoice;
begin
  TSvnClient(baton).FConflictCallback(TSvnClient(baton), ResultFileName, Choice, description);
  ConflictResult := apr_pcalloc(pool, SizeOf(TSvnWcConflictResult));
  ConflictResult^.merged_file := apr_pstrdup(pool, PAnsiChar(UTF8Encode(ResultFileName)));
  ConflictResult^.choice := Choice;
  Result := nil;
end;

function ListReceiver(baton: Pointer; path: PAnsiChar; dirent: PSvnDirEnt; lock: PSvnLock; abs_path: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;
var
  DirEntry: TSvnDirEnt;
  Locked: Boolean;
  LockData: TSvnLock;
begin
  Result := nil;

  if Assigned(dirent) then
    DirEntry := Dirent^
  else
  begin
    DirEntry.kind := svnNodeUnknown;
    DirEntry.size := 0;
    DirEntry.has_props := False;
    DirEntry.created_rev := 0;
    DirEntry.time := DateTimeToAprTime(0);
    DirEntry.last_author := nil;
  end;
  Locked := Assigned(lock);
  if Locked then
    LockData := lock^
  else
  begin
    LockData.path := nil;
    LockData.token := nil;
    LockData.owner := nil;
    LockData.comment := nil;
    LockData.is_dav_comment := False;
    LockData.creation_date := DateTimeToAprTime(0);
    LockData.expiration_date := DateTimeToAprTime(0);
  end;

  TSvnClient(baton).DoList(UTF8ToString(path), DirEntry, Locked, LockData, UTF8ToString(abs_path));
end;

function LogMessage(baton: Pointer; changed_paths: PAprHash; revision: TSvnRevNum; author, date, message: PAnsiChar;
  pool: PAprPool): PSvnError; cdecl;
var
  Item: TSvnHistoryItem;
  Time: TAprTime;
  NewItemIndex: Integer;
  H: PAprHashIndex;
  PName: PAnsiChar;
  PValue: Pointer;
begin
  Result := nil;

  if Assigned(baton) and ((not Assigned(TSvnItem(baton).FHistoryUpdateThread)) or TSvnItem(baton).FHistoryUpdateThread.Terminated) then
    Exit;

  Item := TSvnHistoryItem.Create;
  try
    Item.FOwner := baton;
    Item.FRevision := revision;
    Item.FAuthor := UTF8ToString(author);
    Item.FLogMessage := UTF8ToString(message);
    if Assigned(date) and (date^ <> #0) then
    begin
      SvnCheck(svn_time_from_cstring(Time, date, pool));
      Item.FTime := AprTimeToDateTime(Time);
    end
    else
      Item.FTime := 0;
    if TSvnItem(baton).IncludeChangeFiles and (changed_paths <> nil) then
    begin
      Item.FChangeFiles := TStringList.Create;
      H := apr_hash_first(Pool, changed_paths);
        while Assigned(H) do
        begin
          apr_hash_this(H, @PName, 0, @PValue);
          Item.FChangeFiles.Add(char(PSvnLogChangedPath(PValue).Action) + UTF8ToString(PName));
          H := apr_hash_next(H);
        end;
    end;
    NewItemIndex := TSvnItem(baton).FHistory.Add(Item);
    apr_pool_clear(pool);
    TSvnItem(baton).AsyncUpdateUI(NewItemIndex);
  except
    Item.Free;
    raise;
  end;
end;

function NotifyActionStr(Action: TSvnWcNotifyAction): string;
const
  NotifyActionStrings: array[TSvnWcNotifyAction] of string = (SWcNotifyAdd, SWcNotifyCopy, SWcNotifyDelete,
    SWcNotifyRestore, SWcNotifyRevert, SWcNotifyFailedRevert, SWcNotifyResolved, SWcNotifySkip, SWcNotifyUpdateDelete,
    SWcNotifyUpdateAdd, SWcNotifyUpdateUpdate, SWcNotifyUpdateCompleted, SWcNotifyUpdateExternal,
    SWcNotifyStatusCompleted, SWcNotifyStatusExternal, SWcNotifyCommitModified, SWcNotifyCommitAdded,
    SWcNotifyCommitDeleted, SWcNotifyCommitReplaced, SWcNotifyCommitPostfixTxdelta, SWcNotifyBlameRevision,
    SWcNotifyLocked, SWcNotifyUnlocked, SWcNotifyFailedLock, SWcNotifyFailedUnlock, SWcNotifyExists,
    SWcNotifyChangelistSet, SWcNotifyChangelistClear, SWcNotifyChangelistMoved, SWcNotifyMergeBegin,
    SWcNotifyForeignMergeBegin, SWcNotifyUpdateReplace, SWcNotifyPropertyAdded, SWcNotifyPropertyModified,
    SWcNotifyPropertyDeleted, SWcNotifyPropertyDeletedNonexistent, SWcNotifyRevpropSet, SWcNotifyRevpropDeleted,
    SWcNotifyMergeCompleted, SWcNotifyTreeConflict, SWcNotifyFailedExternal);
begin
  Result := NotifyActionStrings[Action];
end;

function NotifyStateStr(State: TSvnWcNotifyState): string;
const
  NotifyStateStrings: array[TSvnWcNotifyState] of string = (
    sWcNotifyStateInapplicable,
    sWcNotifyStateUnknown,
    sWcNotifyStateUnchanged,
    sWcNotifyStateMissing,
    sWcNotifyStateObstructed,
    sWcNotifyStateChanged,
    sWcNotifyStateMerged,
    sWcNotifyStateConflicted);
begin
  Result := NotifyStateStrings[State];
end;

function SimplePrompt(out cred: PSvnAuthCredSimple; baton: Pointer; realm, username: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;
var
  SRealm, SUserName, SPassword: string;
  Save: Boolean;
begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  if Assigned(username) then
    SetString(SUserName, username, StrLen(username))
  else
    SUserName := '';
  SPassword := '';
  Save := may_save;

  if not TSvnClient(baton).DoLoginPrompt(SRealm, SUserName, SPassword, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSimple));
    // leaving username or password nil would cause A/V
    cred^.username := apr_pstrdup(pool, PAnsiChar(UTF8Encode(SUserName)));
    cred^.password := apr_pstrdup(pool, PAnsiChar(UTF8Encode(SPassword)));
    cred^.may_save := Save;
  end
  else
    raise ESvnError.Create(svn_error_create(SVN_ERR_CANCELLED, nil, PAnsiChar(UTF8Encode(sCancelledByUser))));
end;

function PlaintextPrompt(out MaySavePlainText: TSVNBoolean; realm: PAnsiChar; baton: Pointer;
    pool: PAprPool): PSvnError; cdecl;
begin
  MaySavePlainText := True;
  Result := nil;
end;

function SSLClientCertPrompt(out cred: PSvnAuthCredSSLClientCert; baton: Pointer; realm: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;
var
  SRealm, SCertFileName: string;
  Save: Boolean;
begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SCertFileName := '';
  Save := may_save;
  if not TSvnClient(baton).DoSSLClientCertPrompt(SRealm, SCertFileName, Save) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCert));
    cred^.cert_file := apr_pstrdup(pool, PAnsiChar(UTF8Encode(SCertFileName)));
    cred^.may_save := Save;
  end;
end;

function SSLClientPasswordPrompt(out cred: PSvnAuthCredSSLClientCertPw; baton: Pointer; realm: PAnsiChar;
  may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
var
  SRealm, SPassword: string;
  Save: Boolean;
begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SPassword := '';
  Save := may_save;
  if not TSvnClient(baton).DoSSLClientPasswordPrompt(SRealm, SPassword, Save) then
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLClientCertPw));
    cred^.password := apr_pstrdup(pool, PAnsiChar(UTF8Encode(SPassword)));
    cred^.may_save := Save;
  end
  else
    raise ESvnError.Create(svn_error_create(SVN_ERR_CANCELLED, nil, PAnsiChar(UTF8Encode(sCancelledByUser))));
end;

function SSLServerTrustPrompt(out cred: PSvnAuthCredSSLServerTrust; baton: Pointer; realm: PAnsiChar; failures: Cardinal;
  cert_info: PSvnAuthSSLServerCertInfo; may_save: TSvnBoolean; pool: PAprPool): PSvnError; cdecl;
var
  SRealm: string;
  Failed: TSSLServerTrustFailures;
  Save: Boolean;
begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';

  Failed := [];
  if failures and SVN_AUTH_SSL_NOTYETVALID <> 0 then
    Include(Failed, sslCertNotYetValid);
  if failures and SVN_AUTH_SSL_EXPIRED <> 0 then
    Include(Failed, sslCertExpired);
  if failures and SVN_AUTH_SSL_CNMISMATCH <> 0 then
    Include(Failed, sslCertHostNameMismatch);
  if failures and SVN_AUTH_SSL_UNKNOWNCA <> 0 then
    Include(Failed, sslCertAuthorityUnknown);
  if failures and SVN_AUTH_SSL_OTHER <> 0 then
    Include(Failed, sslCertOther);
  Save := may_save;

  if not TSvnClient(baton).DoSSLServerTrustPrompt(SRealm, cert_info^, Failed, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredSSLServerTrust));
    cred^.may_save := Save;
    cred^.accepted_failures := failures;
  end
  else
    raise ESvnError.Create(svn_error_create(SVN_ERR_CANCELLED, nil, PAnsiChar(UTF8Encode(sCancelledByUser))));
end;

function FileAttrStr(Attr: Cardinal): string;
begin
  Result := '';
  if Attr and faReadOnly <> 0 then
    Result := Result + 'R';
  if Attr and faHidden <> 0 then
    Result := Result + 'H';
  if Attr and faSysFile <> 0 then
    Result := Result + 'S';
  if Attr and faArchive <> 0 then
    Result := Result + 'A';
end;

function StatusKindStr(Status: TSvnWCStatusKind): string;
begin
  case Status of
    svnWcStatusNone:
      Result := SWcStatusNone;
    svnWcStatusUnversioned:
      Result := SWcStatusUnversioned;
    svnWcStatusNormal:
      Result := SWcStatusNormal;
    svnWcStatusAdded:
      Result := SWcStatusAdded;
    svnWcStatusMissing:
      Result := SWcStatusMissing;
    svnWcStatusDeleted:
      Result := SWcStatusDeleted;
    svnWcStatusReplaced:
      Result := SWcStatusReplaced;
    svnWcStatusModified:
      Result := SWcStatusModified;
    svnWcStatusMerged:
      Result := SWcStatusMerged;
    svnWcStatusConflicted:
      Result := SWcStatusConflicted;
    svnWcStatusIgnored:
      Result := SWcStatusIgnored;
    svnWcStatusObstructed:
      Result := SWcStatusObstructed;
    svnWcStatusExternal:
      Result := SWcStatusExternal;
    svnWcStatusIncomplete:
      Result := SWcStatusIncomplete;
  end;
end;

function SvnContextCancel(cancel_baton: Pointer): PSvnError; cdecl;
begin
  if TSvnClient(cancel_baton).DoCancel then
    Result := svn_error_create(SVN_ERR_CANCELLED, nil, PAnsiChar(UTF8Encode(sCancelledByUser)))
  else
    Result := nil;
end;

function SvnContextLogMessage(out log_msg, tmp_file: PAnsiChar; commit_items: PAprArrayHeader; baton: Pointer;
  pool: PAprPool): PSvnError; cdecl;
begin
  Result := nil;
  log_msg := apr_pstrdup(pool, PAnsiChar(UTF8Encode(TSvnClient(baton).FCommitLogMessage)));
end;

procedure SvnContextNotify(baton: Pointer; path: PAnsiChar; action: TSvnWcNotifyAction; kind: TSvnNodeKind;
  mime_type: PAnsiChar; content_state, prop_state: TSvnWCNotifyState; revision: TSvnRevNum); cdecl;
begin
  TSvnClient(baton).DoNotify(UTF8ToString(path), UTF8ToString(mime_type), action, kind, content_state, prop_state, revision);
end;

procedure SvnContextProgress(progress, total: TAprOff; baton: Pointer; pool: PAprPool); cdecl;
begin
{$IFDEF SvnDiag}
  if total = 0 then
    OutputDebugString(PChar(Format('SvnContextProgress(%d, %d)', [progress, total])))
  else
    OutputDebugString(PChar(Format('SvnContextProgress(%d, %d) %.2f%%', [progress, total, 100 * progress / total])));
{$ENDIF}
  TSvnClient(baton).DoProgress(progress, total);
end;

function SvnExcludeTrailingPathDelimiter(const S: string): string;

var
  L: Integer;

begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if IsDelimiter(SvnPathDelim, Result, L) then
    SetLength(Result, L - 1);
end;

function SvnExtractFileDrive(const SvnFileName: string): string;
var
  I, J: Integer;
begin
  if (Length(SvnFileName) >= 2) and (SvnFileName[2] = DriveDelim) then
    Result := Copy(SvnFileName, 1, 2)
  else if (Length(SvnFileName) >= 2) and (SvnFileName[1] = SvnPathDelim) and
    (SvnFileName[2] = SvnPathDelim) then
  begin
    J := 0;
    I := 3;
    while (I < Length(SvnFileName)) and (J < 2) do
    begin
      if SvnFileName[I] = SvnPathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if SvnFileName[I] = SvnPathDelim then Dec(I);
    Result := Copy(SvnFileName, 1, I);
  end else Result := '';
end;

function SvnExtractFileName(const SvnFileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(DriveDelim + SvnPathDelim, SvnFileName);
  Result := Copy(SvnFileName, I + 1, MaxInt);
end;

function SvnExtractFilePath(const SvnFileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(SvnPathDelim + DriveDelim, SvnFileName);
  Result := Copy(SvnFileName, 1, I);
end;

function SvnIncludeTrailingPathDelimiter(const S: string): string;
var
  L: Integer;
begin
  Result := S;
  if Result = '' then
    Exit;

  L := Length(Result);
  if not IsDelimiter(SvnPathDelim, Result, L) then
    Result := Result + SvnPathDelim;
end;

function UserNamePrompt(out cred: PSvnAuthCredUsername; baton: Pointer; realm: PAnsiChar; may_save: TSvnBoolean;
  pool: PAprPool): PSvnError; cdecl;
var
  SRealm, SUserName: string;
  Save: Boolean;
begin
  Result := nil;
  cred := nil;
  if Assigned(realm) then
    SetString(SRealm, realm, StrLen(realm))
  else
    SRealm := '';
  SUserName := '';
  Save := may_save;

  if not TSvnClient(baton).DoUserNamePrompt(SRealm, SUserName, Save) then // not cancelled
  begin
    cred := apr_pcalloc(pool, SizeOf(TSvnAuthCredUserName));
    cred^.username := apr_pstrdup(pool, PAnsiChar(UTF8Encode(SUserName)));
    cred^.may_save := Save;
  end;
end;

procedure WCStatus(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;
begin
  if Assigned(status) then
    TSvnItem(baton).DoWCStatus(path, status^);
end;

procedure WCStatus2(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;
begin
  if Assigned(status) then
    TSvnClient(baton).DoWCStatus(path, status^);
end;

procedure WCStatus3(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;
begin
  if Assigned(status) then
    TSvnItem(baton).LoadStatus(status^);
end;

procedure WCStatus4(baton: Pointer; path: PAnsiChar; status: PSvnWCStatus2); cdecl;
var
  Entry: PStatusEntry;
begin
  Entry := PStatusEntry(baton);
  Entry.Valid := False;
  if Assigned(status) and Assigned(status.entry) then
  begin
    Entry.Name := UTF8ToString(status.entry.name);
    Entry.Revision := status.entry.revision;
    Entry.URL := UTF8ToString(status.entry.url);
    Entry.Repos := UTF8ToString(status.entry.repos);
    Entry.Uuid := UTF8ToString(status.entry.uuid);
    Entry.Kind := status.entry.kind;
    Entry.Schedule := status.entry.schedule;
    Entry.Copied := status.entry.copied;
    Entry.Deleted := status.entry.deleted;
    Entry.Absent := status.entry.absent;
    Entry.Incomplete := status.entry.incomplete;
    Entry.Copyfrom_url := UTF8ToString(status.entry.copyfrom_url);
    Entry.Copyfrom_rev := status.entry.copyfrom_rev;
    Entry.Conflict_old := UTF8ToString(status.entry.conflict_old);
    Entry.Conflict_new := UTF8ToString(status.entry.conflict_new);
    Entry.Conflict_wrk := UTF8ToString(status.entry.conflict_wrk);
    Entry.Prejfile := UTF8ToString(status.entry.prejfile);
    Entry.Text_time := status.entry.text_time;
    Entry.Prop_time := status.entry.prop_time;
    Entry.Checksum := UTF8ToString(status.entry.checksum);
    Entry.Cmt_rev := status.entry.cmt_rev;
    Entry.Cmt_date := status.entry.cmt_date;
    Entry.Cmt_author := UTF8ToString(status.entry.cmt_author);
    Entry.Lock_token := UTF8ToString(status.entry.lock_token);
    Entry.Lock_owner := UTF8ToString(status.entry.lock_owner);
    Entry.Lock_comment := UTF8ToString(status.entry.lock_comment);
    Entry.Lock_creation_date := status.entry.lock_creation_date;
    Entry.Has_props := status.entry.has_props;
    Entry.Has_prop_mods := status.entry.has_prop_mods;
    Entry.Cachable_props := UTF8ToString(status.entry.cachable_props);
    Entry.Present_props := UTF8ToString(status.entry.present_props);
    Entry.Changelist := UTF8ToString(status.entry.changelist);
    Entry.Working_size := status.entry.working_size;
    Entry.Keep_local := status.entry.keep_local;
    Entry.Depth := status.entry.depth;
    Entry.Valid := True;
  end;
end;

function DummyInfoReceiver(baton: Pointer; path: PAnsiChar; const info: TSvnInfo; pool: PAprPool): PSvnError; cdecl;
begin
  Result := nil;
end;

type
  TBlameThread = class(TThread)
  private
    FItem: TSvnHistoryItem;
  protected
    procedure Execute; override;
  public
    constructor Create(AItem: TSvnHistoryItem; AOnTerminate: TNotifyEvent);
  end;

{ TBlameThread protected }

procedure TBlameThread.Execute;
var
  SvnClient: TSvnClient;
begin
  NameThreadForDebugging('DelphiSVN Blame Updater');
  SvnClient := TSvnClient.Create;
  try
    SvnClient.Initialize('', FItem.Owner.SvnClient.Ctx^.auth_baton);
    SvnClient.OnLoginPrompt := FItem.Owner.SvnClient.OnLoginPrompt;
    SvnClient.OnUserNamePrompt := FItem.Owner.SvnClient.OnUserNamePrompt;
    SvnClient.OnSSLServerTrustPrompt := FItem.Owner.SvnClient.OnSSLServerTrustPrompt;
    SvnClient.OnSSLClientPasswordPrompt := FItem.Owner.SvnClient.OnSSLClientPasswordPrompt;
    SvnClient.OnSSLClientPasswordPrompt := FItem.Owner.SvnClient.OnSSLClientPasswordPrompt;
    SvnClient.BlameOptions.Assign(FItem.Owner.SvnClient.BlameOptions);
    FItem.ReloadBlame(SvnClient, FItem.Owner.PathName, FItem.Owner.BaseRevision);
  finally
    SvnClient.Free;
  end;
end;

{ TBlameThread public }

constructor TBlameThread.Create(AItem: TSvnHistoryItem; AOnTerminate: TNotifyEvent);
begin
  FItem := AItem;
  OnTerminate := AOnTerminate;
  FreeOnTerminate := True;
  inherited Create(False);
end;

{ TSvnHistoryItem private }

procedure TSvnHistoryItem.BlameCallback(Sender: TObject; LineNo: Int64; Revision: TSvnRevNum; const Author: string;
  Time: TDateTime; const Line: string; var Cancel: Boolean);
var
  Item: TSvnBlameItem;
  OSTime: Integer;
begin
  Cancel := FCancelBlame or (Assigned(FBlameThread) and TBlameThread(FBlameThread).Terminated);
  if Cancel then
    Exit;

  Item := TSvnBlameItem.Create;
  try
    Item.FLineNo := LineNo;
    Item.FRevision := Revision;
    Item.FAuthor := Author;
    Item.FTime := Time;
    Item.FLine := Line;
    if Revision > FMaxRevision then
      FMaxRevision := Revision;
    if Revision < FMinRevision then
      FMinRevision := Revision;
    OSTime := DateTimeToFileDate(Time);
    if OSTime > FMaxTime then
      FMaxTime := OSTime;
    if OSTime < FMinTime then
      FMinTime := OSTime;
    FBlame.Add(Item);
  except
    Item.Free;
    raise;
  end;
end;

procedure TSvnHistoryItem.BlameThreadTerminate(Sender: TObject);
begin
  try
    if Assigned(TThread(Sender).FatalException) then
    begin
      ClearBlame;
      if TThread(Sender).FatalException is Exception then
        FBlameError := Exception(TThread(Sender).FatalException).Message;
    end;

    if Assigned(FBlameNotify) and not FCancelBlame and not FDestroying then
      FBlameNotify.AnnotationComplete(TSvnClientAnnotationLineProvider.Create(Self));
  finally
    FBlameThread := nil;
    FBlameNotify := nil;
    FCancelBlame := False;
  end;
end;

procedure TSvnHistoryItem.ClearBlame;
var
  I: Integer;
begin
  if Assigned(FBlame) then
  begin
    for I := 0 to FBlame.Count - 1 do
      TObject(FBlame[I]).Free;
    FreeAndNil(FBlame);
  end;
end;

procedure TSvnHistoryItem.ReloadBlame;
begin
  ReloadBlame(FOwner.SvnClient, FOwner.PathName, FOwner.BaseRevision);
end;

procedure TSvnHistoryItem.ReloadBlame(ASvnClient: TSvnClient; const APathName: string; ABaseRevision: Integer);
begin
  ClearBlame;
  FBlameError := '';

  FBlame := TList.Create;
  try
    ASvnClient.Blame(APathName, BlameCallback, 1, FRevision, ABaseRevision);
  except
    ClearBlame;
    raise;
  end;
end;

{ TSvnHistoryItem public }

constructor TSvnHistoryItem.Create;
begin
  inherited Create;
  FBlame := nil;
  FBlameThread := nil;
  FBlameNotify := nil;
  FCancelBlame := False;
  FMinRevision := MaxInt;
  FMaxRevision := 0;
  FMinTime := MaxInt;
  FMaxTime := 0;
end;

destructor TSvnHistoryItem.Destroy;
begin
  FDestroying := True;
  CancelBlame;
  FBlameThread := nil;
  ClearBlame;
  inherited Destroy;
end;

function TSvnHistoryItem.GetBlameCount: Integer;
begin
  Result := 0;
  if IsLoadingBlame then
    Exit;

  if not Assigned(FBlame) then
    ReloadBlame;
  Result := FBlame.Count;
end;

function TSvnHistoryItem.GetBlameItems(Index: Integer): TSvnBlameItem;
begin
  Result := nil;
  if IsLoadingBlame then
    Exit;

  if not Assigned(FBlame) then
    ReloadBlame;
  Result := FBlame[Index - 1];
end;

function TSvnHistoryItem.GetChangeFiles: TStringList;
begin
  Result := FChangeFiles;
end;

procedure TSvnHistoryItem.CancelBlame;
begin
  FCancelBlame := True;
end;

function TSvnHistoryItem.GetFile: TBytes;
var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;

begin
  SetLength(Result, 0);

  if Length(FFile) = 0 then
  begin
    AprCheck(apr_pool_create_ex(SubPool, FOwner.SvnClient.Pool, nil, FOwner.SvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionNumber;
      Revision.Value.number := FRevision;
      Buffer := svn_stringbuf_create('', SubPool);
      Stream := svn_stream_from_stringbuf(Buffer, SubPool);
      FOwner.FSvnClient.FCancelled := False;
      SvnCheck(svn_client_cat2(Stream, PAnsiChar(UTF8Encode(FOwner.SvnPathName)), @PegRevision, @Revision, FOwner.SvnClient.Ctx,
        SubPool));
      SetLength(FFile, Buffer.len);
      Move(Buffer.data^, FFile[0],  Buffer.len);
    finally
      apr_pool_destroy(SubPool);
    end;
  end;

  Result := FFile;
end;

function TSvnHistoryItem.GetHintString: string;
begin
  Result := SAuthor + Author + sLineBreak;
  Result := Result + STime + DateTimeToStr(Time) + sLineBreak;
  Result := Result + SComment + LogMessage;
end;

function TSvnHistoryItem.HasBlameLoaded: Boolean;
begin
  Result := Assigned(FBlame) and not IsLoadingBlame;
end;

function TSvnHistoryItem.IsLoadingBlame: Boolean;
begin
  Result := Assigned(FBlameThread);
end;

procedure TSvnHistoryItem.StartLoadingBlame(ANotify: IAnnotationCompletion = nil);
begin
  if IsLoadingBlame then
    Exit;
  FCancelBlame := False;
  FBlameNotify := ANotify;
  FBlameError := '';
  FBlameThread := TBlameThread.Create(Self, BlameThreadTerminate);
end;

{ TSvnItem private }

procedure TSvnItem.ClearHistory;
var
  I: Integer;
begin
  if Assigned(FHistory) then
  begin
    for I := 0 to FHistory.Count - 1 do
      TObject(FHistory[I]).Free;
    FreeAndNil(FHistory);
  end;
end;

procedure TSvnItem.ClearItems;
var
  I: Integer;
begin
  if Assigned(FItems) then
  begin
    for I := FItems.Count - 1 downto 0 do
      TObject(FItems[I]).Free;
    FreeAndNil(FItems);
  end;
end;

function TSvnItem.GetCount: Integer;
begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems.Count;
end;

function TSvnItem.GetExternal: Boolean;
var
  Externals: TStringList;
  I: Integer;
  S: string;
begin
  Result := False;

  if Assigned(Parent) then
  begin
    Externals := TStringList.Create;
    try
      Externals.Delimiter := Parent.PropValDelimiter;
      {$IFDEF COMPILER_10_UP}
      Externals.StrictDelimiter := True;
      {$ENDIF}
      Externals.DelimitedText := Parent.PropValues['svn:externals'];
      S := ExtractFileName(FPathName) + ' ';
      for I := 0 to Externals.Count - 1 do
        if StrLIComp(PChar(Externals[I]), PChar(S), Length(S)) = 0 then
        begin
          Result := True;
          Break;
        end;
    finally
      Externals.Free;
    end;
  end;
end;

function TSvnItem.GetFileAttr: Cardinal;
begin
  if FFileAttr = 0 then
    FFileAttr := GetFileAttributes(PChar(FPathName));
  Result := FFileAttr;
end;

function TSvnItem.GetHintStrings(Index: Integer): string;
begin
  Result := GetHistoryItems(Index).HintString;
end;

function TSvnItem.GetHistoryCount: Integer;
begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory.Count;
end;

function TSvnItem.GetHistoryItems(Index: Integer): TSvnHistoryItem;
begin
  if not Assigned(FHistory) then
    ReloadHistory;
  Result := FHistory[Index];
end;

function TSvnItem.GetIsDirectory: Boolean;
begin
  case FKind of
    svnNodeNone:
      Result := GetFileAttr and FILE_ATTRIBUTE_DIRECTORY <> 0;
    svnNodeDir:
      Result := True;
    else
      Result := False;
  end;
end;

function TSvnItem.GetItems(Index: Integer): TSvnItem;
begin
  if not Assigned(FItems) then
    Reload;
  Result := FItems[Index];
end;

function TSvnItem.GetPropCount: Integer;
begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Count;
end;

function TSvnItem.GetPropNames(Index: Integer): string;
begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Names[Index];
end;

function TSvnItem.GetPropValueFromIndex(Index: Integer): string;
begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.ValueFromIndex[Index];
end;

function TSvnItem.GetPropValues(const Name: string): string;
begin
  if not Assigned(FProps) then
    ReloadProps;
  Result := FProps.Values[Name];
end;

procedure TSvnItem.HistoryThreadTerminated;
begin
  FHistoryUpdateThread := nil;
  if Assigned(FNextAsyncUpdate) then
  begin
    FAsyncUpdate := FNextAsyncUpdate;
    FNextAsyncUpdate := nil;
    AsyncReloadHistory;
  end
  else
    FAsyncUpdate := nil;
end;

procedure TSvnItem.LoadStatus(const Status: TSvnWcStatus2);
begin
  if Assigned(Status.entry) then
  begin
    FBaseRevision := Status.entry^.revision;
    FURL := UTF8ToString(Status.entry^.url);
    FRepository := UTF8ToString(Status.entry^.repos);
    FUUID := string(Status.entry^.uuid);
    FKind := Status.entry^.kind;
    FSchedule := Status.entry^.schedule;
    FCopied := Status.entry^.copied;
    FDeleted := Status.entry^.deleted;
    FAbsent := Status.entry^.absent;
    FIncomplete := Status.entry^.incomplete;
    FCopiedFromURL := UTF8ToString(Status.entry^.copyfrom_url);
    FCopiedFromRevision := Status.entry^.copyfrom_rev;
    FConflictOldFile := UTF8ToString(Status.entry^.conflict_old);
    FConflictNewFile := UTF8ToString(Status.entry^.conflict_new);
    FConflictWorkingFile := UTF8ToString(Status.entry^.conflict_wrk);
    FPropRejectFile := UTF8ToString(Status.entry^.prejfile);
    FTextTime := AprTimeToDateTime(Status.entry^.text_time);
    FPropTime := AprTimeToDateTime(Status.entry^.prop_time);
    FCheckSum := string(Status.entry^.checksum);
    FCommittedRevision := Status.entry^.cmt_rev;
    FCommitAuthor := UTF8ToString(Status.entry^.cmt_author);
    FCommitTime := AprTimeToDateTime(Status.entry^.cmt_date);
    FLockToken := string(Status.entry^.lock_token);
    FLockOwner := UTF8ToString(Status.entry^.lock_owner);
    FLockComment := UTF8ToString(Status.entry^.lock_comment);
    FLockTime := AprTimeToDateTime(Status.entry^.lock_creation_date);
    FChangeList := UTF8ToString(Status.entry^.changelist);
  end
  else
  begin
    FBaseRevision := -1;
    FURL := '';
    FRepository := '';
    FUUID := '';
    FKind := svnNodeNone;
    FSchedule := svnWcScheduleNormal;
    FCopied := False;
    FDeleted := False;
    FAbsent := False;
    FIncomplete := False;
    FCopiedFromURL := '';
    FCopiedFromRevision := -1;
    FConflictOldFile := '';
    FConflictNewFile := '';
    FConflictWorkingFile := '';
    FPropRejectFile := '';
    FTextTime := 0;
    FPropTime := 0;
    FCheckSum := '';
    FCommittedRevision := -1;
    FCommitAuthor := '';
    FCommitTime := 0;
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockTime := 0;
    FChangeList := '';
  end;
  FTextStatus := Status.text_status;
  FPropStatus := Status.prop_status;
  FLocked := Status.locked;
  FCopied := Status.copied;
  FSwitched := Status.switched;
  FRemoteTextStatus := Status.repos_text_status;
  FRemotePropStatus := Status.repos_prop_status;
  if Assigned(Status.repos_lock) then
  begin
    FLockPath := UTF8ToString(Status.repos_lock^.path);
    FLockToken := UTF8ToString(Status.repos_lock^.token);
    FLockOwner := UTF8ToString(Status.repos_lock^.owner);
    FLockComment := UTF8ToString(Status.repos_lock^.comment);
    FLockDAVComment := Status.repos_lock^.is_dav_comment;
    FLockTime := AprTimeToDateTime(Status.repos_lock^.creation_date);
    FLockExpirationTime := AprTimeToDateTime(Status.repos_lock^.expiration_date);
  end
  else
  begin
    FLockPath := '';
    FLockToken := '';
    FLockOwner := '';
    FLockComment := '';
    FLockDAVComment := False;
    FLockTime := 0;
    FLockExpirationTime := 0;
  end;
  FURL := UTF8ToString(Status.url);

  FLastCommitRevision := Status.ood_last_cmt_rev;
  FLastCommitAuthor := UTF8ToString(Status.ood_last_cmt_author);
  FLastCommitTime := AprTimeToDateTime(Status.ood_last_cmt_date);

  if IsDirectory and (FTextStatus = svnWcStatusUnversioned) and FSvnClient.RecurseUnversioned then
    LoadUnversionedPaths;
end;

procedure TSvnItem.LoadUnversionedPaths;
var
  R: Integer;
  F: TSearchRec;
  Status: TSvnWcStatus2;
  Item: TSvnItem;
  Error: PSvnError;
begin
  R := FindFirst(IncludeTrailingPathDelimiter(FPathName) + '*.*', faAnyFile, F);
  if R <> 0 then
    Exit;

  try
    while R = 0 do
    begin
      if (F.Name <> '.') and (F.Name <> '..') and not FSvnClient.MatchGlobalIgnores(F.Name) then
      begin
        if (F.Attr and faDirectory <> 0) and
          FSvnClient.IsPathVersioned(IncludeTrailingPathDelimiter(FPathName) + F.Name) then
        begin
{$IFDEF SvnDiag}
          OutputDebugString(PChar(Format('versioned subdirectory ''%s'' in unversioned directory ''%s''',
            [F.Name, FPathName])));
{$ENDIF}
        end
        else
        begin
          FillChar(Status, SizeOf(TSvnWcStatus2), 0);
          Status.entry := nil;
          Status.text_status := svnWcStatusUnversioned;
          Item := TSvnItem.Create(FSvnClient, Self,
            FSvnClient.NativePathToSvnPath(IncludeTrailingPathDelimiter(FPathName) + F.Name), Status);

          if Assigned(FSvnClient.FStatusCallback) then
            FSvnClient.FStatusCallback(FSvnClient, Item, FSvnClient.FCancelled);
          Error := SvnContextCancel(FSvnClient);
          if Assigned(Error) then
            RaiseSvnError(Error);
        end;
      end;

      R := FindNext(F);
    end;
  finally
    FindClose(F);
  end;
end;

procedure TSvnItem.PauseAsynchronousUpdate;
begin
  FHistoryUpdateThread.Suspend;
end;

procedure TSvnItem.ReloadHistory;
var
  SubPool: PAprPool;
  StartRevision, EndRevision: TSvnOptRevision;
  Targets: PAprArrayHeader;
  Error: PSvnError;
begin
  ClearHistory;

  FHistory := TList.Create;
  try
    if FTextStatus in [svnWcStatusNone, svnWcStatusUnversioned] then
      Exit;

    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(StartRevision, SizeOf(TSvnOptRevision), 0);
      StartRevision.Kind := svnOptRevisionHead;
      FillChar(EndRevision, SizeOf(TSvnOptRevision), 0);
      EndRevision.Kind := svnOptRevisionNumber;
      EndRevision.Value.number := 0;

      Targets := FSvnClient.PathNamesToAprArray([FSvnPathName], SubPool);
      FSvnClient.FCancelled := False;
      Error := svn_client_log2(Targets, @StartRevision, @EndRevision, FLogLimit, False, False, LogMessage, Self, FSvnClient.Ctx,
        SubPool);
      if Assigned(Error) then
      begin
        if Error^.apr_err = APR_OS_START_SYSERR + WSAHOST_NOT_FOUND then
          svn_error_clear(Error)
        else
          RaiseSvnError(Error);
      end;
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    ClearHistory;
    raise;
  end;
end;

procedure TSvnItem.ReloadProps;
var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  TruePath: PAnsiChar;
  Props: PAprArrayHeader;
begin
  FreeAndNil(FProps);

  FProps := TStringList.Create;
  try
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
      PegRevision.Kind := svnOptRevisionUnspecified;
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionUnspecified;
      AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(UTF8Encode(FPathName)), APR_FILEPATH_TRUENAME, SubPool));
      FSvnClient.FCancelled := False;
      SvnCheck(svn_client_proplist2(Props, TruePath, @PegRevision, @Revision, False, FSvnClient.Ctx,
        SubPool));

      FSvnClient.GetProps(Props, FProps, SubPool, FPropValDelimiter);
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    FreeAndNil(FProps);
    raise;
  end;
end;

procedure TSvnItem.ScheduleAsyncReload(NextAsyncUpdate: IAsyncUpdate);
begin
  FNextAsyncUpdate := NextAsyncUpdate;
end;

procedure TSvnItem.SetPropValues(const Name, Value: string);
var
  SubPool: PAprPool;
  TruePath: PAnsiChar;
  SValue: string;
  SvnValue: PSvnString;
begin
  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(UTF8Encode(FPathName)), APR_FILEPATH_TRUENAME, SubPool));
    SValue := Value;
    if Pos(';', SValue) <> 0 then
    begin
      if SValue[Length(SValue)] <> ';' then
        SValue := SValue + ';';
      SValue := StringReplace(SValue, ';', SvnLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;

    SvnValue := svn_string_create(PAnsiChar(UTF8Encode(Value)), SubPool);
    if svn_prop_needs_translation(PAnsiChar(UTF8Encode(Name))) then
      SvnCheck(svn_subst_translate_string(SvnValue, SvnValue, nil, SubPool));

    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_propset2(PAnsiChar(UTF8Encode(Name)), SvnValue, TruePath, False, False, FSvnClient.Ctx, SubPool));
  finally
    apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnItem.SortItems(Recurse: Boolean);
var
  I: Integer;
begin
  if not Assigned(FItems) then
    Exit;

  for I := 0 to FItems.Count - 1 do
    if TSvnItem(FItems[I]).Kind = svnNodeDir then
      TSvnItem(FItems[I]).SortItems(Recurse);
  FItems.Sort(CompareNativePathNames);
end;

procedure TSvnItem.TerminateAsynchronousUpdate(WaitForTerminate: Boolean);
begin
  if Assigned(FHistoryUpdateThread) then
  begin
    SvnClient.FCancelled := True;
    if WaitForTerminate then
    begin
      FHistoryUpdateThread.FreeOnTerminate := False;
      FreeAndNil(FHistoryUpdateThread);
    end
    else
      FHistoryUpdateThread.Terminate;
  end;
end;

{ TSvnItem protected }

procedure TSvnItem.DoDestroyNotifications;
var
  I: Integer;
begin
  if Assigned(FDestroyNotifications) then
    for I := 0 to FDestroyNotifications.Count - 1 do
      TNotifyEvent(PMethod(FDestroyNotifications[I])^)(Self);
end;

//----------------------------------------------------------------------------------------------------------------------
// svn recursive status lists items in the following order:
// 1.  unversioned items within current directory, if any
// 2.  current directory itself
// 3.  versioned items within current directory, however:
//     if the item is a directory, descend and continue with step 1 (unversioned items first).
//
// for example:
//   V:/unversioned1.txt
//   V:/unversioned2.txt
//   V:
//   V:/Subdir/unversioned1.txt
//   V:/Subdir/unversioned2.txt
//   V:/Subdir
//   V:/Subdir/versioned1.txt
//   V:/Subdir/versioned2.txt
//   V:/versioned1.txt
//   V:/versioned2.txt

procedure TSvnItem.DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2);
var
  Parent, Child: TSvnItem;
  ParentPath, ChildsParentPath: string;
  I: Integer;

  procedure AddExternals(Parent: TSvnItem);
  var
    I: Integer;
    Child: TSvnItem;
  begin
    for I := FReloadExternals.Count - 1 downto 0 do
    begin
      Child := FReloadExternals[I];
      FReloadExternals.Delete(I);
      Child.FParent := Parent;
      Parent.Add(Child);
    end;
  end;

  procedure AddUnversionedItems(Parent: TSvnItem);
  var
    I: Integer;
    Child: TSvnItem;
  begin
    for I := FReloadUnversioned.Count - 1 downto 0 do
    begin
      Child := FReloadUnversioned[I];
      FReloadUnversioned.Delete(I);
      Child.FParent := Parent;
      Parent.Add(Child);
    end;
  end;

begin
  case Status.text_status of
    svnWcStatusUnversioned:
      FReloadUnversioned.Add(TSvnItem.Create(FSvnClient, nil, UTF8ToString(Path), Status));
    svnWcStatusExternal:
      begin
        Child := TSvnItem.Create(FSvnClient, nil, UTF8ToString(Path), Status);
        FReloadExternals.Add(Child);
        if FReloadRecursive then
          FReloadGlobalExternals.Add(Child);
      end;
    else
    begin
      if FReloadRecursive then
      begin
        if FReloadStack.Count > 0 then
          Parent := FReloadStack.Peek
        else
          Parent := nil;
      end
      else
        Parent := Self;

      if Assigned(Parent) then
      begin
        if StrIComp(Path, PAnsiChar(UTF8Encode(Parent.SvnPathName))) = 0 then
        begin
          LoadStatus(Status);
          if FKind = svnNodeDir then
          begin
            AddUnversionedItems(Self);
            AddExternals(Self);
          end;
          Exit;
        end
        else if FReloadRecursive then
        begin
          ChildsParentPath := SvnExtractFilePath(UTF8ToString(Path));
          ParentPath := SvnIncludeTrailingPathDelimiter(Parent.SvnPathName);
          while not AnsiSameText(ChildsParentPath, ParentPath) do
          begin
            FReloadStack.Pop;
            if FReloadStack.Count > 0 then
            begin
              Parent := FReloadStack.Peek;
              ParentPath := SvnIncludeTrailingPathDelimiter(Parent.SvnPathName);
            end
            else
            begin
              Parent := nil;
              Break;
            end;
          end;
        end;
      end;

      if Assigned(Parent) then
      begin
        Child := TSvnItem.Create(FSvnClient, Parent, UTF8ToString(Path), Status);
        if Child.Kind = svnNodeDir then
        begin
          AddUnversionedItems(Child);
          AddExternals(Child);
          FReloadStack.Push(Child);
        end;
      end
      else if FReloadRecursive then // not found in current stack, try externals
      begin
        for I := 0 to FReloadGlobalExternals.Count - 1 do
          if AnsiSameText(UTF8ToString(Path), TSvnItem(FReloadGlobalExternals[I]).SvnPathName) then
          begin
            Parent := FReloadGlobalExternals[I];
            FReloadGlobalExternals.Delete(I);
            FReloadStack.Push(Parent);
            Break;
          end;
        if Assigned(Parent) then
          Parent.LoadStatus(Status);
      end;
    end;
  end;
end;

{ TSvnItem public }

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const APathName: string;
  Recurse: Boolean = False; Update: Boolean = False; DoReload:Boolean = False);
var
  SubPool: PAprPool;
begin
  inherited Create;
  FSvnClient := ASvnClient;
  FParent := AParent;
  if Assigned(FParent) then
    FParent.Add(Self);
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FPathName := APathName;
  FFileAttr := 0;
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  FLogLimit := 0;
  FLogFirstRev := -1;
  FLogLastRev := -1;
  if DoReload then
    Reload(Recurse, Update)
  else
  begin
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FSvnPathName := FSvnClient.NativePathToSvnPath(FPathName, SubPool);
    finally
      apr_pool_destroy(SubPool);
    end;
  end;
end;

constructor TSvnItem.Create(ASvnClient: TSvnClient; AParent: TSvnItem; const ASvnPathName: string;
  const Status: TSvnWcStatus2);
begin
  inherited Create;
  FSvnClient := ASvnClient;
  FSvnPathName := ASvnPathName;
  FParent := AParent;
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FFileAttr := 0;
  FPathName := FSvnClient.SvnPathToNativePath(FSvnPathName);
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  LoadStatus(Status);
  if Assigned(FParent) then
    FParent.Add(Self);
  FDestroyNotifications := nil;
end;

constructor TSvnItem.Create(ASvnClient: TSvnClient; const AURL: string);
var
  Status: TSvnWcStatus2;
begin
  inherited Create;
  FSvnClient := ASvnClient;
  FSvnPathName := '';
  FParent := nil;
  FItems := nil;
  FHistory := nil;
  FProps := nil;
  FFileAttr := 0;
  FPathName := '';
  FSmallImageIndex := -1;
  FLargeImageIndex := -1;
  FPropValDelimiter := ';';
  FLogLimit := 0;
  FLogFirstRev := -1;
  FLogLastRev := -1;
  Status.entry := nil;
  Status.repos_lock := nil;
  LoadStatus(Status);
  FURL := AURL;
  FDestroyNotifications := nil;
end;

destructor TSvnItem.Destroy;
var
  I: Integer;
begin
  if Assigned(FHistoryUpdateThread) then
  begin
    FHistoryUpdateThread.OnTerminate := nil;
    FHistoryUpdateThread.Free;
  end;
  DoDestroyNotifications;
  if Assigned(FDestroyNotifications) then
    for I := 0 to FDestroyNotifications.Count - 1 do
      Dispose(FDestroyNotifications[I]);
  FDestroyNotifications.Free;
  if Assigned(FParent) then
    FParent.Remove(Self);
  Clear;
  inherited Destroy;
end;

function TSvnItem.Add(Item: TSvnItem): Integer;
begin
  if not Assigned(FItems) then
    FItems := TList.Create;
  Result := FItems.Add(Item);
end;

procedure TSvnItem.AddDestroyNotification(Notification: TNotifyEvent);
var
  P: PMethod;
begin
  if not Assigned(FDestroyNotifications) then
    FDestroyNotifications := TList.Create;

  New(P);
  try
    P^ := TMethod(Notification);
    FDestroyNotifications.Add(P);
  except
    Dispose(P);
    raise;
  end;
end;

procedure TSvnItem.AsyncReloadHistory;
begin
  if FHistoryUpdateThread = nil then
  begin
    ClearHistory;
    FHistoryUpdateThread := THistoryUpdateThread.Create(Self, FAsyncUpdate);
  end
  else
  begin
    FHistoryUpdateThread.ResetUpdate;
    FHistoryUpdateThread.Update;
  end;
end;

procedure TSvnItem.AsyncUpdateUI(NewItemIndex: Integer);
begin
  if Assigned(FHistoryUpdateThread) and (not FHistoryUpdateThread.Terminated) then
  begin
    FHistoryUpdateThread.FNewItemIndex := NewItemIndex;
    FHistoryUpdateThread.Synchronize(FHistoryUpdateThread.Update);
  end;
end;

procedure TSvnItem.Clear;
begin
  FBaseRevision := -1;
  FURL := '';
  FRepository := '';
  FUUID := '';
  FKind := svnNodeNone;
  FSchedule := svnWcScheduleNormal;
  FCopied := False;
  FDeleted := False;
  FAbsent := False;
  FIncomplete := False;
  FCopiedFromURL := '';
  FCopiedFromRevision := -1;
  FConflictOldFile := '';
  FConflictNewFile := '';
  FConflictWorkingFile := '';
  FPropRejectFile := '';
  FTextTime := 0;
  FPropTime := 0;
  FCheckSum := '';
  FCommittedRevision := -1;
  FCommitAuthor := '';
  FCommitTime := 0;
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockTime := 0;
  FTextStatus := svnWcStatusNone;
  FPropStatus := svnWcStatusNone;
  FLocked := False;
  FCopied := False;
  FSwitched := False;
  FRemoteTextStatus := svnWcStatusNone;
  FRemotePropStatus := svnWcStatusNone;
  FLockPath := '';
  FLockToken := '';
  FLockOwner := '';
  FLockComment := '';
  FLockDAVComment := False;
  FLockTime := 0;
  FLockExpirationTime := 0;
  FURL := '';
  FLastCommitRevision := -1;
  FLastCommitAuthor := '';
  FLastCommitTime := 0;

  ClearItems;
  ClearHistory;
  FreeAndNil(FProps);
end;

function TSvnItem.GetBaseFile: TBytes;
var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;
begin
  SetLength(Result, 0);
  if FKind <> svnNodeFile then
    Exit;

  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionBase;
    Buffer := svn_stringbuf_create('', SubPool);
    Stream := svn_stream_from_stringbuf(Buffer, SubPool);
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_cat2(Stream, PAnsiChar(UTF8Encode(FSvnPathName)), @PegRevision, @Revision, FSvnClient.Ctx, SubPool));
    SetLength(Result, Buffer.len);
    move(Buffer.data^, Result[0], Buffer.len);
  finally
    apr_pool_destroy(SubPool);
  end;
end;

function TSvnItem.GetCommittedFile: TBytes;
var
  SubPool: PAprPool;
  PegRevision, Revision: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;
begin
  SetLength(Result, 0);
  if FKind <> svnNodeFile then
    Exit;

  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionCommitted;
    Buffer := svn_stringbuf_create('', SubPool);
    Stream := svn_stream_from_stringbuf(Buffer, SubPool);
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_cat2(Stream, PAnsiChar(UTF8Encode(FSvnPathName)), @PegRevision, @Revision, FSvnClient.Ctx, SubPool));
    SetLength(Result, Buffer.len);
    move(Buffer.data^, Result[0], Buffer.len);
  finally
    apr_pool_destroy(SubPool);
  end;
end;

function TSvnItem.IndexOf(Item: TSvnItem): Integer;
begin
  if Assigned(FItems) then
    Result := FItems.IndexOf(Item)
  else
    Result := -1;
end;

function TSvnItem.IndexOf(const PathName: string; SvnPath: Boolean = False): Integer;
var
  I: Integer;
  SPathName1, SPathName2: string;
begin
  Result := -1;

  if not Assigned(FItems) then
    Exit;

  for I := 0 to FItems.Count - 1 do
  begin
    if SvnPath then
    begin
      SPathName1 := SvnExcludeTrailingPathDelimiter(PathName);
      SPathName2 := SvnExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).SvnPathName);
    end
    else
    begin
      SPathName1 := ExcludeTrailingPathDelimiter(PathName);
      SPathName2 := ExcludeTrailingPathDelimiter(TSvnItem(FItems[I]).PathName);
    end;

    if AnsiSameText(SPathName1, SPathName2) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TSvnItem.Reload(Recurse: Boolean = False; Update: Boolean = False);
var
  SubPool: PAprPool;
  Revision: TSvnOptRevision;
  I: Integer;
begin
  Clear;
  FItems := TList.Create;

  FReloadRecursive := Recurse;
  FReloadExternals := nil;
  FReloadGlobalExternals := nil;
  FReloadUnversioned := nil;
  FReloadStack := TStack.Create;
  try
    FReloadExternals := TList.Create;
    if FReloadRecursive then
      FReloadGlobalExternals := TList.Create;
    FReloadUnversioned := TList.Create;
    AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
    try
      FSvnPathName := FSvnClient.NativePathToSvnPath(FPathName, SubPool);
      FillChar(Revision, SizeOf(TSvnOptRevision), 0);
      Revision.Kind := svnOptRevisionHead;
      FSvnClient.FCancelled := False;
      if FReloadRecursive then
        FReloadStack.Push(Self);
      SvnCheck(svn_client_status2(nil, PAnsiChar(UTF8Encode(FSvnPathName)),
        @Revision, WCStatus, Self, Recurse, True, Update, False, False,
        FSvnClient.Ctx, SubPool));
    finally
      apr_pool_destroy(SubPool);
    end;

    if FKind = svnNodeDir then
      SortItems(Recurse);
  finally
    FreeAndNil(FReloadStack);
    for I := 0 to FReloadUnversioned.Count - 1 do
      TSvnItem(FReloadUnversioned[I]).Free;
    FreeAndNil(FReloadUnversioned);
    for I := 0 to FReloadExternals.Count - 1 do
      TSvnItem(FReloadExternals[I]).Free;
    FreeAndNil(FReloadExternals);
    if FReloadRecursive then
      for I := 0 to FReloadGlobalExternals.Count - 1 do
        TSvnItem(FReloadGlobalExternals[I]).Free;
    FreeAndNil(FReloadGlobalExternals);
  end;
end;

procedure TSvnItem.ReloadStatus;
var
  SubPool: PAprPool;
  Revision: TSvnOptRevision;
begin
  AprCheck(apr_pool_create_ex(SubPool, FSvnClient.Pool, nil, FSvnClient.Allocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FSvnClient.FCancelled := False;
    SvnCheck(svn_client_status2(nil, PAnsiChar(UTF8Encode(FSvnPathName)), @Revision, WCStatus3, Self, False, True, True, False, False,
      FSvnClient.Ctx, SubPool));
  finally
    apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnItem.Remove(Item: TSvnItem);
begin
  if Assigned(FItems) then
    FItems.Remove(Item);
end;

procedure TSvnItem.RemoveDestroyNotification(Notification: TNotifyEvent);
var
  P: PMethod;
  I, Index: Integer;
begin
  if Assigned(FDestroyNotifications) then
  begin
    P := nil;
    Index := -1;
    for I := 0 to FDestroyNotifications.Count - 1 do
     if (PMethod(FDestroyNotifications[I])^.Data = TMethod(Notification).Data) and
       (PMethod(FDestroyNotifications[I])^.Code = TMethod(Notification).Code) then
     begin
       P := FDestroyNotifications[I];
       Index := I;
       Break;
     end;

     if Index <> -1 then
     begin
       FDestroyNotifications.Delete(Index);
       Dispose(P);
     end;
  end;
end;

procedure TSvnItem.Resolved(Recurse: Boolean = False);
begin
  FSvnClient.Resolved(FSvnPathName, SvnWcConflictChooseMerged, Recurse);
  ReloadStatus;
end;

procedure TSvnItem.ResumeAsynchronousUpdate;
begin
  FHistoryUpdateThread.Resume;
end;

procedure TSvnListItem.AssignTo(Dest: TPersistent);

begin
  if Dest is TSvnListItem then
  begin
    TSvnListItem(Dest).AbsolutePath := AbsolutePath;
    TSvnListItem(Dest).CreatedRevision := CreatedRevision;
    TSvnListItem(Dest).HasProps := HasProps;
    TSvnListItem(Dest).Path := Path;
    TSvnListItem(Dest).Kind := Kind;
    TSvnListItem(Dest).LastAuthor := LastAuthor;
    TSvnListItem(Dest).LockComment := LockComment;
    TSvnListItem(Dest).LockDAVComment := LockDAVComment;
    TSvnListItem(Dest).Locked := Locked;
    TSvnListItem(Dest).LockExpirationTime := LockExpirationTime;
    TSvnListItem(Dest).LockOwner := LockOwner;
    TSvnListItem(Dest).LockPath := LockPath;
    TSvnListItem(Dest).LockTime := LockTime;
    TSvnListItem(Dest).LockToken := LockToken;
    TSvnListItem(Dest).Size := Size;
    TSvnListItem(Dest).Time := Time;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TSvnList.Create(ASvnClient: TSvnClient);
begin
  inherited Create;
  FItems := TObjectList.Create;
  FSvnClient := ASvnClient;
end;

destructor TSvnList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TSvnList.Clear;
begin
  FItems.Clear;
end;

function TSvnList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSvnList.GetItems(Index: Integer): TSvnListItem;
begin
  Result := TSvnListItem(FItems[Index]);
end;

procedure TSvnList.ListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock;
  AbsPath: string; var Cancel: Boolean);
var
  SvnListItem: TSvnListItem;
begin
  Cancel := False;
  FItems.Add(TSvnListItem.Create);
  SvnListItem := TSvnListItem(FItems.Last);
  SvnListItem.AbsolutePath := AbsPath;
  SvnListItem.Path := Path;
  SvnListItem.Kind := DirEntry.kind;
  SvnListItem.Size := DirEntry.size;
  SvnListItem.HasProps := DirEntry.has_props;
  SvnListItem.CreatedRevision := DirEntry.created_rev;
  SvnListItem.Time := AprTimeToDateTime(DirEntry.time);
  SvnListItem.LastAuthor := UTF8ToString(DirEntry.last_author);
  SvnListItem.Locked := Locked;
  SvnListItem.LockPath := UTF8ToString(LockData.path);
  SvnListItem.LockToken := string(LockData.token);
  SvnListItem.LockOwner := UTF8ToString(LockData.owner);
  SvnListItem.LockComment := UTF8ToString(LockData.comment);
  SvnListItem.LockDAVComment := LockData.is_dav_comment;
  SvnListItem.LockTime := AprTimeToDateTime(LockData.creation_date);
  SvnListItem.LockExpirationTime := AprTimeToDateTime(LockData.expiration_date);
end;


procedure TSvnList.LoadList(const APathName: string; ADepth: TSvnDepth; AFetchLocks: Boolean; ADirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1);
begin
  Clear;
  FPathName := APathName;
  FDepth := ADepth;
  FFetchLocks := AFetchLocks;
  FDirEntryFields := ADirEntryFields;
  FSvnClient.List(FPathName, FDepth, FFetchLocks, FDirEntryFields, ListCallback, Revision);
end;

{ TSvnMergeRevisionRange }

constructor TSvnMergeRevisionRange.Create(AStartRevision, AEndRevision: TSvnRevNum);
begin
  inherited Create;
  FStartRevision := AStartRevision;
  FEndRevision := AEndRevision;
end;

{ TSvnMergeRevisionList }

procedure TSvnMergeRevisionList.AddRevision(ARevision: TSvnRevNum);
begin
  AddRevisionRange(ARevision, ARevision);
end;

procedure TSvnMergeRevisionList.AddRevisionRange(AStartRevision, AEndRevision: TSvnRevNum);
begin
  FItems.Add(TSvnMergeRevisionRange.Create(AStartRevision, AEndRevision));
end;

procedure TSvnMergeRevisionList.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TSvnMergeRevisionList then
  begin
    TSvnMergeRevisionList(Dest).Clear;
    for I := 0 to Count - 1 do
      TSvnMergeRevisionList(Dest).AddRevisionRange(Items[I].StartRevision, Items[I].EndRevision);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TSvnMergeRevisionList.Clear;
begin
  FItems.Clear;
end;

constructor TSvnMergeRevisionList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TSvnMergeRevisionList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TSvnMergeRevisionList.GetAsString: string;

  function RevisionToStr(ARevision: TSvnRevNum): string;
  begin
    if ARevision = -1 then
      Result := 'HEAD'
    else
      Result := IntToStr(ARevision);
  end;

var
  I: Integer;
  S: string;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if Items[I].StartRevision = Items[I].EndRevision then
      S := RevisionToStr(Items[I].StartRevision)
    else
      S := Format('%s-%s', [RevisionToStr(Items[I].StartRevision), RevisionToStr(Items[I].EndRevision)]);
    if Result <> '' then
      Result := Result + ',' + S
    else
      Result := S;
  end;
end;

function TSvnMergeRevisionList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSvnMergeRevisionList.GetItems(AIndex: Integer): TSvnMergeRevisionRange;
begin
  Result := TSvnMergeRevisionRange(FItems[AIndex]);
end;

function TSvnMergeRevisionList.IsValidRangeStr(const AValue: string): Boolean;

  function IsValidStrToRevision(const AValue: string): Boolean;
  var
    Dummy: Integer;
  begin
    if AnsiSameText('HEAD', AValue) then
      Result := True
    else
      Result := TryStrToInt(AValue, Dummy);
  end;

var
  I, P: Integer;
  SL: TStringList;
begin
  Result := True;
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    SL.DelimitedText := AValue;
    for I := 0 to SL.Count - 1 do
    begin
      P := Pos('-', SL[I]);
      if P > 0 then
      begin
        if not IsValidStrToRevision(Trim(Copy(SL[I], 1, P - 1))) or
          not IsValidStrToRevision(Trim(Copy(SL[I], P + 1, MaxInt))) then
        begin
          Result := False;
          Break;
        end;
      end
      else
      if not IsValidStrToRevision(Trim(SL[I])) then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    SL.Free;
  end;
end;

procedure TSvnMergeRevisionList.PrepareForMerge(AHeadRevision: TSvnRevNum;
  AReverseMerge: Boolean);
var
  I, StartRev, EndRev: Integer;
begin
  if Count = 0 then
    AddRevisionRange(1, -1);
  for I := 0 to Count - 1 do
  begin
    if Items[I].StartRevision = -1 then
      Items[I].FStartRevision := AHeadRevision;
    if Items[I].EndRevision = -1 then
      Items[I].FEndRevision := AHeadRevision;
    if AReverseMerge then
    begin
      StartRev := Items[I].StartRevision;
      EndRev := Items[I].EndRevision;
      Items[I].FStartRevision := EndRev;
      Items[I].FEndRevision := StartRev - 1;
    end
    else
      Items[I].FStartRevision := Items[I].StartRevision - 1;
  end;
end;

procedure TSvnMergeRevisionList.SetAsString(const Value: string);

  function StrToRevision(const AValue: string): Integer;
  begin
    if AnsiSameText('HEAD', AValue) then
      Result := -1
    else
      Result := StrToInt(AValue);
  end;

var
  I, P, Rev, StartRev, EndRev: Integer;
  SL: TStringList;
begin
  Clear;
  SL := TStringList.Create;
  try
    SL.Delimiter := ',';
    SL.StrictDelimiter := True;
    SL.DelimitedText := Value;
    for I := 0 to SL.Count - 1 do
    begin
      P := Pos('-', SL[I]);
      if P > 0 then
      begin
        StartRev := StrToRevision(Trim(Copy(SL[I], 1, P - 1)));
        EndRev := StrToRevision(Trim(Copy(SL[I], P + 1, MaxInt)));
        AddRevisionRange(StartRev, EndRev);
      end
      else
      begin
        Rev := StrToRevision(Trim(SL[I]));
        AddRevision(Rev);
      end;
    end;
  finally
    SL.Free;
  end;
end;

function TSvnMergeRevisionList.ToAprArray(ASvnClient: TSvnClient; SubPool: PAprPool): PAprArrayHeader;
type
  PPSvnOptRevisionRange = ^PSvnOptRevisionRange;
var
  NewPool: Boolean;
  I: Integer;
  Range: PSvnOptRevisionRange;
begin
  Result := nil;

  if Count = 0 then
    Exit;

  if not ASvnClient.Initialized then
    ASvnClient.Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, ASvnClient.Pool, nil, ASvnClient.Allocator));
  try
    Result := apr_array_make(SubPool, Count, SizeOf(PSvnOptRevisionRange));
    for I := 0 to Count - 1 do
    begin
      Range := apr_pcalloc(SubPool, SizeOf(TSvnOptRevisionRange));
      Range^.rev_start.Kind := svnOptRevisionNumber;
      Range^.rev_start.Value.number := Items[I].StartRevision;
      Range^.rev_end.Kind := svnOptRevisionNumber;
      Range^.rev_end.Value.number := Items[I].EndRevision;
      PPSvnOptRevisionRange(apr_array_push(Result))^ := Range;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

{ TSvnConfigString }

constructor TSvnConfigString.Create;
begin
  inherited Create;
  FState := scisDefault;
end;

procedure TSvnConfigString.SetValue(AValue: string);
begin
  FState := scisSet;
  FValue := AValue;
end;

{ TSvnCustomConfig }

procedure TSvnCustomConfig.GetStringValue(AConfig: PSvnConfig; ASection, AOption: PAnsiChar;
  AConfigString: TSvnConfigString);
var
  OptionValue: PAnsiChar;
begin
  svn_config_get(AConfig, OptionValue, ASection, AOption, nil);
  if Assigned(OptionValue) then
    AConfigString.Value := UTF8ToString(OptionValue)
  else
    AConfigString.State := scisRemove;
end;

procedure TSvnCustomConfig.SetStringValue(AConfig: PSvnConfig; ASection, AOption: PAnsiChar;
  AConfigString: TSvnConfigString);
begin
  if AConfigString.State = scisSet then
    svn_config_set(AConfig, ASection, AOption, PAnsiChar(UTF8Encode(AConfigString.Value)))
  else
  if AConfigString.State = scisRemove then
    svn_config_set(AConfig, ASection, AOption, nil);
end;

{ TSvnServerConfig }

constructor TSvnServerConfig.Create;
begin
  inherited Create;
  FHttpProxyExceptions := TSvnConfigString.Create;
  FHttpProxyHost := TSvnConfigString.Create;
  FHttpProxyPassword := TSvnConfigString.Create;
  FHttpProxyPort := TSvnConfigString.Create;
  FHttpProxyUsername := TSvnConfigString.Create;
end;

destructor TSvnServerConfig.Destroy;
begin
  FHttpProxyExceptions.Free;
  FHttpProxyHost.Free;
  FHttpProxyPassword.Free;
  FHttpProxyPort.Free;
  FHttpProxyUsername.Free;
  inherited Destroy;
end;

procedure TSvnServerConfig.GetConfig(AConfig: PSvnConfig);
begin
  GetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_HOST, FHttpProxyHost);
  GetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_PORT, FHttpProxyPort);
  GetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_USERNAME, FHttpProxyUsername);
  GetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_PASSWORD, FHttpProxyPassword);
  GetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_EXCEPTIONS, FHttpProxyExceptions);
end;

procedure TSvnServerConfig.SetConfig(AConfig: PSvnConfig);
begin
  SetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_HOST, FHttpProxyHost);
  SetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_PORT, FHttpProxyPort);
  SetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_USERNAME, FHttpProxyUsername);
  SetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_PASSWORD, FHttpProxyPassword);
  SetStringValue(AConfig, SVN_CONFIG_SECTION_GLOBAL, SVN_CONFIG_OPTION_HTTP_PROXY_EXCEPTIONS, FHttpProxyExceptions);
end;

{ TSvnBlameOptions }

constructor TSvnBlameOptions.Create;
begin
  inherited Create;
  FIgnoreEOL := False;
  FIgnoreSpace := False;
  FIgnoreSpaceAll := False;
end;

procedure TSvnBlameOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TSvnBlameOptions then
  begin
    TSvnBlameOptions(Dest).IgnoreEOL := IgnoreEOL;
    TSvnBlameOptions(Dest).IgnoreSpace := IgnoreSpace;
    TSvnBlameOptions(Dest).IgnoreSpaceAll := IgnoreSpaceAll;
  end
  else
    inherited AssignTo(Dest);
end;

type
  TSvnClientManager = class(TObject)
  private
    FAprLoadCounter: Integer;
    FLock: TCriticalSection;
    FSvnLoadCounter: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeAprLib;
    procedure FreeSvnLibs;
    function LoadAprLib: string;
    function LoadSvnLibs: string;
  end;

{ TSvnClientManager }

constructor TSvnClientManager.Create;
begin
  inherited Create;
  FAprLoadCounter := 0;
  FLock := TCriticalSection.Create;
  FSvnLoadCounter := 0;
end;

destructor TSvnClientManager.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

procedure TSvnClientManager.FreeAprLib;
begin
  FLock.Enter;
  try
    if FAprLoadCounter > 0 then
    begin
      Dec(FAprLoadCounter);
      if FAprLoadCounter = 0 then
        apr.FreeAprLib;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSvnClientManager.FreeSvnLibs;
begin
  FLock.Enter;
  try
    if FSvnLoadCounter > 0 then
    begin
      Dec(FSvnLoadCounter);
      if FSvnLoadCounter = 0 then
      begin
        FreeSvnClientLib;
        FreeSvnDeltaLib;
        FreeSvnDiffLib;
        FreeSvnFsLib;
        FreeSvnRaLib;
        FreeSvnReposLib;
        FreeSvnSubrLib;
        FreeSvnWcLib;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

function TSvnClientManager.LoadAprLib: string;
var
  CurrentDir: string;
begin
  Result := '';
  FLock.Enter;
  try
    if FAprLoadCounter = 0 then
    begin
      CurrentDir := GetCurrentDir;
      try
        SetCurrentDir(BaseDllDir);
        if apr.LoadAprLib(sLibAprDll, BaseDllDir) then
          Result := ''
        else
          Result := sLibAprDll;
      finally
        SetCurrentDir(CurrentDir);
      end;
    end
    else
      Result := '';
    if Result  = '' then
      Inc(FAprLoadCounter);
  finally
    FLock.Leave;
  end;
end;

function TSvnClientManager.LoadSvnLibs: string;
var
  CurrentDir: string;
begin
  Result := '';
  FLock.Enter;
  try
    if FSvnLoadCounter = 0 then
    begin
      CurrentDir := GetCurrentDir;
      try
        SetCurrentDir(BaseDllDir);
        if not LoadSvnClientLib(sLibSvnClientDll, BaseDllDir) then
        begin
          Result := sLibSvnClientDll;
          Exit;
        end;
        if not LoadSvnSubrLib(sLibSvnSubrDll, BaseDllDir) then
        begin
          Result := sLibSvnSubrDll;
          Exit;
        end;
        if not LoadSvnDeltaLib(sLibSvnDeltaDll, BaseDllDir) then
        begin
          Result := sLibSvnDeltaDll;
          Exit;
        end;
        if not LoadSvnDiffLib(sLibSvnDiffDll, BaseDllDir) then
        begin
          Result := sLibSvnDiffDll;
          Exit;
        end;
        if not LoadSvnRaLib(sLibSvnRaDll, BaseDllDir) then
        begin
          Result := sLibSvnRaDll;
          Exit;
        end;
        if not LoadSvnReposLib(sLibSvnReposDll, BaseDllDir) then
        begin
          Result := sLibSvnReposDll;
          Exit;
        end;
        if not LoadSvnWcLib(sLibSvnWcDll, BaseDllDir) then
        begin
          Result := sLibSvnWcDll;
          Exit;
        end;
        if not LoadSvnFsLib(sLibSvnFsDll, BaseDllDir) then
        begin
          Result := sLibSvnFsDll;
          Exit;
        end;
      finally
        SetCurrentDir(CurrentDir);
      end;
    end;
    if Result  = '' then
      Inc(FSvnLoadCounter);
  finally
    FLock.Leave;
  end;
end;

var
  GSvnClientManager: TSvnClientManager = nil;

function SvnClientManager: TSvnClientManager;
begin
  if not Assigned(GSvnClientManager) then
    GSvnClientManager := TSvnClientManager.Create;
  Result := GSvnClientManager;
end;

procedure FreeSvnClientManager;
begin
  FreeAndNil(GSvnClientManager);
end;

{ TSvnClient private }

procedure TSvnClient.GetExternalsCallback(Sender: TObject; Item: TSvnItem; var Cancel: Boolean);
begin
  Cancel := False;
  if Assigned(FExternals) and (Item.TextStatus = svnWcStatusExternal) then
    FExternals.Add(Item.PathName);
end;

procedure TSvnClient.GetFirstStatus(const PathName: string;
  var Status: TStatusEntry; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Revision: TSvnOptRevision;
  SvnPathName: string;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;

    SvnPathName := NativePathToSvnPath(PathName, SubPool);

    Status.Valid := False;
    SvnCheck(svn_client_status2(nil, PAnsiChar(UTF8Encode(SvnPathName)),
      @Revision, WCStatus4, Pointer(@Status), False, True, False, False, False,
      FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.GetHeadRevision(const URL: string; SubPool: PAprPool): TSvnRevNum;
var
  NewPool: Boolean;
  EncodedURL: PAnsiChar;
  RASession: PSvnRaSession;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    EncodedURL := svn_path_uri_encode(PAnsiChar(UTF8Encode(URL)), SubPool);
    SvnCheck(svn_client_open_ra_session(RASession, EncodedURL, FCtx, SubPool));
    SvnCheck(svn_ra_get_latest_revnum(RASession, Result, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.GetInitialized: Boolean;
begin
  Result := Assigned(FAllocator);
end;

procedure TSvnClient.GetListCallback(Sender: TObject; Path: string; DirEntry: TSvnDirEnt; Locked: Boolean;
  LockData: TSvnLock; AbsPath: string; var Cancel: Boolean);
begin
  Cancel := False;
  if Assigned(FListStrings) then
  begin
    if Length(Path) = 0 then
      Path := '.'
    else
      AbsPath := AbsPath + '/' + Path;
    FListStrings.Add(Path + '=' + AbsPath);
  end;
end;

{ TSvnClient protected }

function TSvnClient.DoBlame(LineNo: Int64; Revision: TSvnRevNum; const Author, Date, Line: string): Boolean;
var
  D: TDateTime;
  L: Integer;
  SLine: string;
begin
  Result := False;
  if Assigned(FBlameCallback) then
  begin
    D := SvnStrToDateTime(Date, FBlameSubPool);
    SLine := Line;
    L := Length(SLine);
    if (L > 0) and (SLine[L] = #13) then
      Delete(SLine, L, 1);
    FBlameCallback(Self, LineNo, Revision, Author, D, SLine, Result);
    FCancelled := Result;
  end;
end;

function TSvnClient.DoCancel: Boolean;
var
  Cancel: Boolean;
begin
  if Assigned(FOnCancel) then
  begin
    Cancel := False;
    FOnCancel(Self, Cancel);
    if Cancel then
      FCancelled := True;
  end;
  Result := Cancelled;
end;

function TSvnClient.DoList(Path: string; DirEntry: TSvnDirEnt; Locked: Boolean; LockData: TSvnLock; AbsPath: string): Boolean;
begin
  Result := False;
  if Assigned(FListCallback) then
  begin
    FListCallback(Self, Path, DirEntry, Locked, LockData, AbsPath, Result);
    FCancelled := Result;
  end;
end;

function TSvnClient.DoLoginPrompt(const Realm: string; var UserName, Password: string; var Save: Boolean): Boolean;
begin
  Result := not Assigned(FOnLoginPrompt);
  if not Result then
    FOnLoginPrompt(Self, Realm, UserName, Password, Result, Save);
end;

function TSvnClient.DoNotify(const Path, MimeType: string; Action: TSvnWcNotifyAction; Kind: TSvnNodeKind;
  ContentState, PropState: TSvnWCNotifyState; Revision: TSvnRevNum): Boolean;
var
  SPath: string;
begin
  Result := False;
  if Assigned(FNotifyCallback) then
  begin
    SPath := Path;
    if SPath = SvnPathDelim then
      Insert(ExtractFileDrive(GetCurrentDir), SPath, 1)
    else if SPath = SvnExtractFileDrive(SPath) then
      SPath := SvnIncludeTrailingPathDelimiter(SPath);

    FNotifyCallback(Self, SPath, MimeType, Action, Kind, ContentState, PropState, Revision, Result);
    FCancelled := Result;
  end;
end;

procedure TSvnClient.DoProgress(Progress, Total: TAprOff);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Total);
end;

function TSvnClient.DoSSLClientCertPrompt(const Realm: string; var CertFileName: string; var Save: Boolean): Boolean;
begin
  Result := not Assigned(FOnSSLClientCertPrompt);
  if not Result then
    FOnSSLClientCertPrompt(Self, Realm, CertFileName, Result, Save);
end;

function TSvnClient.DoSSLClientPasswordPrompt(const Realm: string; var Password: string; var Save: Boolean): Boolean;
begin
  Result := not Assigned(FOnSSLClientPasswordPrompt);
  if not Result then
     FOnSSLClientPasswordPrompt(Self, Realm, Password, Result, Save);
end;

function TSvnClient.DoSSLServerTrustPrompt(const Realm: string; const CertInfo: TSvnAuthSSLServerCertInfo;
  Failures: TSSLServerTrustFailures; var Save: Boolean): Boolean;
begin
  Result := not Assigned(FOnSSLServerTrustPrompt);
  if not Result then
    FOnSSLServerTrustPrompt(Self, Realm, CertInfo, Failures, Result, Save);
end;

function TSvnClient.DoUserNamePrompt(const Realm: string; var UserName: string; var Save: Boolean): Boolean;
begin
  Result := not Assigned(FOnUserNamePrompt);
  if not Result then
    FOnUserNamePrompt(Self, Realm, UserName, Result, Save);
end;

function TSvnClient.DoWCStatus(Path: PAnsiChar; const Status: TSvnWcStatus2): Boolean;
var
  Item: TSvnItem;
begin
  Result := False;
  if Assigned(FStatusCallback) then
  begin
    Item := TSvnItem.Create(Self, nil, UTF8ToString(Path), Status);
    try
      FStatusCallback(Self, Item, Result);
    except
      Item.Free;
      raise;
    end;
    FCancelled := Result;
  end;
end;

{ TSvnClient public }

constructor TSvnClient.Create;
begin
  inherited Create;
  FAllocator := nil;
  FPool := nil;
  FCtx := nil;
  FUserName := '';
  FPassword := '';
  FLastCommitInfoRevision := SVN_INVALID_REVNUM;
  FServerConfig := TSvnServerConfig.Create;
  FBlameOptions := TSvnBlameOptions.Create;
end;

procedure TSvnClient.SaveFileContentToStream(const PathName: string; Revision: TSvnRevNum;
  OutputStream: TStream; SubPool: PAprPool);
var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;
  Buffer: PSvnStringBuf;
  Stream: PSvnStream;
begin
  if not Initialized then
    Initialize;
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    PegRev.Kind := svnOptRevisionUnspecified;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    Rev.Kind := svnOptRevisionNumber;
    Rev.Value.number := Revision;
    Buffer := svn_stringbuf_create('', SubPool);
    Stream := svn_stream_from_stringbuf(Buffer, SubPool);
    FCancelled := False;
    SvnCheck(svn_client_cat2(Stream, PAnsiChar(UTF8Encode(NativePathToSvnPath(PathName))), @PegRev, @Rev,
      FCtx, SubPool));
    OutputStream.Write(Buffer.data^, Buffer.len)
  finally
    FStatusCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.SetRevisionProperty(const URL: string;
  Revision: TSvnRevNum; PropName, PropValue: string; Force: Boolean = True;
  SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  SetRev: TSvnRevNum;
  Rev: TSvnOptRevision;
  SValue: string;
  SvnValue: PSvnString;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    Rev.Kind := svnOptRevisionNumber;
    Rev.Value.number := Revision;
    SValue := PropValue;
    if Pos(';', SValue) <> 0 then
    begin
      if SValue[Length(SValue)] <> ';' then
        SValue := SValue + ';';
      SValue := StringReplace(SValue, ';', SvnLineBreak, [rfReplaceAll, rfIgnoreCase]);
    end;

    if svn_prop_needs_translation(PAnsiChar(UTF8Encode(PropName))) then
    begin
      Assert(SvnLineBreak = #10);
      PropValue := AdjustLineBreaks(PropValue, tlbsLF);
      SvnValue := svn_string_create(PAnsiChar(UTF8Encode(PropValue)), SubPool);
    end
    else
      SvnValue := svn_string_create(PAnsiChar(UTF8Encode(PropValue)), SubPool);

    SvnCheck(svn_client_revprop_set(PAnsiChar(UTF8Encode(PropName)), SvnValue,
      PAnsiChar(UTF8Encode(URL)), @Rev, SetRev, Force, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.StringListToAprArray(List: TStrings;
  SubPool: PAprPool): PAprArrayHeader;
var
  NewPool: Boolean;
  I: Integer;
  P: PAnsiChar;
begin
  Result := nil;

  if not Assigned(List) or (List.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, List.Count, SizeOf(PAnsiChar));

    for I := 0 to List.Count - 1 do
    begin

      P := apr_pstrdup(SubPool, PAnsiChar(AnsiString(List[I])));

      PPAnsiChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;

end;

destructor TSvnClient.Destroy;
begin
  FBlameOptions.Free;
  FServerConfig.Free;
  Finalize;
  inherited Destroy;
end;

procedure TSvnClient.CheckDllLoadError(const DLLName: string);
begin
  if DLLName <> '' then
    raise DllLoadException.Create(DllName);
end;

procedure TSvnClient.Add(const PathName: string; Recurse, Force, NoIgnore: Boolean; SubPool: PAprPool);
var
  NewPool: Boolean;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FCancelled := False;
    SvnCheck(svn_client_add3(PAnsiChar(UTF8Encode(NativePathToSvnPath(PathName))), Recurse, Force, NoIgnore, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.AddToChangeList(PathNames: TStrings; const AChangeList: string;
  SvnDepth: TSvnDepth = svnDepthFiles; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Paths: PAprArrayHeader;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    SvnCheck(svn_client_add_to_changelist(Paths, PAnsiChar(UTF8Encode(AChangeList)), SvnDepth, nil, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Blame(const PathName: string; Callback: TSvnBlameCallback; StartRevision: TSvnRevNum = 1;
  EndRevision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  PegRev, StartRev, EndRev: TSvnOptRevision;
  DiffOptions: TSvnDiffFileOptions;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(StartRev, SizeOf(TSvnOptRevision), 0);
    StartRev.Kind := svnOptRevisionNumber;
    if StartRevision <= 0 then
      StartRev.Value.number := 1
    else
      StartRev.Value.number := StartRevision;
    FillChar(EndRev, SizeOf(TSvnOptRevision), 0);
    if EndRevision <= 0 then
      EndRev.Kind := svnOptRevisionBase
    else
    begin
      EndRev.Kind := svnOptRevisionNumber;
      EndRev.Value.number := EndRevision;
    end;
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionUnspecified
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    if FBlameOptions.IgnoreSpaceAll then
      DiffOptions.ignore_space := svnIgnoreSpaceAll
    else
    if FBlameOptions.IgnoreSpace then
      DiffOptions.ignore_space := svnIgnoreSpaceChange
    else
      DiffOptions.ignore_space := svnIgnoreSpaceNone;
    DiffOptions.ignore_eol_style := FBlameOptions.IgnoreEOL;
    DiffOptions.show_c_function := False;

    FCancelled := False;
    FBlameCallback := Callback;
    FBlameSubPool := SubPool;
    SvnCheck(svn_client_blame3(PAnsiChar(UTF8Encode(NativePathToSvnPath(PathName))), @PegRev, @StartRev, @EndRev, @DiffOptions,
      False, BlameReceiver, Self, FCtx, SubPool));
  finally
    FBlameSubPool := nil;
    FBlameCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Checkout(const PathName, TargetDir: string; Callback: TSvnNotifyCallback;
  Recurse, IgnoreExternals: Boolean; Revision, PegRevision: TSvnRevNum;
  SvnCancelCallback: TSvnCancelCallback; SubPool: PAprPool);
var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;
  SaveCancel: TSvnCancelCallback;
  EncodedURL: PAnsiChar;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionHead
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;
    FCancelled := False;
    FNotifyCallback := Callback;
    SaveCancel := FOnCancel;
    try
      if Assigned(SvnCancelCallback) then
        FOnCancel := SvnCancelCallback;
      EncodedURL := svn_path_uri_encode(PAnsiChar(UTF8Encode(PathName)), SubPool);
      SvnCheck(svn_client_checkout2(Revision, EncodedURL,
        PAnsiChar(UTF8Encode(ExcludeTrailingPathDelimiter(TargetDir))), @PegRev, @Rev, Recurse, IgnoreExternals,
        FCtx, SubPool));
    finally
      FOnCancel := SaveCancel;
    end;
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Cleanup(const PathName: string; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FCancelled := False;
    SvnCheck(svn_client_cleanup(PAnsiChar(UTF8Encode(NativePathToSvnPath(PathName))), FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.Commit(PathNames: TStrings; const LogMessage: string; Callback: TSvnNotifyCallback = nil;
  Recurse: Boolean = True; KeepLocks: Boolean = False; SubPool: PAprPool = nil): Boolean;
var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;
begin
  Result := False;

  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    CommitInfo := nil;
    Assert(SvnLineBreak = #10);
    FCommitLogMessage := AdjustLineBreaks(LogMessage, tlbsLF);
    FNotifyCallback := Callback;
    FCancelled := False;
    SvnCheck(svn_client_commit3(CommitInfo, Targets, Recurse, KeepLocks, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(UTF8ToString(CommitInfo^.post_commit_err));

    Result := Assigned(CommitInfo) and (CommitInfo^.revision <> SVN_INVALID_REVNUM);
    FLastCommitInfoRevision := CommitInfo^.revision;
  finally
    FCommitLogMessage := '';
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Export(const PathName, TargetDir: string; Callback: TSvnNotifyCallback;
  Overwrite, Recurse, IgnoreExternals: Boolean; Revision, PegRevision: TSvnRevNum; SubPool: PAprPool);
var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionHead
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;

    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_export3(Revision, PAnsiChar(UTF8Encode(PathName)),
      PAnsiChar(UTF8Encode(TargetDir)), @PegRev, @Rev, Overwrite, IgnoreExternals,
      Recurse, sLineBreak, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Finalize;
begin
  if Assigned(FPool) then
  begin
    apr_pool_destroy(FPool);
    FPool := nil;
  end;
  if Assigned(FPoolUtf8) then
  begin
    apr_pool_destroy(FPoolUtf8);
    FPoolUtf8 := nil;
  end;
  if Assigned(FAllocator) then
  begin
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
  end;
  if FAprLibLoaded then
    apr_terminate2;

  { TODO -cCleanup -ousc : remove if SvnClientManager solution is okay }
  {
  if FSvnClientLibLoaded then
    FreeSvnClientLib;
  if FAprLibLoaded then
    FreeAprLib;
  }
  if FSvnClientLibLoaded then
  begin
    SvnClientManager.FreeSvnLibs;
    FSvnClientLibLoaded := False;
  end;
  if FAprLibLoaded then
  begin
    SvnClientManager.FreeAprLib;
    FAprLibLoaded := False;
  end;
end;

function TSvnClient.FindRepository(const Path: string;
  SubPool: PAprPool): string;
var
  NewPool: Boolean;
  RepoPath: PAnsiChar;
  TempPath: string;
  Error: PSvnError;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    TempPath := Path;
    Error := svn_client_url_from_path(RepoPath, PAnsiChar(UTF8Encode(TempPath)), SubPool);
    if Assigned(Error) then
      RepoPath := nil;
    if RepoPath = nil then
      Result := ''
    else
      Result := UTF8ToString(svn_path_uri_decode(RepoPath, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.FindRepositoryRoot(const Path: string; SubPool: PAprPool): string;
var
  NewPool: Boolean;
  RootPath: PAnsiChar;
  TempPath: string;
  Error: PSvnError;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    TempPath := ExtractFilePath(Path);
    Error := svn_client_root_url_from_path(RootPath, PAnsiChar(UTF8String(TempPath)), FCtx, SubPool);
    if Assigned(Error) then
      RootPath := nil;
    if RootPath = nil then
      Result := ''
    else
      Result := String(UTF8String(RootPath));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function ExtractBasePath(ADirectories: TStringList; var ABasePath: string): Boolean;
var
  I, J, Idx: Integer;
  S, ShortestPath: string;
  TestPaths: TStringList;
  FoundAll: Boolean;
begin
  Result := False;
  ABasePath := '';
  ShortestPath := '';
  for I := 0 to ADirectories.Count - 1 do
    if (ShortestPath = '') or (Length(ShortestPath) > Length(ADirectories[I])) then
      ShortestPath := ADirectories[I];
  TestPaths := TStringList.Create;
  try
    S := ShortestPath;
    while Pos(PathDelim, S) > 0 do
    begin
      TestPaths.Add(S);
      Delete(S, Length(S), 1);
      Idx := LastDelimiter(PathDelim, S);
      if Idx > 0 then
        Delete(S, Idx + 1, Length(S) - Idx);
    end;
    for I := 0 to TestPaths.Count - 1 do
    begin
      S := TestPaths[I];
      FoundAll := True;
      for J := 0 to ADirectories.Count - 1 do
        if Pos(S, ADirectories[J]) = 0 then
        begin
          FoundAll := False;
          Break;
        end;
      if FoundAll then
      begin
        ABasePath := S;
        Result := True;
        Break;
      end;
    end;
  finally
    TestPaths.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
// This function gets the base URL and path for a list of directories and files
//
// Example #1:
// c:\svn\delphisvn\svn\                    [versioned]
// c:\svn\delphisvn\just-some-testing\      [unversioned]
//
// -> base path = c:\svn\delphisvn\svn\
// -> base URL  = URL for base path
//
// Example #2:
// c:\svn\delphisvn\svn\                    [versioned]
// c:\svn\delphisvn\svnide\                 [versioned]
// c:\svn\delphisvn\svnui\                  [versioned]
// c:\svn\delphisvn\just-some-testing\      [unversioned]
//
// -> base path = c:\svn\delphisvn\
// -> base URL  = URL for base path
//
// Example #3:
// c:\svn\delphisvn\svn\                    [versioned]
// d:\svn\jcl\                              [versioned]
//
// -> base path = empty (there is no common path)
// -> base URL  = empty (there is no base path)

function TSvnClient.GetBaseURL(AFilesAndDirectories: TStringList; var ABasePath: string): string;
var
  I: Integer;
  Directories: TStringList;
begin
  Directories := TStringList.Create;
  try
    Directories.Sorted := True;
    Directories.Duplicates := dupIgnore;
    for I := 0 to AFilesAndDirectories.Count - 1 do
      Directories.Add(ExtractFilePath(AFilesAndDirectories[I]));
    for I := Directories.Count - 1 downto 0 do
      if not IsPathVersioned(Directories[I]) then
        Directories.Delete(I);
    if ExtractBasePath(Directories, ABasePath) then
      Result := FindRepository(ABasePath)
    else
      Result := '';
  finally
    Directories.Free;
  end;
end;

procedure TSvnClient.GetChangeLists(const PathName: string; ChangeLists: TStrings;
  SvnDepth: TSvnDepth = svnDepthInfinity; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    ChangeLists.BeginUpdate;
    try
      FChangeLists := ChangeLists;
      try
        SvnCheck(svn_client_get_changelists(PAnsiChar(UTF8Encode(PathName)), nil, SvnDepth,
          ChangeListReceiver, Self, FCtx, SubPool));
      finally
        FChangeLists := nil;
      end;
    finally
      ChangeLists.EndUpdate;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.GetExternals(const PathName: string; Externals: TStrings; Recurse: Boolean);
begin
  Externals.BeginUpdate;
  try
    FExternals := Externals;
    try
      GetModifications(PathName, GetExternalsCallback, Recurse);
    finally
      FExternals := nil;
    end;
  finally
    Externals.EndUpdate;
  end;
end;

function TSvnClient.GetMaxRevision(const PathName: string; SubPool: PAprPool = nil): Integer;
var
  NewPool: Boolean;
  Status: PSvnWcRevisionStatus;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SvnCheck(svn_wc_revision_status(Status, PAnsiChar(UTF8Encode(PathName)), nil, False, nil, nil, SubPool));
    Result := Status^.max_rev;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.GetModifications(const PathName: string; Callback: TSvnStatusCallback;
  Recurse, Update, IgnoreExternals, RecurseUnversioned: Boolean; SubPool: PAprPool): TSvnRevNum;
var
  NewPool: Boolean;
  Revision: TSvnOptRevision;
begin
  Result := -1;
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FRecurseUnversioned := RecurseUnversioned;
    FStatusCallback := Callback;
    SvnCheck(svn_client_status2(@Result, PAnsiChar(UTF8Encode(NativePathToSvnPath(PathName))), @Revision, WCStatus2, Self, Recurse,
      False, Update, False, IgnoreExternals, FCtx, SubPool));
  finally
    FStatusCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.GetProps(Props: PAprArrayHeader; Strings: TStrings; SubPool: PAprPool = nil;
  Delimiter: Char = DefaultPropValDelimiter);
const
  CRLF = #13#10;
var
  NewPool: Boolean;
  P: PSvnClientPropListItem;
  I, J: Integer;
  H: PAprHashIndex;
  PName: PAnsiChar;
  PValue: PSvnString;
  Name, Value: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if not Assigned(Props) then
      Exit;

    NewPool := not Assigned(SubPool);
    if NewPool then
      AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
    try
      P := PPSvnClientPropListItem(Props^.elts)^;
      for I := 0 to Props^.nelts - 1 do
      begin
        H := apr_hash_first(SubPool, P^.prop_hash);
        while Assigned(H) do
        begin
          apr_hash_this(H, @PName, 0, @PValue);
          // special Subversion properties, stored as UTF8
          if svn_prop_needs_translation(PName) or // svn:
            (StrLComp(PName, 'bugtraq:', 8) = 0) or (StrLComp(PName, 'tsvn:', 5) = 0) then // TortoiseSVN
            SvnCheck(svn_subst_detranslate_string(PValue, PValue, False, SubPool));

          SetString(Name, PName, StrLen(PName));
          SetString(Value, PValue^.data, StrLen(PValue^.data));

          Value := StringReplace(Value, CRLF, Delimiter, [rfReplaceAll, rfIgnoreCase]);
          if (Value <> '') and (Value[Length(Value)] = Delimiter) then
            Delete(Value, Length(Value), 1);

          J := Strings.IndexOfName(Name);
          if J = -1 then
            Strings.Add(Format('%s=%s', [Name, Value]))
          else
            Strings.ValueFromIndex[J] := Strings.ValueFromIndex[J] + Delimiter + Value;

          H := apr_hash_next(H);
        end;

        Inc(P);
      end;
    finally
      if NewPool then
        apr_pool_destroy(SubPool);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TSvnClient.GetRevisionProperties(const URL: string; Revision: TSvnRevNum;
  Properties: TStrings; SubPool: PAprPool = nil; Delimiter: Char = DefaultPropValDelimiter);
const
  CRLF = #13#10;
var
  NewPool: Boolean;
  SetRev: TSvnRevNum;
  Rev: TSvnOptRevision;
  props: PAprHash;
  H: PAprHashIndex;
  PName: PAnsiChar;
  PValue: PSvnString;
  J: Integer;
  Name, Value: string;
begin
  Properties.BeginUpdate;
  try
    if not Initialized then
      Initialize;

    NewPool := not Assigned(SubPool);
    if NewPool then
      AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
    try
      FillChar(Rev, SizeOf(TSvnOptRevision), 0);
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
      SvnCheck(svn_client_revprop_list(props, PAnsiChar(UTF8Encode(URL)), @Rev, SetRev, FCtx, SubPool));

      H := apr_hash_first(SubPool, props);
      while Assigned(H) do
      begin
        apr_hash_this(H, @PName, 0, @PValue);

        // special Subversion properties, stored as UTF8
        if svn_prop_needs_translation(PName) or // svn:
          (StrLComp(PName, 'bugtraq:', 8) = 0) or (StrLComp(PName, 'tsvn:', 5) = 0) then // TortoiseSVN
          SvnCheck(svn_subst_detranslate_string(PValue, PValue, False, SubPool));

        SetString(Name, PName, StrLen(PName));
        SetString(Value, PValue^.data, StrLen(PValue^.data));

        Value := StringReplace(Value, CRLF, Delimiter, [rfReplaceAll, rfIgnoreCase]);
        if (Value <> '') and (Value[Length(Value)] = Delimiter) then
          Delete(Value, Length(Value), 1);

        J := Properties.IndexOfName(Name);
        if J = -1 then
          Properties.Add(Format('%s=%s', [Name, Value]))
        else
          Properties.ValueFromIndex[J] := Properties.ValueFromIndex[J] + Delimiter + Value;

        H := apr_hash_next(H);
      end;
    finally
      if NewPool then
        apr_pool_destroy(SubPool);
    end;
  finally
    Properties.EndUpdate;
  end;
end;

procedure TSvnClient.Import(const PathName, Url: string; SvnDepth: TSvnDepth);
var
  CommitInfo: PSvnCommitInfo;
  RevPropTable: PAprHash;
begin
  CommitInfo := nil;
  RevPropTable := nil;
  SvnCheck(svn_client_import3(CommitInfo, PAnsiChar(AnsiString(PathName)),
    PAnsiChar(AnsiString(Url)), SvnDepth, True, True, RevPropTable, FCtx, FPool));
end;

procedure TSvnClient.Initialize(const AConfigDir: string = ''; Auth: PSvnAuthBaton = nil);
var
  Providers: PAprArrayHeader;
  Provider: PSvnAuthProviderObject;
  S: string;
  P: PAnsiChar;
  SvnConfig: PSvnConfig;
begin
  if not FAprLibLoaded then
  begin
    CheckDllLoadError(SvnClientManager.LoadAprLib);
    FAprLibLoaded := True;
  end;

  if not FSvnClientLibLoaded then
  begin
    CheckDllLoadError(SvnClientManager.LoadSvnLibs);
    FSvnClientLibLoaded := True;
  end;

  AprCheck(apr_initialize);
  FPoolUtf8 := svn_pool_create_ex(nil, nil);
  svn_utf_initialize(FPoolUtf8);
  SvnCheck(svn_nls_init);
  AprCheck(apr_allocator_create(FAllocator));
  try
    apr_allocator_max_free_set(Allocator, SVN_ALLOCATOR_RECOMMENDED_MAX_FREE);
    AprCheck(apr_pool_create_ex(FPool, nil, nil, FAllocator));
    try
      SvnCheck(svn_ra_initialize(FPool));
      SvnCheck(svn_client_create_context(FCtx, FPool));
      if GetEnvironmentVariable('ASP_DOT_NET_HACK') <> '' then
        SvnCheck(svn_wc_set_adm_dir('_svn', FPool));
      FCtx^.notify_func := SvnContextNotify;
      FCtx^.notify_baton := Self;
      FCtx^.progress_func := SvnContextProgress;
      FCtx^.progress_baton := Self;
      FCtx^.cancel_func := SvnContextCancel;
      FCtx^.cancel_baton := Self;
      FCtx^.log_msg_func := SvnContextLogMessage;
      FCtx^.log_msg_baton := Self;

      if AConfigDir = '' then
        S := IncludeTrailingPathDelimiter(GetAppDataDir) + 'Subversion'
      else
        S := ExcludeTrailingPathDelimiter(AConfigDir);
      SvnCheck(svn_utf_cstring_to_utf8(@P, PAnsiChar(AnsiString(S)), FPool));
      P := svn_path_canonicalize(P, FPool);
      SvnCheck(svn_config_ensure(P, FPool));
      SetString(FConfigDir, P, StrLen(P));
      SvnCheck(svn_config_get_config(FCtx^.config, PAnsiChar(UTF8Encode(FConfigDir)), FPool));

      if not Assigned(Auth) then
      begin
        Provider := nil;
        Providers := apr_array_make(FPool, 14, SizeOf(PSvnAuthProviderObject));

//	svn_config_t * cfg_config = (svn_config_t *)apr_hash_get(ctx->config, SVN_CONFIG_CATEGORY_CONFIG, APR_HASH_KEY_STRING);

//	/* Populate the registered providers with the platform-specific providers */
//	svn_auth_get_platform_specific_client_providers(&providers, cfg_config, pool);
        SvnConfig := apr_hash_get(FCtx^.config, PAnsiChar('config'), APR_HASH_KEY_STRING);
        svn_auth_get_platform_specific_client_providers(Provider, SvnConfig, FPool);

        svn_auth_get_simple_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_auth_get_simple_provider2(Provider, PlaintextPrompt, Self, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;

        if Assigned(svn_auth_get_platform_specific_provider) then
        begin
          svn_auth_get_platform_specific_provider(Provider, 'windows', 'ssl_server_trust', FPool);
          if Assigned(Provider) then
            PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        end;

        svn_client_get_windows_simple_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_simple_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_username_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_server_trust_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_pw_file_provider(Provider, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_simple_prompt_provider(Provider, SimplePrompt, Self, 3, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_username_prompt_provider(Provider, UserNamePrompt, Self, 3, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_server_trust_prompt_provider(Provider, SSLServerTrustPrompt, Self, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_prompt_provider(Provider, SSLClientCertPrompt, Self, 0, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_client_get_ssl_client_cert_pw_prompt_provider(Provider, SSLClientPasswordPrompt, Self, 0, FPool);
        if Assigned(Provider) then
          PPSvnAuthProviderObject(apr_array_push(Providers))^ := Provider;
        svn_auth_open(Auth, Providers, FPool);
      end;
      Ctx^.auth_baton := Auth;
      if FUserName <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_USERNAME, PAnsiChar(UTF8Encode(FUserName)));
      if FPassword <> '' then
        svn_auth_set_parameter(Auth, SVN_AUTH_PARAM_DEFAULT_PASSWORD, PAnsiChar(UTF8Encode(FPassword)));

      SvnConfig := apr_hash_get(FCtx^.config, PAnsiChar(SVN_CONFIG_CATEGORY_SERVERS), APR_HASH_KEY_STRING);
      FServerConfig.SetConfig(SvnConfig);
    except
      apr_pool_destroy(FPool);
      FPool := nil;
      raise;
    end;
  except
    apr_pool_destroy(FPoolUtf8);
    apr_allocator_destroy(FAllocator);
    FAllocator := nil;
    raise;
  end;
end;

function TSvnClient.IsPathVersioned(const PathName: string): Boolean;
var
  SubPool: PAprPool;
  TruePath: PAnsiChar;
  Revision, PegRevision: TSvnOptRevision;
  SvnError: PSvnError;
begin
  Result := False;
  if (PathName = '') or (GetFileAttributes(PChar(PathName)) = Cardinal(-1)) then // path does not exist
    Exit;

  if not Initialized then
    Initialize;

  AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRevision, SizeOf(TSvnOptRevision), 0);
    PegRevision.Kind := svnOptRevisionUnspecified;
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionUnspecified;
    AprCheck(apr_filepath_merge(TruePath, '', PAnsiChar(UTF8Encode(PathName)), APR_FILEPATH_TRUENAME, SubPool));
    FCancelled := False;
    SvnError := svn_client_info(TruePath, @PegRevision, @Revision, DummyInfoReceiver, nil, False, Ctx, SubPool);
    Result := not Assigned(SvnError);
    if not Result then
    begin
      case SvnError^.apr_err of
        SVN_ERR_WC_NOT_DIRECTORY, SVN_ERR_WC_PATH_NOT_FOUND, SVN_ERR_UNVERSIONED_RESOURCE:
          svn_error_clear(SvnError);
        else
          RaiseSvnError(SvnError);
      end;
    end;
  finally
    apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; DirEntryFields: DWORD = SVN_DIRENT_ALL;
  Callback: TSvnListCallback = nil; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  PegRev, Rev: TSvnOptRevision;
  EncodedPathOrURL: PAnsiChar;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionUnspecified
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FillChar(Rev, SizeOf(TSvnOptRevision), 0);
    if Revision <= 0 then
      Rev.Kind := svnOptRevisionHead
    else
    begin
      Rev.Kind := svnOptRevisionNumber;
      Rev.Value.number := Revision;
    end;

    FCancelled := False;
    FListCallback := Callback;
    EncodedPathOrURL := svn_path_uri_encode(PAnsiChar(UTF8Encode(PathName)), SubPool);
    SvnCheck(svn_client_list2(EncodedPathOrURL, @PegRev, @Rev, Depth, DirEntryFields, FetchLocks, ListReceiver, Self,
      FCtx, SubPool));
  finally
    FListCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.List(const PathName: string; Depth: TSvnDepth; FetchLocks: Boolean; ListStrings: TStrings;
  DirEntryFields: DWORD = SVN_DIRENT_ALL; Revision: TSvnRevNum = -1; PegRevision: TSvnRevNum = -1; SubPool: PAprPool = nil);
begin
  ListStrings.BeginUpdate;
  try
    FListStrings := ListStrings;
    try
      List(PathName, Depth, FetchLocks, DirEntryFields, GetListCallback, Revision, PegRevision, SubPool);
    finally
      FListStrings := nil;
    end;
  finally
    ListStrings.EndUpdate;
  end;
end;

function TSvnClient.MatchGlobalIgnores(const PathName: string; SubPool: PAprPool = nil): Boolean;
var
  NewPool: Boolean;
  GlobalIgnores: PAprArrayHeader;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SvnCheck(svn_wc_get_default_ignores(GlobalIgnores, FCtx^.config, SubPool));
    Result := svn_cstring_match_glob_list(PAnsiChar(UTF8Encode(PathName)), GlobalIgnores);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Merge(const Source1: string; Revision1: TSvnRevNum; const Source2: string; Revision2: TSvnRevNum;
  const TargetWcpath: string; Callback: TSvnNotifyCallback = nil; SvnCancelCallback: TSvnCancelCallback = nil;
  Depth: TSvnDepth = svnDepthInfinity; IgnoreAncestry: TSvnBoolean = False; Force: TSvnBoolean = False;
  RecordOnly: TSvnBoolean = False; DryRun: TSvnBoolean = False; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Rev1, Rev2: TSvnOptRevision;
  EncodedSource1, EncodedSource2: PAnsiChar;
  SaveCancel: TSvnCancelCallback;
begin
  if not Initialized then
    Initialize;
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    FillChar(Rev1, SizeOf(TSvnOptRevision), 0);
    if Revision1 <= 0 then
      Rev1.Kind := svnOptRevisionHead
    else
    begin
      Rev1.Kind := svnOptRevisionNumber;
      Rev1.Value.number := Revision1;
    end;
    FillChar(Rev2, SizeOf(TSvnOptRevision), 0);
    if Revision2 <= 0 then
      Rev2.Kind := svnOptRevisionHead
    else
    begin
      Rev2.Kind := svnOptRevisionNumber;
      Rev2.Value.number := Revision2;
    end;
    FCancelled := False;
    FNotifyCallback := Callback;
    SaveCancel := FOnCancel;
    try
      if Assigned(SvnCancelCallback) then
        FOnCancel := SvnCancelCallback;
      EncodedSource1 := svn_path_uri_encode(PAnsiChar(UTF8Encode(Source1)), SubPool);
      EncodedSource2 := svn_path_uri_encode(PAnsiChar(UTF8Encode(Source2)), SubPool);
      SvnCheck(svn_client_merge3(EncodedSource1, @Rev1, EncodedSource2, @Rev2,
        PAnsiChar(UTF8Encode(TargetWcpath)), Depth, IgnoreAncestry, Force, RecordOnly, DryRun,
        nil, FCtx, SubPool));
    finally
      FOnCancel := SaveCancel;
    end;
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.MergePeg(const Source: string; RangesToMerge: TSvnMergeRevisionList;
  const TargetWcpath: string; PegRevision: TSvnRevNum;
  Callback: TSvnNotifyCallback; SvnCancelCallback: TSvnCancelCallback;
  Depth: TSvnDepth; IgnoreAncestry, Force, RecordOnly, DryRun: TSvnBoolean;
  IgnoreEOL: Boolean; IgnoreSpace: Boolean; IgnoreSpaceAll: Boolean;
  SubPool: PAprPool);
var
  NewPool: Boolean;
  PegRev: TSvnOptRevision;
  RangesToMergeArray: PAprArrayHeader;
  EncodedSource: PAnsiChar;
  SaveCancel: TSvnCancelCallback;

  Options: TStringList;
  OptionsArray: PAprArrayHeader;
begin
  if not Initialized then
    Initialize;
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Options := TStringList.Create;
    try
      if IgnoreEOL then
        Options.Add('--ignore-eol-style');
      if IgnoreSpaceAll then
        Options.Add('-w')
      else
      if IgnoreSpace then
        Options.Add('-b');
      if Options.Count > 0 then
        OptionsArray := StringListToAprArray(Options, SubPool)
      else
        OptionsArray := nil;
    finally
      Options.Free;
    end;
    RangesToMergeArray := RangesToMerge.ToAprArray(Self, SubPool);
    FillChar(PegRev, SizeOf(TSvnOptRevision), 0);
    if PegRevision <= 0 then
      PegRev.Kind := svnOptRevisionHead
    else
    begin
      PegRev.Kind := svnOptRevisionNumber;
      PegRev.Value.number := PegRevision;
    end;
    FCancelled := False;
    FNotifyCallback := Callback;
    SaveCancel := FOnCancel;
    try
      if Assigned(SvnCancelCallback) then
        FOnCancel := SvnCancelCallback;
      EncodedSource := svn_path_uri_encode(PAnsiChar(UTF8Encode(Source)), SubPool);
      SvnCheck(svn_client_merge_peg3(EncodedSource, RangesToMergeArray, @PegRev,
        PAnsiChar(UTF8Encode(TargetWcpath)), Depth, IgnoreAncestry, Force, RecordOnly, DryRun,
        OptionsArray, FCtx, SubPool));
    finally
      FOnCancel := SaveCancel;
    end;
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.MkDir(const Paths: TStringList; const Comment: string;
  MakeParents: Boolean; SubPool: PAprPool): Boolean;
var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;
  RevPropTable: PAprHash;
  SvnError: PSvnError;
begin
  Result := False;
  if not Assigned(Paths) or (Paths.Count = 0) then
    Exit;
  if not Initialized then
    Initialize;
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := StringListToAprArray(Paths, SubPool);
    CommitInfo := nil;
    RevPropTable := nil;
    FCommitLogMessage := Comment;
    SvnError := svn_client_mkdir3(CommitInfo, Targets, MakeParents, RevPropTable, FCtx, SubPool);
    Result := Assigned(CommitInfo);
    if Assigned(SvnError) then
    begin
      if SvnError^.apr_err = SVN_ERR_FS_ALREADY_EXISTS then
        svn_error_clear(SvnError)
      else
        RaiseSvnError(SvnError);
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Move(SrcPathNames: TStrings; const DstPath: string; Force: Boolean = True; MoveAsChild: Boolean = False;
  MakeParents: Boolean = False; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  SrcPaths: PAprArrayHeader;
  CommitInfo: PSvnCommitInfo;
begin
  if not Assigned(SrcPathNames) or (SrcPathNames.Count = 0) then
    Exit;
  if not Initialized then
    Initialize;
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    SrcPaths := PathNamesToAprArray(SrcPathNames, SubPool);
    CommitInfo := nil;
    FCancelled := False;
    SvnCheck(svn_client_move5(CommitInfo, SrcPaths, PAnsiChar(UTF8Encode(NativePathToSvnPath(DstPath))),
      Force, MoveAsChild, MakeParents, nil, FCtx, SubPool));
    if Assigned(CommitInfo) and Assigned(CommitInfo^.post_commit_err) and (CommitInfo^.post_commit_err^ <> #0) then
      raise Exception.Create(UTF8ToString(CommitInfo^.post_commit_err));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.NativePathToSvnPath(const NativePath: string; SubPool: PAprPool = nil): string;
var
  NewPool: Boolean;
  SvnPath: PAnsiChar;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(SvnPath, '', PAnsiChar(UTF8Encode(NativePath)), APR_FILEPATH_TRUENAME, SubPool));
    Result := UTF8ToString(SvnPath);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.PathNamesToAprArray(PathNames: TStrings; SubPool: PAprPool): PAprArrayHeader;
var
  NewPool: Boolean;
  I: Integer;
  CurrentDrive, S: string;
  P: PAnsiChar;
begin
  Result := nil;

  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  CurrentDrive := ExtractFileDrive(GetCurrentDir);

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, PathNames.Count, SizeOf(PAnsiChar));

    for I := 0 to PathNames.Count - 1 do
    begin
      // Work around an apparent Subversion glitch:
      // Svn update reproducibly fails with error SVN_ERR_WC_LOCKED "Attempted to lock an already-locked dir"
      // in two cases:
      // 1. A directory with trailing path delimiter, e.g. D:\Temp\Test\ (D:/Temp/Test/ in SVN notation)
      //    Workaround: always exclude the trailing path delimiter

      S := ExcludeTrailingPathDelimiter(PathNames[I]);

      // 2. Root directory which is the same as the current directory drive, e.g. 'D:\' or 'D:'
      //    when current directory is anywhere on drive D:
      //    Workaround: use '/'

      if S = CurrentDrive then
        P := SvnPathDelim
      else
        AprCheck(apr_filepath_merge(P, '', PAnsiChar(UTF8Encode(S)), APR_FILEPATH_TRUENAME, SubPool));

      PPAnsiChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.PathNamesToAprArray(const PathNames: array of string; SubPool: PAprPool): PAprArrayHeader;
var
  NewPool: Boolean;
  I: Integer;
  CurrentDrive, S: string;
  P: PAnsiChar;
begin
  Result := nil;

  if Length(PathNames) = 0 then
    Exit;

  if not Initialized then
    Initialize;

  CurrentDrive := ExtractFileDrive(GetCurrentDir);

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Result := apr_array_make(SubPool, Length(PathNames), SizeOf(PAnsiChar));

    for I := Low(PathNames) to High(PathNames) do
    begin
      // Work around an apparent Subversion glitch:
      // Svn update reproducibly fails with error SVN_ERR_WC_LOCKED "Attempted to lock an already-locked dir"
      // in two cases:
      // 1. A directory with trailing path delimiter, e.g. D:\Temp\Test\ (D:/Temp/Test/ in SVN notation)
      //    Workaround: always exclude the trailing path delimiter

      S := ExcludeTrailingPathDelimiter(PathNames[I]);

      // 2. Root directory which is the same as the current directory drive, e.g. 'D:\' or 'D:'
      //    when current directory is anywhere on drive D:
      //    Workaround: use '/'

      if S = CurrentDrive then
        P := SvnPathDelim
      else
        AprCheck(apr_filepath_merge(P, '', PAnsiChar(UTF8Encode(S)), APR_FILEPATH_TRUENAME, SubPool));

      PPAnsiChar(apr_array_push(Result))^ := P;
    end;
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Revert(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Paths: PAprArrayHeader;
begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    FCancelled := False;
    FNotifyCallback := Callback;
    SvnCheck(svn_client_revert(Paths, Recurse, FCtx, SubPool));
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.RemoveFromChangeList(PathNames: TStrings; SvnDepth: TSvnDepth = svnDepthFiles; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Paths: PAprArrayHeader;
begin
  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Paths := PathNamesToAprArray(PathNames, SubPool);
    SvnCheck(svn_client_remove_from_changelists(Paths, SvnDepth, nil, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Resolved(const SvnPath: string; ConflictChoice: TSvnWcConflictChoice = SvnWcConflictChooseMerged;
  Recurse: Boolean = False; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Depth: TSvnDepth;
  Status: TStatusEntry;
  Path: string;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    GetFirstStatus(SvnPath, Status);
    if (Status.Valid) then
    begin
      Path := IncludeTrailingPathDelimiter(ExtractFilePath(SvnPath));
      DeleteFile(Path + Status.Conflict_old);
      DeleteFile(Path + Status.Conflict_new);
      if ConflictChoice <> SvnWcConflictChooseMineFull then
        DeleteFile(Path + Status.Conflict_wrk);
    end;
    if Recurse then
      Depth := svnDepthInfinity
    else
      Depth := svnDepthEmpty;
    SvnCheck(svn_client_resolve(PAnsiChar(UTF8Encode(Path)), Depth,
      ConflictChoice, FCtx, SubPool));
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

function TSvnClient.SvnPathToNativePath(const SvnPath: string; SubPool: PAprPool = nil): string;
var
  NewPool: Boolean;
  NativePath: PAnsiChar;
begin
  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    AprCheck(apr_filepath_merge(NativePath, '', PAnsiChar(UTF8Encode(SvnPath)), APR_FILEPATH_NATIVE, SubPool));
    Result := UTF8ToString(NativePath);
  finally
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

procedure TSvnClient.Update(PathNames: TStrings; Callback: TSvnNotifyCallback = nil; Recurse: Boolean = True;
  IgnoreExternals: Boolean = False; ConflictCallBack: TSvnConflictCallback = nil;
  SvnCancelCallback: TSvnCancelCallback = nil; SubPool: PAprPool = nil);
var
  NewPool: Boolean;
  Targets: PAprArrayHeader;
  Revision: TSvnOptRevision;
  PCtx: PSvnClientCtx;
  SaveCancel: TSvnCancelCallback;
begin
  if not Assigned(PathNames) or (PathNames.Count = 0) then
    Exit;

  if not Initialized then
    Initialize;

  NewPool := not Assigned(SubPool);
  if NewPool then
    AprCheck(apr_pool_create_ex(SubPool, FPool, nil, FAllocator));
  try
    Targets := PathNamesToAprArray(PathNames, SubPool);
    FillChar(Revision, SizeOf(TSvnOptRevision), 0);
    Revision.Kind := svnOptRevisionHead;
    FCancelled := False;
    FNotifyCallback := Callback;
    svn_client_create_context(PCtx, SubPool);
    PCtx^ := FCtx^;
    if @ConflictCallBack <> nil then
    begin
      PCtx^.conflict_func := ConflictReceiver;
      FConflictCallback := ConflictCallBack;
      PCtx^.conflict_baton := Pointer(Self);
    end;
    SaveCancel := FOnCancel;
    try
      if Assigned(SvnCancelCallback) then
        FOnCancel := SvnCancelCallback;
      SvnCheck(svn_client_update2(nil, Targets, @Revision, Recurse, IgnoreExternals, PCtx, SubPool));
    finally
      FOnCancel := SaveCancel;
    end;
  finally
    FNotifyCallback := nil;
    if NewPool then
      apr_pool_destroy(SubPool);
  end;
end;

{ THistoryUpdateThread }

procedure THistoryUpdateThread.Completed;
begin
  if not Unloading then
  begin
    FAsyncUpdate.Completed;
    FSvnItem.HistoryThreadTerminated;
  end;
end;

constructor THistoryUpdateThread.Create(SvnItem: TSvnItem;
  const AsyncUpdate: IAsyncUpdate);
begin
  inherited Create(False);
  FSvnItem := SvnItem;
  FAsyncUpdate := AsyncUpdate;
  FLastAdded := 0;
  FreeOnTerminate := True;
  OnTerminate := Completed;
end;

function HistoryThreadCancel(cancel_baton: Pointer): PSvnError; cdecl;
begin
  Result := nil;
  if Assigned(cancel_baton) and THistoryUpdateThread(cancel_baton).Terminated then
      Result := svn_error_create(SVN_ERR_CANCELLED, nil, PAnsiChar(UTF8Encode(sCancelledByUser)));
end;

procedure THistoryUpdateThread.Execute;
var
  SubPool: PAprPool;
  StartRevision, EndRevision: TSvnOptRevision;
  Targets: PAprArrayHeader;
  Error: PSvnError;
  PCtx: PSvnClientCtx;
  EncodedURL: PAnsiChar;
  SL: TStringList;
begin
  NameThreadForDebugging('DelphiSVN History Updater');
  FSvnItem.FHistory := TList.Create;
  try
    if FSvnItem.TextStatus in [svnWcStatusNone, svnWcStatusUnversioned] then
      Exit;

    AprCheck(apr_pool_create_ex(SubPool, FSvnItem.SvnClient.Pool, nil, FSvnItem.SvnClient.Allocator));
    try
      FillChar(StartRevision, SizeOf(TSvnOptRevision), 0);
      if FSvnItem.LogFirstRev = -1 then
        StartRevision.Kind := svnOptRevisionHead
      else
      begin
        StartRevision.Kind := svnOptRevisionNumber;
        StartRevision.Value.number := FSvnItem.LogFirstRev;
      end;
      FillChar(EndRevision, SizeOf(TSvnOptRevision), 0);
      EndRevision.Kind := svnOptRevisionNumber;
      if FSvnItem.LogLastRev = -1 then
        EndRevision.Value.number := 0
      else
        EndRevision.Value.number := FSvnItem.LogLastRev;
      if (FSvnItem.SvnPathName = '') and (FSvnItem.URL <> '') then
      begin
        EncodedURL := svn_path_uri_encode(PAnsiChar(UTF8Encode(FSvnItem.URL)), SubPool);
        SL := TStringList.Create;
        try
          SL.Add(EncodedURL);
          Targets := FSvnItem.SvnClient.StringListToAprArray(SL, SubPool);
        finally
          SL.Free;
        end;
      end
      else
        Targets := FSvnItem.SvnClient.PathNamesToAprArray([FSvnItem.SvnPathName], SubPool);
      FSvnItem.SvnClient.FCancelled := False;
      svn_client_create_context(PCtx, SubPool);
      PCtx^ := FSvnItem.SvnClient.Ctx^;
      PCtx^.cancel_func := HistoryThreadCancel;
      PCtx^.cancel_baton := Pointer(Self);
      Error := svn_client_log2(Targets, @StartRevision, @EndRevision, FSvnItem.LogLimit,
        FSvnItem.IncludeChangeFiles, False, LogMessage, FSvnItem, PCtx, SubPool);
      if not Terminated then
      begin
        if Assigned(Error) then
        begin
          if Error^.apr_err =  SVN_ERR_CANCELLED then
            svn_error_clear(Error)
          else if Error^.apr_err = APR_OS_START_SYSERR + WSAHOST_NOT_FOUND then
            svn_error_clear(Error)
          else
            RaiseSvnError(Error);
        end
        else
          InternalUpdate(True);
      end;
    finally
      apr_pool_destroy(SubPool);
    end;
  except
    if not Terminated then
    begin
      FSvnItem.ClearHistory;
      raise;
    end;
  end;
end;

procedure THistoryUpdateThread.InternalUpdate(ForceUpdate: Boolean);
begin
  FAsyncUpdate.UpdateHistoryItems(FSvnItem, FLastAdded, FNewItemIndex, ForceUpdate);
  FLastAdded := FNewItemIndex + 1;
end;

procedure THistoryUpdateThread.NewUpdate;
var
  Item: TSvnHistoryItem;
  Time: TAprTime;
  NewItemIndex: Integer;
begin
  Item := TSvnHistoryItem.Create;
  try
    Item.FOwner := FSvnItem;
    Item.FRevision := Revision;
    Item.FAuthor := string(UTF8String(Author));
    Item.FLogMessage := string(UTF8String(Message));
    if Assigned(Date) and (Date^ <> #0) then
    begin
      SvnCheck(svn_time_from_cstring(Time, Date, Pool));
      Item.FTime := AprTimeToDateTime(Time);
    end
    else
      Item.FTime := 0;

    NewItemIndex := FSvnItem.FHistory.Add(Item);
//    apr_pool_clear(Pool);
    FAsyncUpdate.UpdateHistoryItems(FSvnItem, NewItemIndex, NewItemIndex, True);
  except
    Item.Free;
    raise;
  end;
end;

procedure THistoryUpdateThread.ResetUpdate;
begin
  FLastAdded := 0;
end;

procedure THistoryUpdateThread.Update;
begin
  InternalUpdate(False);
end;

{ TSvnClientAnnotationLineProvider }

constructor TSvnClientAnnotationLineProvider.Create(SvnHistoryItem: TSvnHistoryItem);
begin
  inherited Create;
  FSvnHistoryItem := SvnHistoryItem;
  FMaxGutterWidth := 0;
end;

function TSvnClientAnnotationLineProvider.GetCount: Integer;
begin
  Result := FSvnHistoryItem.BlameCount;
end;

function TSvnClientAnnotationLineProvider.GetGutterInfo(Index: Integer): string;
begin
  Result := ' ' + IntToStr(FSvnHistoryItem.BlameItems[Index].Revision) + ' ';
end;

function TSvnClientAnnotationLineProvider.GetMaxGutterWidth: Integer;
begin
  if FMaxGutterWidth = 0 then
  begin
    FMaxGutterWidth := Length(IntToStr(FSvnHistoryItem.FMaxRevision)) + 2;
    Result := FMaxGutterWidth;
  end
  else
    Result := FMaxGutterWidth;
end;

function TSvnClientAnnotationLineProvider.GetHintStr(Index: Integer): string;
var
  I: Integer;
  Revision: Integer;
begin
//
// TODO: DLW Need to add a hash to allow doing a lookup by revision number to be fast.
  Revision := FSvnHistoryItem.BlameItems[Index].Revision;
  for I  := 0 to FSvnHistoryItem.Owner.HistoryCount - 1 do
    if FSvnHistoryItem.Owner.HistoryItems[I].Revision = Revision then
    begin
      Result := FSvnHistoryItem.Owner.HistoryItems[I].HintString;
      Break;
    end;
end;

function TSvnClientAnnotationLineProvider.GetIntensity(Index: Integer): Integer;
var
  Time: Integer;
  Diff: Integer;
begin
  Time := DateTimeToFileDate(FSvnHistoryItem.BlameItems[Index].Time);
  Diff := FSvnHistoryItem.MaxTime - FSvnHistoryItem.MinTime;
  Result := Trunc((Time - FSvnHistoryItem.MinTime) div ((Diff div 10) + 1)) * 100;
end;

{ DllLoadException }

constructor DllLoadException.Create(const AFileName: string);
begin
  FLastError := GetLastError;
  inherited CreateFmt('%d %s: %s', [FLastError, SysErrorMessage(FLastError), AFileName]);
  FFileName := AFileName;
end;

function DllLoadException.GetFileNotFoundError: Boolean;
begin
  Result := FLastError = 2;
end;

initialization

finalization
  FreeSvnClientManager;

end.
