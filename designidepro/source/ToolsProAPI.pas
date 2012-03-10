//Copyright (c) 2011 Uwe Schuster
unit ToolsProAPI;

interface

uses
  ToolsAPI, DesignIntf, Classes, Graphics, ImgList;

const
  { Default file state value indexes }
  fsiNormal = 0;
  fsiModified = 1;
  fsiConflicted = 2;
  fsiReadOnly = 3;
  fsiDeleted = 4;
  fsiLocked = 5;
  fsiAdded = 6;
  fsiIgnored = 7;
  fsiNonVersioned = 8;

type
  IOTAProProjectManagerMenu155 = interface(IOTAProjectManagerMenu)
  ['{E968EAEA-8ACA-4C95-85E5-DD83830E0FCC}']
    { Return the index of the image to be used for this menu item.  The return
      value is the index in the main application imagelist.  You
      should first add items to this list using INTAServices.AddImages }
    function GetImageIndex: Integer;
  end;

  IOTAProMessageGroup = interface(IOTAMessageGroup)
  ['{359BD23B-4408-4853-A7F5-543BB90A449B}']
    { Returns the index in the imagelist of the message view }
    function GetImageIndex: Integer;
  end;

  IOTAProMessageServices = interface(IOTAMessageServices)
  ['{AA13D79D-7CE6-4066-963F-F06E06A1E804}']
    { This method is an enhanced version of IOTAMessageServices.AddMessageGroup with
      the option to specify the index in the imagelist of the message view.  You
      should first add items to this list using INTAProMessageServices.AddImages }
    function AddMessageGroup(const GroupName: string; ImageIndex: Integer): IOTAMessageGroup;
  end;

  INTAProMessageServices = interface(IInterface)
  ['{CE6CC765-8852-4D90-B49F-53E9C456106A}']
    { AddImages takes all the images from the given image list and adds them to the
      message view imagelist.  Indent is not yet supported! }
    function AddImages(AImages: TCustomImageList; const Ident: string): Integer;
  end;

  IOTAProVersionControlNotifier155 = interface(IOTAVersionControlNotifier150)
  ['{D9B8A970-F0E8-432A-A451-739871972DFB}']
    { This procedure is called when the file browser is creating its local
      menu.  The version control system may add any menu items to
      FileBrowserMenuList that it wishes to have shown }
    procedure FileBrowserMenu(const IdentList: TStrings;
      const FileBrowserMenuList: IInterfaceList; IsMultiSelect: Boolean);
    { Return the index of the image to be used for menu items for this version
      control system.  This index is for example used for the "Open From Version
      Control..." or "Add to Version Control" menu items.  The return
      value is the index in the main application imagelist.  You
      should first add items to this list using INTAServices.AddImages }
    function GetImageIndex: Integer;
    { Return the Caption for "Open From Version Control..." menu item.  It is
      used when only one version control system is installed }
    function GetCheckoutMenuCaption: string;
    { Return the Caption for "Add to Version Control" menu item.  It is used
      when only one version control system is installed }
    function GetAddNewProjectCaption: string;
    { Returns whether "Add to Version Control" is enabled for this version control
      system or not.  This way one can install multiple version control systems,
      but new get always added to the primary one. }
    function GetAddNewProjectEnabled: Boolean;
  end;

  TOTAProFileState = record
    { Index of a common state like fsiNormal.  Look above in this file for other constants.
      Use -1 or values greater than the provided constants for other states }
    FileStateIndex: Integer;
    { Index of the image to be used for the Project Manager tree view and the editor tabs.
      The value is the index in the imagelist of the version control service.  You should
      first add items to this list using INTAProVersionControlServices.AddImages }
    OverlayImageIndex: Integer;
    { Index of the image to be used for the editors status bar.  The value is the index in
      the imagelist of the version control service.  You should first add items to this
      list using INTAProVersionControlServices.AddImages }
    StatusBarImageIndex: Integer;
    { Text used for the file state to be used for the editors status bar. }
    DisplayText: string;
    { Color used for the file name to be used for the Project Manager tree view and the 
      editor tabs. }
    TextColor: TColor;
  end;

  { fsrOK       - indicates file state operation was successful
    fsrError    - indicates file state operation was unsuccessful
    fsrDeferred - indicates file state operation is deferred }
  TOTAProFileStateResult = (fsrOK, fsrError, fsrDeferred);

  IOTAProVersionControlFileStateProvider = interface(IInterface)
  ['{E13971C2-E80E-4B7C-9CFF-753A46F37D14}']
    { This procedure is called after a compile.  The file state provider can now
      perform again any actions. }
    procedure AfterCompile;
    { This procedure is called after the user selected "File | Close All", the
      method IOTAModuleServices.CloseAll was called or implicitly when actions
      like creating a new project or opening an existing project do call Close All
      before.  The file state provider could delete all cached states here }
    procedure AfterCloseAll;
    { This procedure is called before a compile.  The file state provider now
      should stop any actions to avoid any bad interactions during compilation. }
    procedure BeforeCompile;
    { This procedure is called when a file in the given directory has been changed.
      The file state provider should either delete or update all cached states for
      that directory }
    procedure FlushDir(const ADirectory: string);
    { This procedure is called when the given file has been changed.  The file state
      provider should either delete or update the cached state for that file }
    procedure FlushFile(const FileName: string);
    { Return in AFileState the common file state information for the file given in FileName
      and the associated files in the AChildFiles list.

      The function is similar to GetFileState }
    function GetCommonFileState(const FileName: string; AChildFiles: TStrings;
      var AFileState: TOTAProFileState): TOTAProFileStateResult;
    { Return in AFileState the file state information for the file given in FileName.
      The return value of the function indicates if getting the information was successful,
      was not successful for example if the file is not managed or if the operation was
      deferred.

      The file state provider should take that information from a cache, because this
      function is called when the UI is updated and there must not be any slowdown.
      If the information is not in the cache then the file state provider should return
      fsrDeferred, gather the information in a background thread and call
      IOTAProVersionControlServices.InvalidateControls when the information is in the cache }
    function GetFileState(const FileName: string; var AFileState: TOTAProFileState): TOTAProFileStateResult;
    { Return in AProperty an IProperty interface instance with file state related information
      for the file given in FileName.  That information will be shown in the Object Inspector.

      The function is similar to GetFileState apart from the fact that it returns an IProperty
      interface instance than a TOTAProFileState record }
    function GetFileStateInfo(const FileName: string; var AProperty: IProperty): TOTAProFileStateResult;
  end;

  IOTAProVersionControlServices = interface(IOTAVersionControlServices)
  ['{FA453E38-6726-457D-8234-DC364709D57F}']
    { Returns the default TOTAProFileState record in AFileState for the state given by Index.
      An example of a value for Index is fsiNormal.  Look above in this file for other constants.
      The function returns True if the state was found and False if the state was not found }
    function GetDefaultFileStateValues(Index: Integer; var AFileState: TOTAProFileState): Boolean;
    { Invalidates all controls that show file states }
    procedure InvalidateControls;
    { Registers a version control file state provider.  Returns the index of the newly
      registered file state provider }
    function RegisterFileStateProvider(const FileStateProvider: IOTAProVersionControlFileStateProvider): Integer;
    { Unregister a previously registered file state provider }
    procedure UnregisterFileStateProvider(Index: Integer);
  end;

  INTAProVersionControlServices = interface(IInterface)
  ['{593FC525-EAE9-4660-A4FB-37B9A1F179F0}']
    { AddImages takes all the images from the given image list and adds them to the
      version control services imagelist.  Indent is not yet supported! }
    function AddImages(AImages: TCustomImageList; const Ident: string): Integer;
  end;

  IProIDEServices = interface(IBorlandIDEServices)
  ['{F3E8CC21-9500-4CE1-82E9-046A4A931292}']
  end;

implementation

end.
