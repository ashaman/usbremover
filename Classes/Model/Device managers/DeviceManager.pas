{
  Abstract class - parent class for all device managers
}
unit DeviceManager;

interface
uses
  Windows, Classes, DeviceException, Messages, WinIOCtl, Device;

type

  //Abstract class for device manager
  TDeviceManager = class(TObject)
  private
    fEventHandlers: TList;
    function GetLogicalDrives(const Volumes: TStrings): TStrings; //gets all logical drives in system
    function GetVolumes: TStrings;
    function FilterDevices(drivePath: PChar): boolean;
  protected
    fDevices: TList;
    fLogicalDrives: TStringList;
    constructor Create;
    procedure NotifyAll;
    procedure ProcessMessages(var msg: TMessage); message WM_DeviceChange;
  public
    destructor Destroy; override;
    //These functions depend on device type
    procedure RemoveDrive(index: integer); overload; virtual; abstract;
    procedure RemoveDrive(device: TDevice); overload; virtual; abstract;
    procedure ForcedRemoveDrive(index: integer); virtual; abstract;
    //These funtions are common for all devices
    function GetBlockedFiles: TStrings;
    function GetBlockerID: HWND;
    function GetDeviceInfo(handle: THandle): TDevice; overload;
    function GetDeviceInfo(name: PChar): TDevice; overload;
    function GetDeviceCount: integer;
    //These functions add event handlers from listeners
    procedure AddHandler(Handler: TNotifyEvent);
    procedure RemoveHandler(Handler: TNotifyEvent);
  end;

{==============================================================================}
implementation

uses
  SysUtils, WMI, ShellObjExtended;

//Filters only necessary devices which match the type criteria
function TDeviceManager.FilterDevices(drivePath: PChar): boolean;
var
  bufChar: TCharArray;
  driveType: Cardinal;
begin
  Result := false;
  //we check if this drive is removable or fixed
  driveType := GetDriveTypeA(drivePath);
  if (driveType = DRIVE_REMOVABLE) or (driveType = DRIVE_FIXED)
  then begin
    QueryDosDevice(PChar(Copy(drivePath,1,2)),@bufChar[0],MAXCHAR);
    //checking if the device is a floppy drive
    if (Copy(bufChar,1,14) <> DEV_FLOPPY)
    then begin
      Result := true;
    end;
  end;
end;


{
//This function gets all logical drives in system
procedure TDeviceManager.GetLogicalDrives;
const
  charCount = 4; //four characters describe each drive, e.g.: c:\<null-term>
var
  drives: PAnsiChar; //buffer for driver strings
  pDrives: Pointer; //pointer to the drives array
  bufSize: DWORD; //size of buffer
  i: integer; //counter
  driveNumber: integer; //number of drives
begin
  //clearing the list
  fLogicalDrives.Clear;
  //here we get size needed for buffer
  bufSize := GetLogicalDriveStrings(0,nil);
  if bufSize<>0
  then begin //everything is OK
    drives := AllocMem(bufSize); //we alloc memory
    pDrives := drives; //save pointer to the beginning of the array
    GetLogicalDriveStrings(bufSize,drives);
    driveNumber := (bufSize-1) div charCount; //we count the quantity of drives
    for i := 1 to driveNumber do
    begin
      if FilterDevices(drives) //skipping floppies and CDROM's
      then begin
        fLogicalDrives.Add(drives);
        //fDevices.Add(TDevice.Create(drives));
      end; //filter
      Inc(drives, charCount);  //move to the next list item
    end; //drives
    FreeMem(pDrives,bufSize); //we release resources
  end //bufSize<>0
  else begin
    raise EDeviceException.Create('Initialization failed!');
  end; //Raise
end; //GetDrives
}

{
//This function gets all logical drives in system
procedure TDeviceManager.GetDrives;
var
  devInfo: THandle; //device info handle
  devInfoData: TSPDevInfoData; //device info
  devInterfaceData: TSPDeviceInterfaceData; //device interface info
  devInterfaceDefailData: TSPDeviceInterfaceDetailData; //concrete info
  i: integer; //counter
  dwSize: Cardinal; //dummy integer
begin
  //First, we get all disk devices in the system. Then, we choose
  //which are removable and then add them to the device list
  devInfo := SetupDiGetClassDevsA(@GUID_DEVCLASS_DISKDRIVE, nil, HWND(nil),
    DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if devInfo = INVALID_HANDLE_VALUE
  then begin
  end //then
  else begin
    i := 0;
    devInterfaceData.cbSize := sizeof(devInterfaceData);
    while (SetupDiEnumDeviceInterfaces(devInfo,nil,GUID_DEVCLASS_DISKDRIVE,
      i,devInterfaceData)) do
    begin
      inc(i);
      SetupDiGetDeviceInterfaceDetailA(devInfo,@devInterfaceData,nil,0,dwSize,nil);
      if dwSize = 0
      then begin
      end //fail
      else begin
        devInfoData.cbSize := dwSize;
        devInterfaceDefailData.cbSize := 5;
        SetupDiGetDeviceInterfaceDetailA(devInfo,@devInterfaceData,
          @devInterfaceDefailData,dwSize,dwSize,nil);

          ////!!!!!!!!!!!Look for SafeRemove by Bagel
          //// the previous works good

      end;
    end; //while
  end; //else
end; //GetDrives}


function TDeviceManager.GetLogicalDrives(const Volumes: TStrings): TStrings;
var
  driveName: TCharArray;
  pDriveName: PChar;
  i: integer;
  dummy: Cardinal; //for function
  s: string;
  size: integer;
  handle: THandle;
  buf: TVolumeDiskExtents;

begin


  for i := 0 to Volumes.Count-1 do
  begin
    s := Volumes.Strings[i];
    s[3] := '.';
    Delete(s,Length(s),1);
    handle := CreateFile(PChar(s),GENERIC_READ, FILE_SHARE_READ
      or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if handle <> INVALID_HANDLE_VALUE
    then begin
      if DeviceIoControl(handle,IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS, nil,0,
        @buf,sizeof(buf),dummy,nil)
      then begin
        dummy := buf.Extents[0].DiskNumber;
      end;
      CloseHandle(handle);
    end;
    {
    if GetVolumePathNamesForVolumeNameA(PChar(Volumes.Strings[i]),
      driveName, sizeof(driveName), dummy)
    then begin
      s := String(driveName);
      pDriveName := @driveName;
      size := 0;
      while (s <> '') do
      begin
        inc(size, Length(s));
        s := String(pDriveName + Length(s)+1);
        //inc(driveName, Length(driveName));
      end;
      //FreeMem(pDriveName, size);
    end //if-then
    else begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError)); //
      exit;
    end;
    }
  end;
end;


//This function gets all
function TDeviceManager.GetVolumes: TStrings;
var
  volName: TCharArray; //name buffer
  handle: THandle; //handle of the first system volume
  volList: TStringList; //list of all volumes
  i: integer; //counter
  s: string;
begin
  //we get the first volume in system
  Result := TStringList.Create;
  handle := FindFirstVolumeA(volName,sizeof(volName));
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end
  else begin
    Result.Add(String(volName));
    //we get all other volumes
    while FindNextVolumeA(handle,volName,sizeof(volName)) do
    begin
      Result.Add(String(volName));
    end;
    FindVolumeClose(handle);
  end;
end;


//Constructor
constructor TDeviceManager.Create;
begin
  inherited Create;
  //create lists: event listeners and devices
  fEventHandlers := TList.Create;
  fDevices := TList.Create;
  //create logical drive letters list
  fLogicalDrives := TStringList.Create;
  GetVolumes;
end;

//Destructor
destructor TDeviceManager.Destroy;
var
  i: integer;
begin
  for i := 0 to fEventHandlers.Count-1 do
  begin
    FreeMem(fEventHandlers.Items[i]);
  end;
  for i := 0 to fDevices.Count-1 do
  begin
    TDevice(fDevices.Items[i]).Destroy;
  end;
  fEventHandlers.Destroy;
  fDevices.Destroy;
  fLogicalDrives.Destroy;
  inherited Destroy;
end;

function TDeviceManager.GetBlockerID: HWND;
begin
  Result := 0;
end;

function TDeviceManager.GetBlockedFiles: TStrings;
begin
  Result := nil;
end;

function TDeviceManager.GetDeviceCount: integer;
begin
  Result := fDevices.Count;
end;

{
  IT DOES NOT WORD PROPERLY!!!
}

//This function searches the device by its handle
//If such a device does not exist, this function throws EDeviceException
function TDeviceManager.GetDeviceInfo(handle: THandle): TDevice;
var
  i: integer;
  found: boolean;
begin
  i := 0;
  found := false;
  Result := nil;
  while (i<=self.fDevices.Count-1) and not found do
  begin
    if true
    then begin
      Result := TDevice(fDevices.Items[i]);
      found := true;
    end {if-handle}
    else begin
      inc(i);
    end; {else}
  end; {while}
  if not found
  then begin
    raise EDeviceException.Create('Device with the specified handle does not exist!');
  end; {not found}
end; {GetDeviceInfo}

function TDeviceManager.GetDeviceInfo(name: PChar): TDevice;
begin
  Result := nil;
end;

//Adds new listener to list
procedure TDeviceManager.AddHandler(Handler: TNotifyEvent);
begin
  if (fEventHandlers.IndexOf(@Handler) = -1) //is not in the list
  then begin
    fEventHandlers.Add(@Handler);
  end;
end;

//Removes event listener
procedure TDeviceManager.RemoveHandler(Handler: TNotifyEvent);
begin
  fEventHandlers.Remove(@Handler);
end;

//Notifies all listeners about changes in this object
procedure TDeviceManager.NotifyAll;
var
  i, count: integer;
  event: TNotifyEvent;
begin
  count := fEventHandlers.Count;
  for i := 0 to count-1 do
  begin
    event := TNotifyEvent(fEventHandlers.Items[i]^);
    if Assigned(event)
    then begin
      event(Self);
    end;
  end;
end;

procedure TDeviceManager.ProcessMessages(var msg: TMessage);
begin
end;


end.
