{
  Abstract class - parent class for all device managers
  Developed by J.L.Blackrow
}

{
  TODO: Repair device rescanning
}

unit DeviceManager;

interface
uses
  Windows, Classes, DeviceException, Messages, WinIOCtl, Device, Dbt;

type

  //Abstract class for device manager
  TDeviceManager = class(TObject)
  private
    fEventHandlers: TList;
    function FilterVolumes(drivePath: PChar): boolean;
    function LinkDevices(var Volumes: TList; var Drives: TList): TList;
    procedure GetVolumesAndDrives(var Volumes: TList; var Drives: TList);
    procedure RegisterNotification;
    procedure WndProc(var Msg: TMessage); message WM_DEVICECHANGE;
  protected
    fDevices: TList;
    fLogicalDrives: TStringList;
    fWindowPointer: Pointer;
    constructor Create;
    function BuildAll(Devices: TList): TList; virtual; abstract;
    procedure FillDevices;
    procedure HandleMessage(var Msg: TMessage); virtual; abstract;
    function SetMessageFilter: TDEV_BROADCAST_DEVICEINTERFACE; virtual; abstract;
    procedure NotifyAll;
  public
    destructor Destroy; override;
    //These functions depend on device type
    procedure RemoveDrive(index: integer); overload; virtual; abstract;
    procedure RemoveDrive(device: TDevice); overload; virtual; abstract;
    procedure ForcedRemoveDrive(index: integer); virtual; abstract;
    //These funtions are common for all devices
    function GetBlockedFiles: TStrings;
    function GetDeviceInfo(index: integer): TDevice; overload; virtual;
    function GetDeviceInfo(name: PChar): TDevice; overload; virtual;
    function GetDeviceCount: integer; virtual;
    //These functions add event handlers from listeners
    procedure AddHandler(Handler: TNotifyEvent);
    procedure RemoveHandler(Handler: TNotifyEvent);
  end;

{==============================================================================}
implementation

uses
  SysUtils, WMI, ShellObjExtended, Volume, StrUtils, Drive;

//Filters only necessary devices which match the type criteria
function TDeviceManager.FilterVolumes(drivePath: PChar): boolean;
var
  bufChar: TCharArray; //char buffer
  deviceMountPoint: string; //device first mount point
  bufStr: TStrings; //strings buffer
begin
  Result := false;
  bufStr := nil;
  //we check if this drive is removable or not
  if GetDriveType(drivePath) = DRIVE_REMOVABLE
  then begin
    try
      bufStr := TVolume.GetVolumeMountPoints(drivePath);
      deviceMountPoint := bufStr[0];
      Delete(deviceMountPoint, Length(deviceMountPoint),1);
      {TODO: how to filter floppy drives?!?!}
      if QueryDosDevice(PChar(@deviceMountPoint[1]), PChar(@bufChar[0]), sizeof(bufChar)) = 0
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end //then - error
      else begin
        //checking if the device is a floppy drive
        if (Copy(bufChar,1,14) <> DEV_FLOPPY)
        then begin
          Result := true;
        end; //then - not floppy
      end; //else - floppy
    finally
      if Assigned(bufStr)
      then begin
        bufStr.Destroy;
      end; //then - destroy buffer
    end; //finally
  end; //then - removable
end; //FilterDevices

//This function links two lists in a one containing "device hives"
function TDeviceManager.LinkDevices(var Volumes: TList; var Drives: TList): TList;
var
  i,j: integer; //for-loop indexes
  volume: TDevice; //volume device
  disk: TDevice; //disk device
  parentHandle: THandle; //volume parent handle
begin
  Result := TList.Create;
  //we iterare over the drives array
  for i := 0 to Drives.Count-1 do
  begin
    disk := TDevice(Drives.Items[i]);
    //iterating over the volumes array
    for j := 0 to Volumes.Count-1 do
    begin
      volume := TDevice(Volumes.Items[j]);
      //getting parent instance handle
      if CM_Get_Parent(parentHandle, volume.InstanceHandle, 0) = CR_SUCCESS
      then begin
        //checking if the disk is the parent device of the current volume
        if parentHandle = disk.InstanceHandle
        then begin
          disk.AddChild(volume);
        end; //then - disk is a parent device
      end; //then - successfully got the parent's number
      //adding to the result
    end; //for-loop-j
    Result.Add(disk);
  end; //for-loop-i
end; //GetPhysicalDrives

//This procedure gets all available volumes and physical drives
procedure TDeviceManager.GetVolumesAndDrives(var Volumes: TList;
  var Drives: TList);
type
  DiskSet = set of byte;
var
  volName: TCharArray; //name buffer
  handle: THandle; //handle of the first system volume
  fVolumeNames: TStringList; //buffer for volume names
  i, j: integer; //loop indexes
  volume: TVolume; //temporary object for volume
  driveIndex: Cardinal; //current drive index
  diskIndexes: DiskSet; //set for disk indexes
begin
  //getting the first volume in the system
  handle := FindFirstVolumeA(PChar(@volName[0]), sizeof(volName));
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - failed
  else begin
    try
      fVolumeNames := TStringList.Create;
      fVolumeNames.Add(String(volName));
      //getting all other volumes
      while FindNextVolumeA(handle, PChar(@volName[0]), sizeof(volName)) do
      begin
        fVolumeNames.Add(String(volName));
      end; //while
    finally
      FindVolumeClose(handle);
    end; //finally
    diskIndexes := [];
    for i := 0 to fVolumeNames.Count-1 do
    begin
      //filtering the volumes
      if FilterVolumes(PChar(@fVolumeNames.Strings[i][1]))
      then begin
        volume := TVolume.Create(fVolumeNames.Strings[i]);
        for j := 0 to volume.VolumeDiskNumbers.Count-1 do
        begin
          driveIndex := Cardinal(volume.VolumeDiskNumbers.Items[j]^);
          //checking if the drive already presents
          if not (driveIndex in diskIndexes)
          then begin
            diskIndexes := diskIndexes + [driveIndex];
            Drives.Add(TDiskDrive.Create(Format(DrivePattern, [driveIndex])));
          end; //then - add new disk
        end; //for-loop
        Volumes.Add(volume);
      end; //volume is removable
    end; //for-loop
  end; //else - opened
end; //GetVolumesAndDrives

//This procedure fills the device list
procedure TDeviceManager.FillDevices;
var
  fVolumes, fDrives: TList; //temporary lists
begin
  fVolumes := TList.Create;
  fDrives := TList.Create;
  if Assigned(fDevices)
  then begin
    fDevices.Clear;
  end;
  GetVolumesAndDrives(fVolumes, fDrives);
  fDevices := BuildAll(LinkDevices(fVolumes, fDrives));
  if not Assigned(fDevices)
  then begin
    fDevices := TList.Create;
  end; //then - no devices were detected
  fVolumes.Destroy;
  fDrives.Destroy;
  //NotifyAll;
end; //FillDevices

//This function registers this class for handling Windows messages
procedure TDeviceManager.RegisterNotification;
var
  windowHandle: THandle; //new window handle
  messageFilter: TDEV_BROADCAST_DEVICEINTERFACE; //message filter
begin
  //setting filters
  messageFilter := SetMessageFilter;
  //registering window class for handling messages
  windowHandle := AllocateHWnd(WndProc);
  fWindowPointer := RegisterDeviceNotification(windowHandle,
    @messageFilter, DEVICE_NOTIFY_ALL_INTERFACE_CLASSES);
  if not Assigned(fWindowPointer)
  then begin
    raise EDeviceException.Create('Cannot register message handler');
  end; //then - failed to register message handler
end; //RegisterNotification

//This procedure gets Windows messages
procedure TDeviceManager.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DEVICECHANGE
  then begin
    HandleMessage(Msg);
  end; //then - it's a device notification
end; //WndProc

{TODO: how to refresh devices on a new start}

{CONSTRUCTOR}
constructor TDeviceManager.Create;
begin
  inherited Create;
  fEventHandlers := TList.Create;
  FillDevices;
  RegisterNotification;
end;

{DESTRUCTOR}
destructor TDeviceManager.Destroy;
var
  i: integer;
begin
  UnregisterDeviceNotification(fWindowPointer);
  if Assigned(fEventHandlers)
  then begin
    for i := 0 to fEventHandlers.Count-1 do
    begin
      FreeMem(fEventHandlers.Items[i]);
    end;
    fEventHandlers.Free;
  end;
  if Assigned(fDevices)
  then begin
    for i := 0 to fDevices.Count-1 do
    begin
      TDevice(fDevices.Items[i]).Destroy;
    end;
    fDevices.Free;
  end;
  inherited Destroy;
end; //Destroy

function TDeviceManager.GetBlockedFiles: TStrings;
begin
  Result := nil;
end;

function TDeviceManager.GetDeviceCount: integer;
begin
  Result := fDevices.Count;
end;

function TDeviceManager.GetDeviceInfo(name: PChar): TDevice;
begin
  Result := nil;
end;

//Device info getter
function TDeviceManager.GetDeviceInfo(index: integer): TDevice;
begin
  try
    Result := TDevice(fDevices.Items[index]);
  except
    on e: Exception do
    begin
      raise EDeviceException.Create(e, e.Message);
    end;
  end;
end; //GetDeviceInfo

//Adds new listener to list
procedure TDeviceManager.AddHandler(Handler: TNotifyEvent);
begin
  if (fEventHandlers.IndexOf(@Handler) = -1) //is not in the list
  then begin
    fEventHandlers.Add(@Handler);
    Handler(self);
  end;
end;

//Removes event listener
procedure TDeviceManager.RemoveHandler(Handler: TNotifyEvent);
begin
  fEventHandlers.Remove(@Handler);
end; //RemoveHandler

//Notifies all listeners about changes in this object

{TODO: Synchronize with a view object!!!}
{TODO: fix invalid typecast}
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
end; //NotifyAll

end.





