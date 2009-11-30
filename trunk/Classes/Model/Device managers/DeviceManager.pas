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
    function GetVolumes: TStrings;
    function FilterVolumes(drivePath: PChar): boolean;
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
  SysUtils, WMI, ShellObjExtended, Volume, StrUtils;

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
      Delete(deviceMountPoint,Length(deviceMountPoint),1);
      if QueryDosDevice(PChar(deviceMountPoint), PChar(@bufChar), sizeof(bufChar)) = 0
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
      end;
    end; //finally
  end; //then - removable
end; //FilterDevices

//This function gets all
function TDeviceManager.GetVolumes: TStrings;
var
  volName: TCharArray; //name buffer
  handle: THandle; //handle of the first system volume
  i: integer; //counter
  s: string;
  device: TDevice;
  fList: TStringList;
begin
  //we get the first volume in system
  fList := TStringList.Create;
  handle := FindFirstVolumeA(volName,sizeof(volName));
  if handle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end
  else begin
    fList.Add(String(volName));
    //we get all other volumes
    while FindNextVolumeA(handle,volName,sizeof(volName)) do
    begin
      fList.Add(String(volName));
    end;
    FindVolumeClose(handle);
  end;
  {TODO: Somewhere there's a leak of memory, so there's Invalid pointer operation}
  //IT IS COMPILER'S BUG!!! THERE ARE EXACTLY NO LEAKS!!!
  for i := 0 to fList.Count-1 do
  begin
    s := fList.Strings[i];
    if FilterVolumes(PChar(s))
    then begin
      device := TVolume.Create(s);
      try
        device.Destroy;
      except
      end;
    end;
  end;
end;


//Constructor
constructor TDeviceManager.Create;
begin
  inherited Create;
  //create lists: event listeners and devices
  //fEventHandlers := TList.Create;
  //fDevices := TList.Create;
  //create logical drive letters list
  //fLogicalDrives := TStringList.Create;
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
  IT DOES NOT WORK PROPERLY!!!
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
