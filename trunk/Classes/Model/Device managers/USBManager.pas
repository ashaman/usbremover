{
  USB Drive Device manager
  Made as singleton
  Inherited from TDeviceManager
  Developed by J.L. Blackrow
}
unit USBManager;

interface

uses
  DeviceManager, Device, Classes, Messages, Dbt, Windows, SyncObjs;

type
  TUSBManager = class(TDeviceManager)
  private
    procedure InvokeOnInstallation;
    //procedure InvokeOnInstallation(Instance: Pointer);
  protected
    constructor Create;
    function BuildAll(Devices: TList): TList; override;
    procedure HandleMessage(var Msg: TMessage); override;
    function SetMessageFilter: TDEV_BROADCAST_DEVICEINTERFACE; override;
  public
    destructor Destroy; override;
    procedure ForcedRemoveDrive(index: integer); override;
    function GetDeviceInfo(index: integer): TDevice; overload; override;
    function GetDeviceInfo(name: PChar): TDevice; overload; override;
    function GetDeviceCount: integer; override;
    procedure RemoveDrive(index: integer); overload; override;
    procedure RemoveDrive(device: TDevice); overload; override;
    class function GetManager: TUSBManager;
  end;

implementation

uses
  SysUtils, WMI, ShlObj, ShellObjExtended, WinIOCtl, DeviceException,
  USBDevice;

var
  Instance: TUSBManager;
  fCriticalSection: TRTLCriticalSection;

function TUSBManager.GetDeviceInfo(index: integer): TDevice;
begin
  EnterCriticalSection(fCriticalSection);
  try
    Result := inherited GetDeviceInfo(index);
  finally
    LeaveCriticalSection(fCriticalSection);
  end;
end;

function TUSBManager.GetDeviceInfo(name: PChar): TDevice;
begin
  EnterCriticalSection(fCriticalSection);
  try
    Result := inherited GetDeviceInfo(name);
  finally
    LeaveCriticalSection(fCriticalSection);
  end;
end;

function TUSBManager.GetDeviceCount: integer;
begin
  EnterCriticalSection(fCriticalSection);
  try
    Result := inherited GetDeviceCount;
  finally
    LeaveCriticalSection(fCriticalSection);
  end;
end;

//This function completes the building oth the device tree
function TUSBManager.BuildAll(Devices: TList): TList;
var
  fUSBDevices: TStringList; //temporary array of USB devices
  i: integer; //loop indexes
  drive: TDevice; //drive device
  device: TDevice; //USB device
  parentHandle: THandle; //parent device handle
  key: string; //string key for the value
  driveIndex: integer; //drive index in the hashtable
begin
  Result := TList.Create;
  fUSBDevices := TStringList.Create;
  for i := 0 to Devices.Count-1 do
  begin
    drive := TDevice(Devices.Items[i]);
    if CM_Get_Parent(parentHandle, drive.InstanceHandle, 0) = CR_SUCCESS
    then begin
      key := IntToStr(parentHandle);
      //if there's no such device with the specified code - we add it
      driveIndex := fUSBDevices.IndexOf(key);
      if driveIndex = -1
      then begin
        device := TUSBDevice.Create(parentHandle);
        device.AddChild(drive);
        fUSBDevices.AddObject(key, device);
        Result.Add(device);
      end //then - no device in list
      else begin
        device := TDevice(fUSBDevices.Objects[driveIndex]);
        device.AddChild(drive);
      end; //else - linking into the trees
    end; //then- successfully
  end; //for-loop
  fUSBDevices.Free;
end; //BuildAll

{CONSTRUCTOR}
constructor TUSBManager.Create;
begin
  inherited Create;
end; //Create

{DESTRUCTOR}
destructor TUSBManager.Destroy;
begin
  inherited Destroy;
end;

//This function removes drive by its index in the list
procedure TUSBManager.RemoveDrive(index: integer);
var
  Device: TDevice; //current device to remove
begin
  //check if there are any available devices
  if (fDevices.Count <> 0)
  then begin
    try
      //trying to get the device by its index
      Device := TDevice(fDevices.Items[index]);
      self.RemoveDrive(Device);
    except
      on e: Exception do
      begin
        raise EDeviceException.Create(e, e.Message);
      end; //on..do
    end; //except
  end; //then - devices count <> 0
end; //RemoveDrive

//This function removes the specified device
procedure TUSBManager.RemoveDrive(device: TDevice);
var
  vetoName: TCharArray; //the reason of the fail of ejection
begin
  try
    //trying to eject the device
    if CM_Request_Device_EjectA(device.InstanceHandle, nil, PWideChar(@vetoName[0]),
      sizeof(vetoName), 0) = CR_SUCCESS
    then begin
      if vetoName = ''
      then begin
        //notifying all the windows
        device.NotifySystem;
        fDevices.Remove(device);
        device.Destroy;
        NotifyAll;
      end; //then
    end //then - ejection succeeded
    else begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end; //else - ejection failed
  finally
  end;
end; //RemoveDrive

//This method makes force drive removal
procedure TUSBManager.ForcedRemoveDrive;
begin
end; //ForcedRemoveDrive

{
  TODO: Critical section works only with globals!!!
}

//This procedure runs in a separate thread
procedure TUSBManager.InvokeOnInstallation;
begin
  //loop until the device is installed
  while CMP_WaitNoPendingInstallEvents(2000) = WAIT_TIMEOUT do;
  EnterCriticalSection(fCriticalSection);
  try
    Instance.FillDevices;
  finally
    LeaveCriticalSection(fCriticalSection);
  end; //finally
  EndThread(0);
end; //InvokeOnInstallation

//This function handles Windows messages
procedure TUSBManager.HandleMessage(var Msg: TMessage);
var
  dev: PDEV_BROADCAST_HDR; //device header
  s: string; //temporary string
begin
  if (Msg.WParam = DBT_DEVICEARRIVAL) //or (Msg.WParam = DBT_DEVICEREMOVECOMPLETE)
  then begin
    dev := PDEV_BROADCAST_HDR(msg.LParam);
    if dev^.dbch_devicetype = DBT_DEVTYP_DEVICEINTERFACE
    then begin
      s := String(PChar(@PDEV_BROADCAST_DEVICEINTERFACE(Msg.LParam)^.dbcc_name[0]));
      if Pos(USBDevicePath, s) <> 0
      then begin
        {TODO: improve delays!!!}
        {TODO: try to use Delphi thread with critical section?}
        BeginThread(nil, 0, @TUSBManager.InvokeOnInstallation,
          nil, 0, PDWORD(nil)^);
      end; //then - USB device arrived, need to add to list
    end; //then - device interface
  end; //then - DBT_DEVICEARRIVAL or DBT_DEVICEREMOVECOMPLETE
  if (Msg.WParam = DBT_DEVNODES_CHANGED)
  then begin
    //MessageBox(0, PChar('Change'), PChar('!!!'), 0);
  end; //then - DBT_DEVNODES_CHANGED
end; //HandleMessage

//This function sets Windows message filter
function TUSBManager.SetMessageFilter: TDEV_BROADCAST_DEVICEINTERFACE;
begin
  Result.dbcc_size := sizeof(Result);
  Result.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  Result.dbcc_classguid := GUID_DEVINTERFACE_USB_DEVICE;
end; //SetMessageFilter


{TODO: Find out if it is possible to return disconnected device}

class function TUSBManager.GetManager: TUSBManager;
begin
  if not Assigned(Instance)
  then begin
    Instance := TUSBManager.Create;
  end;
  Result := Instance;
end;

initialization
  InitializeCriticalSection(fCriticalSection);

end.
