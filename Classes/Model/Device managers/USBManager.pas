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

//This is the event name that should de used if someone wants to access the
//manager's properties
const
  EventName = 'USRTargetInvocationEvt[{DCF5F2E1-52BA-40C6-A5AF-68AFFE002A8D}]';

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
  USBDevice, ProcessManager, Volume;

var
  Instance: TUSBManager;
  fManualResetEvent: THandle;

function TUSBManager.GetDeviceInfo(index: integer): TDevice;
begin
  ResetEvent(fManualResetEvent);
  try
    Result := inherited GetDeviceInfo(index);
  finally
    SetEvent(fManualResetEvent);
    PulseEvent(fManualResetEvent);
  end;
end;

function TUSBManager.GetDeviceInfo(name: PChar): TDevice;
begin
  ResetEvent(fManualResetEvent);
  try
    Result := inherited GetDeviceInfo(name);
  finally
    SetEvent(fManualResetEvent);
    PulseEvent(fManualResetEvent);
  end;
end;

function TUSBManager.GetDeviceCount: integer;
begin
  ResetEvent(fManualResetEvent);
  try
    Result := inherited GetDeviceCount;
  finally
    SetEvent(fManualResetEvent);
    PulseEvent(fManualResetEvent);
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
  fManualResetEvent := CreateEvent(nil, true, true, PChar(EventName));
  if fManualResetEvent = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end;
end; //Create

{DESTRUCTOR}
destructor TUSBManager.Destroy;
begin
  CloseHandle(fManualResetEvent);
  inherited Destroy;
end; //Destroy

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
  errorCode: Cardinal; //error code
begin
  ZeroMemory(@vetoName[0],sizeof(vetoName));
  try
    {WORKS UNSTABLE!!!}
    errorCode := CM_Request_Device_EjectA(device.InstanceHandle, nil,
      PWideChar(@vetoName[0]), sizeof(vetoName), 0);
    //trying to eject the device
    if errorCode = CR_SUCCESS
    then begin
      if vetoName = ''
      then begin
        //device was successfully ejected
        //notifying all the windows
        device.NotifySystem;
        fDevices.Remove(device);
        //notifying listenes
        fRemovalSucceeded.Signal(device);
        device.Destroy;
        fBroadcastEvent.Signal(nil);
      end //then - device was ejected
      else begin
        fRemovalFailed.Signal(device);
      end; //
    end //then - ejection succeeded
    else begin
      if errorCode = CR_FAILURE
      then begin
      end //cannot handle too many requests - CR_FAILURE
      else begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end; //other reasons
    end; //else - ejection failed
  finally
  end;
end; //RemoveDrive

//This method makes force drive removal
procedure TUSBManager.ForcedRemoveDrive;
begin
end; //ForcedRemoveDrive

//This procedure runs in a separate thread
procedure TUSBManager.InvokeOnInstallation;
begin
  //loop until the device is installed
  while CMP_WaitNoPendingInstallEvents(2000) = WAIT_TIMEOUT do;
  ResetEvent(fManualResetEvent);
  try
    Instance.FillDevices;
  finally
    SetEvent(fManualResetEvent);
    PulseEvent(fManualResetEvent);
  end; //finally
  EndThread(0);
end; //InvokeOnInstallation

//This function handles Windows messages
procedure TUSBManager.HandleMessage(var Msg: TMessage);
var
  dev: PDEV_BROADCAST_HDR; //device header
  s: string; //temporary string
begin
  if (Msg.WParam = DBT_DEVICEARRIVAL)
  then begin
    dev := PDEV_BROADCAST_HDR(msg.LParam);
    if dev^.dbch_devicetype = DBT_DEVTYP_DEVICEINTERFACE
    then begin
      s := String(PChar(@PDEV_BROADCAST_DEVICEINTERFACE(Msg.LParam)^.dbcc_name[0]));
      if Pos(USBDevicePath, s) <> 0
      then begin
        BeginThread(nil, 0, @TUSBManager.InvokeOnInstallation,
          nil, 0, PDWORD(nil)^);
      end; //then - USB device arrived, need to add to list
    end; //then - device interface
  end; //then - DBT_DEVICEARRIVAL
  if (Msg.WParam = DBT_DEVICEREMOVECOMPLETE)
  then begin
    FillDevices;
  end; //then - DBT_DEVICEREMOVECOMPLETE
end; //HandleMessage

//This function sets Windows message filter
function TUSBManager.SetMessageFilter: TDEV_BROADCAST_DEVICEINTERFACE;
begin
  Result.dbcc_size := sizeof(Result);
  Result.dbcc_devicetype := DBT_DEVTYP_DEVICEINTERFACE;
  Result.dbcc_classguid := GUID_DEVINTERFACE_USB_DEVICE;
end; //SetMessageFilter


{TODO: Find out if it is possible to return disconnected device}

//This is singleton initialization
class function TUSBManager.GetManager: TUSBManager;
begin
  if not Assigned(Instance)
  then begin
    Instance := TUSBManager.Create;
  end;
  Result := Instance;
end; //GetManager

end.
