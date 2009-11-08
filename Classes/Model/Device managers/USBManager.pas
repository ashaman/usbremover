{
  USB Drive Device manager
  Made as singleton
  Inherited from TDeviceManager
}
unit USBManager;

interface
uses
  DeviceManager, Device, Messages, WinIOCtl, Windows, DeviceException;
const
  MAX_ATTEMPTS = 3;

type
  TUSBManager = class(TDeviceManager)
  private
    constructor Create;
  protected
    function FilterDevices(drivePath: PChar): boolean; override;
  public
    destructor Destroy; override;
    procedure RemoveDrive(index: integer); overload; override;
    procedure RemoveDrive(device: TDevice); overload; override;
    procedure ForcedRemoveDrive(index: integer); override;
    class function GetManager: TUSBManager;
  end;

implementation

uses
  SysUtils, WMI, ShlObj;

var
  Instance: TUSBManager;

//Filters only necessary devices which match the type criteria
function TUSBManager.FilterDevices(drivePath: PChar): boolean;
var
  bufChar: TCharArray;
begin
  Result := false;
  //we check if this drive is removable
  if GetDriveType(drivePath) = DRIVE_REMOVABLE
  then begin

  {procedure TDriveEjector.CheckForCardReaders;
var
  i: integer;
begin
  if DrivesCount = 0 then exit;

  for i := 0 to DrivesCount - 1 do
  begin
    if GetNoDevicesWithSameParentInst(RemovableDrives[i].ParentDevInst) > 1 then
      if GetNoDevicesWithSameProductID(RemovableDrives[i].ProductId) > 1 then //Hard drive partitions
        RemovableDrives[i].IsCardReader:=False
      else
        RemovableDrives[i].IsCardReader:=True //Matching devices with parent inst but differing device names are likely to be card readers
  end;
end;
}
    QueryDosDevice(PChar(Copy(drivePath,1,2)),@bufChar[0],MAXCHAR);
    {checking if the device is a floppy drive}
    if (Copy(bufChar,1,14) <> DEV_FLOPPY)
    then begin
      {TODO: check if this device is a card reader}
      Result := true;
    end;
  end;
end;

//This constructor calls the parent one
//and passes as parameter removable drive type
constructor TUSBManager.Create;
begin
  inherited Create(DRIVE_REMOVABLE);
end;

//Overriden destructor
destructor TUSBManager.Destroy;
begin
  inherited Destroy;
end;

procedure TUSBManager.RemoveDrive(index: integer);
var
  Device: TDevice;
begin
  if (fDevices.Count = 0)
  then begin
  end {no device}
  else begin
    if (index<0) or (index>fDevices.Count-1)
    then begin
      raise EDeviceException.Create('Index was out of range!');
    end {fail to get device pointer}
    else begin
      Device := TDevice(fDevices.Items[index]);
      self.RemoveDrive(Device);
    end; {called overload}
  end; {no device}
end;

{
  It works absolutely similar to previous WinAPI realization.
}
procedure TUSBManager.RemoveDrive(device: TDevice);
var
  i: integer;
  DeviceInfo: SP_DEVINFO_DATA;
  DrivesPnpHandle: HDEVINFO;
  Parent: DWORD;
  VetoName: array[0..MAX_PATH] of char;
begin
  DeviceInfo.cbSize := sizeof(SP_DEVINFO_DATA);
  DrivesPnpHandle := SetupDiGetClassDevsA(@GUID_DEVCLASS_DISKDRIVE, nil,
    HWND(nil), DIGCF_PRESENT);
  try
    if DrivesPnpHandle = INVALID_HANDLE_VALUE
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end //invalid handle
    else begin
      if not SetupDiEnumDeviceInfo(DrivesPnpHandle,1,DeviceInfo)
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError));
      end //end error SetupDiEnum...
      else begin
        if CM_Get_Parent(Parent, DeviceInfo.DevInst, 0) = CR_SUCCESS
        then begin
          for i := 0 to MAX_ATTEMPTS-3 do
          begin
            CM_Request_Device_EjectA(Parent,nil,@VetoName,MAX_PATH,0);
            if VetoName = ''
            then begin
              {ÂÎÎÎÎÒ! ÂÎÒ ÝÒÎ ÒÐÓÚÚÚÚ!!!!}
              {TODO: Refactor this code and add BlockedFiles search
                    Check if there's any possibility to switch on
                    disconnected device}

              //SHChangeNotify(SHCNE_MEDIAREMOVED,SHCNF_PATH,device.Path,nil);
            end
            else begin
            end;
          end; //end loop
        end; //end CM_GET_PARENT
      end; //end SetupDiEnum...
    end; //valid handle
  finally
    SetupDiDestroyDeviceInfoList(DrivesPnpHandle);
  end;
end;

{
  WARINING: OLD VERSION. DOES NOT INFORM WINDOWS PROPERLY

  !!!uses ShlObj, ShellAPI

//Main method for drive removal
procedure TUSBManager.RemoveDrive(device: TDevice);
var
  Success: LongBool;
  i: integer;
  ReturnedBytes: DWORD;
  PreventFlag: PREVENT_MEDIA_REMOVAL;
  fHandle: THandle;
begin
  for i := 0 to MAX_ATTEMPTS-1 do
  begin
    //here we try to lock the volume
    Success := DeviceIoControl(fHandle, FSCTL_LOCK_VOLUME, nil, 0, nil, 0,
      ReturnedBytes, nil);
    if not Success
    then begin
      if i = MAX_ATTEMPTS-1
      then begin
        raise EDeviceException.Create(SysErrorMessage(GetLastError)); //access
      end //no more attempts left                                    //denied!
      else begin
        Sleep(2000);
        continue;
      end; //skip the iteration
    end //not successful
    else begin
      //clear the memory
      ZeroMemory(@PreventFlag, sizeof(PreventFlag));
      //we dismount the volume...
      DeviceIoControl(fHandle,FSCTL_DISMOUNT_VOLUME, nil, 0, nil, 0,
        ReturnedBytes, nil);
      //then, we enable volume ejection mechanism...
      DeviceIoControl(fHandle,IOCTL_STORAGE_MEDIA_REMOVAL,@PreventFlag,
        sizeof(PreventFlag),nil,0,ReturnedBytes,nil);
      //after, we eject the volume...
      DeviceIoControl(fHandle,IOCTL_STORAGE_EJECT_MEDIA, nil, 0, nil, 0,
        ReturnedBytes,nil);
      //...and finally we release the device
      DeviceIoControl(fHandle,FSCTL_UNLOCK_VOLUME,nil,0,nil,0,
        ReturnedBytes,nil);
      //...notify system about device removal..
      SHChangeNotify(SHCNE_MEDIAREMOVED,SHCNF_PATH,device.Path,nil);
      //and release all resources
      device.Destroy;
      fDevices.Pack;
      break;
    end; //successful
  end;
end;
}

//This method makes force drive removal
procedure TUSBManager.ForcedRemoveDrive;
begin
end;

{Find out if it is possible to return disconnected device}

class function TUSBManager.GetManager: TUSBManager;
begin
  if not Assigned(Instance)
  then begin
    Instance := TUSBManager.Create;
  end;
  Result := Instance;
end;

end.
