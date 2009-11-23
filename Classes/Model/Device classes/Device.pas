{
  Device class.
  Developed by J.L. Blackrow.
}

unit Device;

interface

uses
  WinIOCtl, Classes, WMI;

type
  //Aliases for data types
  TBusType = STORAGE_BUS_TYPE;
  TDeviceInfoData = SP_DEVINFO_DATA;

  //Device class
  TDevice = class(TObject)
  private
    fBusType: TBusType; //device bus type
    fCapabilities: TDeviceCapabilities; //device capabilities
    fChildren: TList; //child devices
    fClassGUID: TGUID; //class GUID
    fDescription: string; //device description string
    fDeviceClassName: string; //class name
    fDeviceInfoData: TDeviceInfoData; //device info data
    fFriendlyName: string; //device full name
    fManufacturer: string; //device manufacturer
    fParent: TDevice; //parent device of the object. If it is not removable - is null
  protected
    function GetChild(index: integer): TDevice;
    function GetCount: integer;
    function GetInstanceHandle: THandle;
    class function FormatDevicePath(const Path: string): string;
    class function GetDeviceInformation(DeviceNumber: Cardinal; DeviceInfoSet: THandle): TDeviceInfoData;
    class function GetDeviceInformationSet(ClassGUID: TGUID): THandle;
    class function GetDeviceNumber(FileHandle: THandle): Cardinal;
    class function GetDeviceProperty(DeviceNumber: Cardinal; DeviceInformation: TDeviceInfoData;
      PropertyCode: integer; DeviceInfoSet: THandle): Pointer;
  public
    constructor Create(ClassGUID: TGUID; Path: string);
    destructor Destroy; override;
    property BusType: TBusType read fBusType;
    property Capabilities: TDeviceCapabilities read fCapabilities;
    property Children [index: integer]: TDevice read GetChild;
    property ClassGUID: TGUID read fClassGUID;
    property Count: integer read GetCount;
    property Description: string read fDescription;
    property DeviceClassName: string read fDeviceClassName;
    property FriendlyName: string read fFriendlyName;
    property InstanceHandle: THandle read GetInstanceHandle;
    property Manufacturer: string read fManufacturer;
    property Parent: TDevice read fParent;
  end;

implementation

uses
  Windows, DeviceException, SysUtils;

//This function returns the total count of child devices
function TDevice.GetCount: integer;
begin
  Result := fChildren.Count;
end; //GetCount

//This function gets the child of this device specified by index
function TDevice.GetChild(index: integer): TDevice;
begin
  Result := TDevice(fChildren.Items[index]^);
end; //GetChild

//This function gets device instance handle
function TDevice.GetInstanceHandle: THandle;
begin
  Result := fDeviceInfoData.DevInst;
end;

{CLASS FUNCTIONS}

//Formats the device path for opening it as file
class function TDevice.FormatDevicePath(const Path: string): string;
const
  dot = '.';
  question = '?'; //question mark
  slash = '\'; //backslash
var
  lastSlash: integer; //last backslash position
begin
  Result := Path;
  //we get last backslash index
  lastSlash := LastDelimiter(slash, Path);
  //if it is in the end, we delete it
  if lastSlash = Length(Path)
  then begin
    Delete(Result, lastSlash, 1);
  end;
  //we change question mark to dot
  if Result[3] = question
  then begin
    Result[3] := dot;
  end;
end; //FormatDevicePath


//This function gets device number using the DeviceIoControl function
class function TDevice.GetDeviceNumber(FileHandle: THandle): Cardinal;
var
  deviceNumber: TStorageDeviceNumber; //structure that contains device number
  dummy: Cardinal; //returned bytes
begin
  //getting device number through DeviceIoContrtol and TStorageDeviceNumber
  if not DeviceIoControl(FileHandle,IOCTL_STORAGE_GET_DEVICE_NUMBER, nil, 0,
    @deviceNumber, sizeof(deviceNumber), dummy, nil)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then
  else begin
    Result := deviceNumber.DeviceNumber;
  end; //else - success
end; //GetDeviceNumber


//This function returns the property specified by its control code
class function TDevice.GetDeviceProperty(DeviceNumber: Cardinal;
  DeviceInformation: TDeviceInfoData; PropertyCode: integer;
  DeviceInfoSet: THandle): Pointer;
const
  propertyBufferSize = 1024;
var
  dummy: Cardinal; //variable for the function call
  size: Cardinal;
  buffer: TCharArray;
begin
  Result := nil;
  //getting device property
  if not SetupDiGetDeviceRegistryProperty(DeviceInfoSet, DeviceInformation, PropertyCode,
    dummy, @buffer, sizeof(buffer), size)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end
  else begin
    Result := @buffer;
  end;
end; //GetDeviceProperty

//This function retutns the handle to the device information set for the
//specified device class
class function TDevice.GetDeviceInformationSet(ClassGUID: TGUID): THandle;
begin
  //get all devices of the specified class
  Result := SetupDiGetClassDevsA(@ClassGUID, nil, 0, DIGCF_PRESENT or
    DIGCF_DEVICEINTERFACE);
  if Result = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end;
end; //GetDeviceInformationSet

//In this function we get the specified device information by its index in the
//device information set
class function TDevice.GetDeviceInformation(DeviceNumber: Cardinal;
  DeviceInfoSet: THandle): TDeviceInfoData;
begin
  Result.cbSize := sizeof(Result);
  if not SetupDiEnumDeviceInfo(DeviceInfoSet,DeviceNumber,Result)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end; //then
end; //GetDeviceInformation


{CONSTRUCTOR}
constructor TDevice.Create(ClassGUID: TGUID; Path: string);
var
  deviceInfoSet: THandle; //device information set handle
  deviceInfoData: TDeviceInfoData; //device information data
  deviceNumber: Cardinal; //device index in the device informatiom set
  fileHandle: THandle; //device file handle
begin
  inherited Create;
  //we open device as file
  fileHandle := CreateFile(PChar(TDevice.FormatDevicePath(Path)), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  //opening failed
  if fileHandle = INVALID_HANDLE_VALUE
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then - failed
  else begin
    try
      //Here we get device information
      deviceInfoSet := TDevice.GetDeviceInformationSet(ClassGUID);
      deviceNumber := TDevice.GetDeviceNumber(fileHandle);
      deviceInfoData := TDevice.GetDeviceInformation(deviceNumber, deviceInfoSet);
      //Getting device properties
      {fBusType := TBusType(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_BUSNUMBER, deviceInfoSet)^);}
      fCapabilities := TDeviceCapabilities(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_CAPABILITIES, deviceInfoSet)^);
      fClassGUID := ClassGUID;
      fDescription := String(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_DEVICEDESC, deviceInfoSet)^);
      fDeviceClassName := String(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_CLASS, deviceInfoSet)^);
      fDeviceInfoData := deviceInfoData;
      fFriendlyName := String(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_FRIENDLYNAME, deviceInfoSet)^);
      fManufacturer := String(TDevice.GetDeviceProperty(deviceNumber,
        deviceInfoData, SPDRP_MFG, deviceInfoSet)^);
    finally
      CloseHandle(fileHandle);
    end;
  end; //else - success
  //SetupDiGetDeviceRegistryProperty(deviceInfoSet,deviceInfoData,SPDRP_CLASS
end; //Create

{DESTRUCTOR}
destructor TDevice.Destroy;
var
  i: integer;
begin
  if Assigned(fChildren)
  then begin
    for i := 0 to fChildren.Count-1 do
    begin
      TDevice(fChildren.Items[i]^).Destroy;
    end;
  end;
  if Assigned(fParent)
  then begin
    fParent.Destroy;
  end;
  inherited Destroy;
end;

end.
