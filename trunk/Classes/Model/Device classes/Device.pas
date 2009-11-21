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
    fParent: TDevice; //parent device of the object. If it is not removable - is null
  protected
    function GetChild(index: integer): TDevice;
    function GetCount: integer;
    function GetInstanceHandle: THandle;
    class function GetDeviceInformation(DeviceNumber: Cardinal; DeviceInfoSet: THandle): TDeviceInfoData;
    class function GetDeviceInformationSet(ClassGUID: TGUID): THandle;
    class function GetDeviceNumber(FileHandle: THandle): Cardinal;
    class function GetDeviceProperty(PropertyCode: integer; DeviceInfoSet: THandle): Pointer;
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

function TDevice.GetInstanceHandle: THandle;
begin
  Result := fDeviceInfoData.DevInst;
end;

//This function gets device number using the DeviceIoControl function
class function TDevice.GetDeviceNumber(FileHandle: THandle): Cardinal;
var
  deviceNumber: TStorageDeviceNumber; //structure that contains device number
  dummy: Cardinal; //returned bytes
begin
  Result := INVALID_HANDLE_VALUE;
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
class function TDevice.GetDeviceProperty(PropertyCode: integer;
  DeviceInfoSet: THandle): Pointer;
begin
  Result := nil;
  SetupDiGetDeviceRegistryProperty(DeviceInfoSet,f)
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
var
  buffer: TCharArray; //string buffer
begin
  if not SetupDiEnumDeviceInfo(DeviceInfoSet,DeviceNumber,Result)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end; //then
end; //GetDeviceInformation


{CONSTRUCTOR}
constructor TDevice.Create(ClassGUID: TGUID; Path: string);
var
  deviceInfoSet: THandle;
  deviceInfoData: TDeviceInfoData;
begin
  inherited Create;
  deviceInfoSet := TDevice.GetDeviceInformationSet(ClassGUID);
  fClassGUID := ClassGUID;
  //SetupDiGetDeviceRegistryProperty(deviceInfoSet,deviceInfoData,SPDRP_CLASS
end;

{DESTRUCTOR}
destructor TDevice.Destroy;
var
  i: integer;
begin
  for i := 0 to fChildren.Count-1 do
  begin
    TDevice(fChildren.Items[i]^).Destroy;
  end;
  fParent.Destroy;
  inherited Destroy;
end;

end.
