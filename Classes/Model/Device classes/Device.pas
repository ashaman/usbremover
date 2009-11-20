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
    function GetChild(index: integer): TDevice;
    function GetInstanceHandle: THandle;
    class function GetDeviceInformation(InstanceHandle: THandle; DeviceInfoSet: THandle): TDeviceInfoData;
    class function GetDeviceInformationSet(ClassGUID: TGUID): THandle;
    class function GetDeviceProperty(PropertyCode: integer; DeviceInfoSet: THandle): Pointer;
  public
    constructor Create(ClassGUID: TGUID);
    destructor Destroy; override;
    property BusType: TBusType read fBusType;
    property Capabilities: TDeviceCapabilities read fCapabilities;
    property Children [index: integer]: TDevice read GetChild;
    property ClassGUID: TGUID read fClassGUID;
    property Description: string read fDescription;
    property DeviceClassName: string read fDeviceClassName;
    property FriendlyName: string read fFriendlyName;
    property InstanceHandle: THandle read GetInstanceHandle;
    property Parent: TDevice read fParent;
  end;

implementation

uses
  Windows, DeviceException, SysUtils;

function TDevice.GetChild(index: integer): TDevice;
begin
  Result := TDevice(fChildren.Items[index]^);
end;

function TDevice.GetInstanceHandle: THandle;
begin
  Result := fDeviceInfoData.DevInst;
end;

class function TDevice.GetDeviceProperty(PropertyCode: integer; DeviceInfoSet: THandle): Pointer;
begin
end;


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
end;


//In this function we get the specified device information by its instance handle
//We get all the devices of the specified class and get one of them
class function TDevice.GetDeviceInformation(InstanceHandle: THandle; DeviceInfoSet: THandle):
  TDeviceInfoData;
var
  buffer: TCharArray; //string buffer
begin
  //get the specified device ID
  if (CM_Get_Device_IDA(InstanceHandle,@buffer,sizeof(buffer),0) <> 0)
  then begin
    raise EDeviceException.Create(SysErrorMessage(GetLastError));
  end //then
  else begin
    //open the specified device information
    if not SetupDiOpenDeviceInfoA(DeviceInfoSet, PChar(@buffer), 0, 0, @Result)
    then begin
      raise EDeviceException.Create(SysErrorMessage(GetLastError));
    end; //then
  end; //else
end; //GetDeviceInformation

constructor TDevice.Create(ClassGUID: TGUID);
var
  deviceInfoSet: THandle;
  deviceInfoData: TDeviceInfoData;
begin
  deviceInfoSet := TDevice.GetDeviceInformationSet(ClassGUID);
  fClassGUID := ClassGUID;
  //SetupDiGetDeviceRegistryProperty(deviceInfoSet,deviceInfoData,SPDRP_CLASS
end;

destructor TDevice.Destroy;
begin
end;

end.
